#include "type/all.h"

#include "architecture.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/container/map.h"
#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"

namespace type {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  Type const *EnumName = new Primitive(PrimType::EnumName);
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

using InitFnType = void (*)(const Type *, const Type *, ir::Val, ir::Register,
                            Context *ctx);

template <InitFnType InitFn>
static ir::Func *ArrayInitializationWith(const Array *from_type,
                                         const Array *to_type, Context *ctx) {
  static base::guarded<base::unordered_map<
      const Array *, base::unordered_map<const Array *, ir::Func *>>>
      init_fns;

  auto handle         = init_fns.lock();
  auto[iter, success] = (*handle)[to_type].emplace(from_type, nullptr);
  if (success) {
    base::vector<std::pair<std::string, ast::Expression *>> args = {
        {"arg0", nullptr}, {"arg1", nullptr}};
    auto *fn = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(to_type)}, {}), std::move(args));
    iter->second = fn;

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      auto from_arg           = fn->Argument(0);
      auto to_arg             = fn->Argument(1);
      auto phi_block          = ir::Func::Current->AddBlock();
      auto body_block         = ir::Func::Current->AddBlock();
      auto exit_block         = ir::Func::Current->AddBlock();

      auto from_len = [&]() -> ir::RegisterOr<i32> {
        if (from_type->fixed_length) {
          return static_cast<i32>(from_type->len);
        }
        return ir::Load<i32>(ir::ArrayLength(from_arg));
      }();

      if (!to_type->fixed_length) {
        ir::Store(from_len, ir::ArrayLength(to_arg));
        // TODO Architecture dependence?
        ir::Store(
            ir::Malloc(from_type->data_type,
                       Architecture::InterprettingMachine().ComputeArrayLength(
                           from_len, from_type->data_type)),
            ir::ArrayData(to_arg, type::Ptr(to_type)));
      }

      auto from_start = ir::Index(from_type, from_arg, 0);
      auto to_start   = ir::Index(type::Ptr(to_type), to_arg, 0);
      auto from_end =
          ir::PtrIncr(from_start, from_len, type::Ptr(from_type->data_type));
      ir::UncondJump(phi_block);

      ir::BasicBlock::Current = phi_block;
      auto from_phi_index     = ir::Phi(type::Ptr(from_type->data_type));
      auto to_phi_index       = ir::Phi(type::Ptr(from_type->data_type));
      auto from_phi_reg = ir::Func::Current->Command(from_phi_index).result;
      type::Type const *from_phi_reg_type = type::Ptr(from_type->data_type);
      auto to_phi_reg = ir::Func::Current->Command(to_phi_index).result;
      type::Type const *to_phi_reg_type = type::Ptr(to_type->data_type);

      ir::CondJump(ir::Ne(from_end, from_phi_reg), body_block, exit_block);

      ir::BasicBlock::Current = body_block;
      InitFn(from_type->data_type, to_type->data_type,
             ir::Val::Reg(ir::PtrFix(from_phi_reg, from_type->data_type),
                          from_type->data_type),
             to_phi_reg, ctx);
      auto from_incr = ir::PtrIncr(from_phi_reg, 1, from_phi_reg_type);
      auto to_incr   = ir::PtrIncr(to_phi_reg, 1, to_phi_reg_type);
      ir::UncondJump(phi_block);

      ir::MakePhi<ir::Addr>(
          from_phi_index, {{fn->entry(), from_start}, {body_block, from_incr}});
      ir::MakePhi<ir::Addr>(to_phi_index,
                            {{fn->entry(), to_start}, {body_block, to_incr}});

      ir::BasicBlock::Current = exit_block;
      ir::ReturnJump();
    }
  }
  return iter->second;
}

template <InitFnType InitFn>
static ir::Func *StructInitializationWith(const Struct *struct_type,
                                          Context *ctx) {
  static base::guarded<base::unordered_map<const Struct *, ir::Func *>>
      struct_init_fns;
  auto handle         = struct_init_fns.lock();
  auto[iter, success] = handle->emplace(struct_type, nullptr);

  if (success) {
    base::vector<std::pair<std::string, ast::Expression *>> args = {
        {"arg0", nullptr}, {"arg1", nullptr}};
    auto *fn = iter->second = ctx->mod_->AddFunc(
        Func({Ptr(struct_type), Ptr(struct_type)}, {}), std::move(args));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      auto const &fields      = struct_type->fields();
      for (size_t i = 0; i < fields.size(); ++i) {
        InitFn(
            fields.at(i).type, fields.at(i).type,
            ir::Val::Reg(ir::PtrFix(ir::Field(fn->Argument(0), struct_type, i),
                                    fields.at(i).type),
                         fields.at(i).type),
            ir::Field(fn->Argument(1), struct_type, i), ctx);
      }
      ir::ReturnJump();
    }
  }
  return iter->second;
}

void EmitCopyInit(const Type *from_type, const Type *to_type, ir::Val from_val,
                  ir::Register to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>() || to_type->is<Function>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Array>()) {
    ir::LongArgs call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    ir::Func *f = ArrayInitializationWith<EmitCopyInit>(
        &from_type->as<Array>(), &to_type->as<Array>(), ctx);
    call_args.type_ = f->type_;
    ir::Call(ir::AnyFunc{f}, std::move(call_args));

  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);

    ir::LongArgs call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    ir::Func *f =
        StructInitializationWith<EmitCopyInit>(&to_type->as<Struct>(), ctx);
    call_args.type_ = f->type_;
    ir::Call(ir::AnyFunc{f}, std::move(call_args));

  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else {
    UNREACHABLE(to_type->to_string(), from_type->to_string());
  }
}

void EmitMoveInit(const Type *from_type, const Type *to_type, ir::Val from_val,
                  ir::Register to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);

  } else if (to_type->is<Array>()) {
    auto *to_array_type   = &to_type->as<Array>();
    auto *from_array_type = &from_type->as<Array>();

    if (to_array_type->fixed_length || from_array_type->fixed_length) {
      ir::LongArgs call_args;
      call_args.append(from_val);
      call_args.append(to_var);
      ir::Func *f = ArrayInitializationWith<EmitMoveInit>(
          &from_type->as<Array>(), &to_type->as<Array>(), ctx);
      call_args.type_ = f->type_;
      ir::Call(ir::AnyFunc{f}, std::move(call_args));
    } else {
      ir::Store(ir::Load<i32>(
                    ir::ArrayLength(std::get<ir::Register>(from_val.value))),
                ir::ArrayLength(to_var));

      ir::Store(ir::Load<i32>(ir::ArrayData(
                    std::get<ir::Register>(from_val.value), from_val.type)),
                ir::ArrayData(to_var, type::Ptr(to_type)));
      // TODO if this move is to be destructive, this assignment to array
      // length is not necessary.
      ir::Store(0, ir::ArrayLength(std::get<ir::Register>(from_val.value)));
      ir::Store(
          ir::Malloc(from_array_type->data_type, 0),
          ir::ArrayData(std::get<ir::Register>(from_val.value), from_val.type));
    }
  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);

    ir::LongArgs call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    ir::Func *f =
        StructInitializationWith<EmitCopyInit>(&to_type->as<Struct>(), ctx);
    call_args.type_ = f->type_;
    ir::Call(ir::AnyFunc{f}, std::move(call_args));
  } else if (to_type->is<Function>()) {
    NOT_YET();
  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  }
}

// TODO optimize (early exists. don't check lhs->is<> && rhs->is<>. If they
// don't match you can early exit.
const Type *Meet(const Type *lhs, const Type *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Err) { return rhs; }  // Ignore errors
  if (rhs == Err) { return lhs; }  // Ignore errors
  if (lhs == NullPtr || rhs == NullPtr) {
    // TODO It's not obvious to me that this is what I want to do.
    return nullptr;
  }
  if (lhs->is<Pointer>()) {
    return rhs->is<Pointer>() ? Ptr(Meet(lhs->as<Pointer>().pointee,
                                         rhs->as<Pointer>().pointee))
                              : nullptr;
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    const Type *result = nullptr;
    if (lhs->as<Array>().fixed_length && rhs->as<Array>().fixed_length) {
      if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
      result = Meet(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result, lhs->as<Array>().len) : result;
    } else {
      result = Meet(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result,
                          std::max(lhs->as<Array>().len, rhs->as<Array>().len))
                    : result;
    }
  } else if (lhs->is<Array>() && rhs == EmptyArray &&
             !lhs->as<Array>().fixed_length) {
    return Arr(lhs->as<Array>().data_type, 0);
  } else if (rhs->is<Array>() && lhs == EmptyArray &&
             !rhs->as<Array>().fixed_length) {
    return Arr(rhs->as<Array>().data_type, 0);
  } else if (lhs->is<Variant>()) {
    // TODO this feels very fishy, cf. ([3; int] | [4; int]) with [--; int]
    base::vector<const Type *> results;
    if (rhs->is<Variant>()) {
      for (const Type *l_type : lhs->as<Variant>().variants_) {
        for (const Type *r_type : rhs->as<Variant>().variants_) {
          const Type *result = Meet(l_type, r_type);
          if (result != nullptr) { results.push_back(result); }
        }
      }
    } else {
      for (const Type *t : lhs->as<Variant>().variants_) {
        if (const Type *result = Meet(t, rhs)) { results.push_back(result); }
      }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  } else if (rhs->is<Variant>()) {  // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give a
    // better interface.
    base::vector<const Type *> results;
    for (const Type *t : rhs->as<Variant>().variants_) {
      if (const Type *result = Meet(t, lhs)) { results.push_back(result); }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  }

  return nullptr;
}

const Type *Join(const Type *lhs, const Type *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Err) { return rhs; }  // Ignore errors
  if (rhs == Err) { return lhs; }  // Ignore errors
  if ((lhs == Block && rhs == OptBlock) || (lhs == OptBlock && rhs == Block)) {
    return Block;
  }
  if (lhs->is<Primitive>() && rhs->is<Primitive>()) {
    return lhs == rhs ? lhs : nullptr;
  }
  if (lhs == NullPtr && rhs->is<Pointer>()) { return rhs; }
  if (rhs == NullPtr && lhs->is<Pointer>()) { return lhs; }
  if (lhs->is<Pointer>() && rhs->is<Pointer>()) {
    return Join(lhs->as<Pointer>().pointee, rhs->as<Pointer>().pointee);
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    const Type *result = nullptr;
    if (lhs->as<Array>().fixed_length && rhs->as<Array>().fixed_length) {
      if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
      result = Join(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result, lhs->as<Array>().len) : result;
    } else {
      result = Join(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result) : result;
    }
  } else if (lhs->is<Array>() && rhs == EmptyArray &&
             !lhs->as<Array>().fixed_length) {
    return lhs;
  } else if (rhs->is<Array>() && lhs == EmptyArray &&
             !rhs->as<Array>().fixed_length) {
    return rhs;
  } else if (lhs->is<Variant>()) {
    base::vector<const Type *> rhs_types;
    if (rhs->is<Variant>()) {
      rhs_types = rhs->as<Variant>().variants_;
    } else {
      rhs_types = {rhs};
    }

    auto vars = lhs->as<Variant>().variants_;
    vars.insert(vars.end(), rhs_types.begin(), rhs_types.end());
    return Var(std::move(vars));
  } else if (rhs->is<Variant>()) {  // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give
    // a better interface.
    for (const Type *v : rhs->as<Variant>().variants_) {
      if (lhs == v) { return rhs; }
    }
    return nullptr;
  }
  UNREACHABLE(lhs, rhs);
}

static base::guarded<
    base::unordered_map<const Type *, base::unordered_map<size_t, Array>>>
    fixed_arrays_;
const Array *Arr(const Type *t, size_t len) {
  auto handle = fixed_arrays_.lock();
  return &(*handle)[t]
              .emplace(std::piecewise_construct, std::forward_as_tuple(len),
                       std::forward_as_tuple(t, len))
              .first->second;
}
static base::guarded<base::unordered_map<const Type *, Array>> arrays_;
const Array *Arr(const Type *t) {
  return &arrays_.lock()
              ->emplace(std::piecewise_construct, std::forward_as_tuple(t),
                        std::forward_as_tuple(t))
              .first->second;
}

static base::guarded<base::map<base::vector<const Type *>, Variant>> variants_;
const Type *Var(base::vector<const Type *> variants) {
  if (variants.empty()) { return type::Void(); }
  if (variants.size() == 1) { return variants[0]; }

  size_t end = variants.size();
  size_t i   = 0;
  while (i < end) {
    if (variants[i]->is<Variant>()) {
      const Variant *var = &variants[i]->as<Variant>();
      variants[i]        = variants.back();
      variants.pop_back();
      variants.insert(variants.end(), var->variants_.begin(),
                      var->variants_.end());
    } else {
      ++i;
    }
  }

  // TODO This sort order should be deterministic to allow interoperability
  // between multiple runs of the compiler.

  std::sort(variants.begin(), variants.end());
  variants.erase(std::unique(variants.begin(), variants.end()), variants.end());

  if (variants.size() == 1) { return variants.front(); }

  return &variants_.lock()
              ->emplace(std::piecewise_construct,
                        std::forward_as_tuple(variants),
                        std::forward_as_tuple(variants))
              .first->second;
}

static base::guarded<base::unordered_map<const Type *, const Pointer>>
    pointers_;
const Pointer *Ptr(const Type *t) {
  return &pointers_.lock()->emplace(t, Pointer(t)).first->second;
}

static base::guarded<base::map<base::vector<const Type *>,
                               base::map<base::vector<const Type *>, Function>>>
    funcs_;
const Function *Func(base::vector<const Type *> in,
                     base::vector<const Type *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

static base::guarded<base::map<base::vector<const Type *>, const Tuple>> tups_;
const Type *Tup(base::vector<const Type *> entries) {
  if (entries.size() == 1) { return entries[0]; }
  Tuple tup(entries);
  auto[iter, success] =
      tups_.lock()->emplace(std::move(entries), std::move(tup));
  return &iter->second;
}

const Type *Void() { return Tup({}); }

bool Type::is_big() const {
  return is<Array>() || is<Struct>() || is<Variant>() || is<Tuple>();
}

bool CanCastImplicitly(const type::Type *from, const type::Type *to) {
  return Join(from, to) == to;
}

static base::guarded<base::unordered_map<size_t, CharBuffer>> char_bufs_;
const CharBuffer *CharBuf(size_t len) {
  auto[iter, success] = char_bufs_.lock()->emplace(len, CharBuffer(len));
  return &iter->second;
}

Struct *Struct::Make(ast::StructLiteral *lit) {
  auto *s             = new Struct;
  s->to_be_completed_ = lit;
  return s;
}

void Struct::finalize() {
  auto *ptr = to_be_completed_.exchange(nullptr);
  if (ptr != nullptr) { ptr->Complete(this); }
}

base::vector<Struct::Field> const &Struct::fields() const {
  // TODO is doing this lazily a good idea?
  // TODO remove this const_cast
  const_cast<type::Struct *>(this)->finalize();
  return fields_;
}

void Struct::set_last_name(std::string_view s) {
  fields_.back().name = std::string(s);
  auto[iter, success] =
      field_indices_.emplace(fields_.back().name, fields_.size() - 1);
  ASSERT(success);
}
size_t Struct::index(std::string const &name) const {
  // TODO is doing this lazily a good idea?
  // TODO remove this const_cast
  const_cast<type::Struct *>(this)->finalize();
  return field_indices_.at(name);
}

size_t Struct::offset(size_t field_num, Architecture const &arch) const {
  size_t offset = 0;
  for (size_t i = 0; i < field_num; ++i) {
    offset += arch.bytes(fields_.at(i).type);
    offset = arch.MoveForwardToAlignment(fields_.at(i + 1).type, offset);
  }
  return offset;
}

Struct::Field const *Struct::field(std::string const &name) const {
  // TODO is doing this lazily a good idea?
  // TODO remove this const_cast
  const_cast<type::Struct *>(this)->finalize();

  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}

Type const *Generic = new GenericFunction;
}  // namespace type
