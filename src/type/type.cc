#include "type/all.h"

#include "architecture.h"
#include "base/container/map.h"
#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"

namespace type {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  Type *GlobalName = new Primitive(PrimType::EnumName);
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

using InitFnType = void (*)(const Type *, const Type *, IR::Val, IR::Val,
                            Context *ctx);

template <InitFnType InitFn>
static IR::Func *ArrayInitializationWith(const Array *from_type,
                                         const Array *to_type, Context *ctx) {
  static base::guarded<base::unordered_map<
      const Array *, base::unordered_map<const Array *, IR::Func *>>>
      init_fns;

  auto handle         = init_fns.lock();
  auto[iter, success] = (*handle)[to_type].emplace(from_type, nullptr);
  if (success) {
    base::vector<std::pair<std::string, AST::Expression *>> args = {
        {"arg0", nullptr}, {"arg1", nullptr}};
    auto *fn = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(to_type)}, {}), std::move(args));
    iter->second = fn;

    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      auto from_arg           = fn->Argument(0);
      auto to_arg             = fn->Argument(1);
      auto phi_block          = IR::Func::Current->AddBlock();
      auto body_block         = IR::Func::Current->AddBlock();
      auto exit_block         = IR::Func::Current->AddBlock();

      auto from_len = [&]() -> IR::RegisterOr<i32> {
        if (from_type->fixed_length) {
          return static_cast<i32>(from_type->len);
        }
        return IR::LoadInt(IR::ArrayLength(from_arg));
      }();

      if (!to_type->fixed_length) {
        IR::StoreInt(from_len, IR::ArrayLength(to_arg));
        // TODO Architecture dependence?
        IR::StoreAddr(
            IR::Malloc(from_type->data_type,
                       Architecture::InterprettingMachine().ComputeArrayLength(
                           from_len, from_type->data_type)),
            IR::ArrayData(to_arg, type::Ptr(to_type)));
      }

      auto from_start = IR::Index(from_type, from_arg, 0);
      auto to_start   = IR::Index(type::Ptr(to_type), to_arg, 0);
      auto from_end =
          IR::PtrIncr(from_start, from_len, type::Ptr(from_type->data_type));
      IR::UncondJump(phi_block);

      IR::BasicBlock::Current = phi_block;
      auto from_phi_index     = IR::Phi(type::Ptr(from_type->data_type));
      auto to_phi_index       = IR::Phi(type::Ptr(from_type->data_type));
      auto from_phi_reg = IR::Func::Current->Command(from_phi_index).result;
      type::Type const *from_phi_reg_type = type::Ptr(from_type->data_type);
      auto to_phi_reg = IR::Func::Current->Command(to_phi_index).result;
      type::Type const *to_phi_reg_type = type::Ptr(to_type->data_type);

      IR::CondJump(IR::NeAddr(from_phi_reg, from_end), body_block, exit_block);

      IR::BasicBlock::Current = body_block;
      InitFn(from_type->data_type, to_type->data_type,
             PtrCallFix(IR::Val::Reg(from_phi_reg, from_phi_reg_type)),
             IR::Val::Reg(to_phi_reg, to_phi_reg_type), ctx);
      auto from_incr = IR::PtrIncr(from_phi_reg, 1, from_phi_reg_type);
      auto to_incr   = IR::PtrIncr(to_phi_reg, 1,
                                 to_phi_reg_type);
      IR::UncondJump(phi_block);

      IR::MakePhi<IR::Addr>(
          from_phi_index, {{fn->entry(), from_start}, {body_block, from_incr}});
      IR::MakePhi<IR::Addr>(to_phi_index,
                            {{fn->entry(), to_start}, {body_block, to_incr}});

      IR::BasicBlock::Current = exit_block;
      IR::ReturnJump();
    }
  }
  return iter->second;
}

template <InitFnType InitFn>
static IR::Func *StructInitializationWith(const Struct *struct_type,
                                          Context *ctx) {
  static base::guarded<base::unordered_map<const Struct *, IR::Func *>>
      struct_init_fns;
  auto handle         = struct_init_fns.lock();
  auto[iter, success] = handle->emplace(struct_type, nullptr);

  if (success) {
    base::vector<std::pair<std::string, AST::Expression *>> args = {
        {"arg0", nullptr}, {"arg1", nullptr}};
    auto *fn = iter->second = ctx->mod_->AddFunc(
        Func({Ptr(struct_type), Ptr(struct_type)}, {}), std::move(args));

    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      for (size_t i = 0; i < struct_type->fields_.size(); ++i) {
        InitFn(struct_type->fields_[i].type, struct_type->fields_[i].type,
               PtrCallFix(
                   IR::Val::Reg(IR::Field(fn->Argument(0), struct_type, i),
                                type::Ptr(struct_type->fields_.at(i).type))),
               IR::Val::Reg(IR::Field(fn->Argument(1), struct_type, i),
                            type::Ptr(struct_type->fields_.at(i).type)),
               ctx);
      }
      IR::ReturnJump();
    }
  }
  return iter->second;
}

void EmitCopyInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Val to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>() || to_type->is<Function>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Array>()) {
    IR::LongArgs call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    IR::Func *f = ArrayInitializationWith<EmitCopyInit>(
        &from_type->as<Array>(), &to_type->as<Array>(), ctx);
    call_args.type_ = f->type_;
    IR::Call(IR::AnyFunc{f}, std::move(call_args));

  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);

    IR::LongArgs call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    IR::Func *f =
        StructInitializationWith<EmitCopyInit>(&to_type->as<Struct>(), ctx);
    call_args.type_ = f->type_;
    IR::Call(IR::AnyFunc{f}, std::move(call_args));

  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Scope>()) {
    NOT_YET();
  } else {
    UNREACHABLE(to_type->to_string(), from_type->to_string());
  }
}

void EmitMoveInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Val to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);

  } else if (to_type->is<Array>()) {
    auto *to_array_type   = &to_type->as<Array>();
    auto *from_array_type = &from_type->as<Array>();

    if (to_array_type->fixed_length || from_array_type->fixed_length) {
      IR::LongArgs call_args;
      call_args.append(from_val);
      call_args.append(to_var);
      IR::Func *f = ArrayInitializationWith<EmitMoveInit>(
          &from_type->as<Array>(), &to_type->as<Array>(), ctx);
      call_args.type_ = f->type_;
      IR::Call(IR::AnyFunc{f}, std::move(call_args));
    } else {
      IR::StoreInt(
          IR::LoadInt(IR::ArrayLength(std::get<IR::Register>(from_val.value))),
          IR::ArrayLength(std::get<IR::Register>(to_var.value)));

      IR::StoreInt(
          IR::LoadInt(IR::ArrayData(std::get<IR::Register>(from_val.value),
                                    from_val.type)),
          IR::ArrayData(std::get<IR::Register>(to_var.value), to_var.type));
      // TODO if this move is to be destructive, this assignment to array
      // length is not necessary.
      IR::StoreInt(0, IR::ArrayLength(std::get<IR::Register>(from_val.value)));
      IR::StoreAddr(
          IR::Malloc(from_array_type->data_type, 0),
          IR::ArrayData(std::get<IR::Register>(from_val.value), from_val.type));
    }
  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);

    IR::LongArgs call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    IR::Func *f =
        StructInitializationWith<EmitCopyInit>(&to_type->as<Struct>(), ctx);
    call_args.type_ = f->type_;
    IR::Call(IR::AnyFunc{f}, std::move(call_args));
  } else if (to_type->is<Function>()) {
    NOT_YET();
  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Scope>()) {
    NOT_YET();
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

static base::guarded<base::map<base::vector<const Type *>, const Scope>> scopes_;
const Scope *Scp(const base::vector<const Type *> &types) {
  return &scopes_.lock()->emplace(types, Scope(types)).first->second;
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
}  // namespace type
