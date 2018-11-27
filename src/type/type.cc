#include "type/all.h"

#include "architecture.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/container/map.h"
#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"

namespace type {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  Type const *EnumName = new Primitive(PrimType::EnumName);
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

using InitFnType = void (*)(Type const *, Type const *, ir::Val, ir::Register,
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

void EmitCopyInit(Type const *from_type, Type const *to_type, ir::Val from_val,
                  ir::Register to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>() || to_type->is<Function>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Array>()) {
    ir::Arguments call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    ir::Func *f = ArrayInitializationWith<EmitCopyInit>(
        &from_type->as<Array>(), &to_type->as<Array>(), ctx);
    call_args.type_ = f->type_;
    ir::Call(ir::AnyFunc{f}, std::move(call_args));

  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);

    ir::Arguments call_args;
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

void EmitMoveInit(Type const *from_type, Type const *to_type, ir::Val from_val,
                  ir::Register to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);

  } else if (to_type->is<Array>()) {
    auto *to_array_type   = &to_type->as<Array>();
    auto *from_array_type = &from_type->as<Array>();

    if (to_array_type->fixed_length || from_array_type->fixed_length) {
      ir::Arguments call_args;
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

    ir::Arguments call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    ir::Func *f =
        StructInitializationWith<EmitCopyInit>(&to_type->as<Struct>(), ctx);
    call_args.type_ = f->type_;
    ir::Call(ir::AnyFunc{f}, std::move(call_args));
  } else if (to_type->is<Function>()) {
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  }
}

static base::guarded<
    base::unordered_map<Type const *, base::unordered_map<size_t, Array>>>
    fixed_arrays_;
const Array *Arr(Type const *t, size_t len) {
  auto handle = fixed_arrays_.lock();
  return &(*handle)[t]
              .emplace(std::piecewise_construct, std::forward_as_tuple(len),
                       std::forward_as_tuple(t, len))
              .first->second;
}
static base::guarded<base::unordered_map<Type const *, Array>> arrays_;
const Array *Arr(Type const *t) {
  return &arrays_.lock()
              ->emplace(std::piecewise_construct, std::forward_as_tuple(t),
                        std::forward_as_tuple(t))
              .first->second;
}

static base::guarded<base::map<base::vector<Type const *>, Variant>> variants_;
Type const *Var(base::vector<Type const *> variants) {
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

static base::guarded<base::unordered_map<Type const *, const Pointer>>
    pointers_;
const Pointer *Ptr(Type const *t) {
  return &pointers_.lock()->emplace(t, Pointer(t)).first->second;
}

static base::guarded<base::map<base::vector<Type const *>,
                               base::map<base::vector<Type const *>, Function>>>
    funcs_;
const Function *Func(base::vector<Type const *> in,
                     base::vector<Type const *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

static base::guarded<base::map<base::vector<Type const *>, const Tuple>> tups_;
Type const *Tup(base::vector<Type const *> entries) {
  if (entries.size() == 1) { return entries[0]; }
  Tuple tup(entries);
  auto[iter, success] =
      tups_.lock()->emplace(std::move(entries), std::move(tup));
  return &iter->second;
}

Type const *Void() { return Tup({}); }

bool Type::is_big() const {
  return is<Array>() || is<Struct>() || is<Variant>() || is<Tuple>();
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
