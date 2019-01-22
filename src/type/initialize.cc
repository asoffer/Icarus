#include "type/all.h"

#include "architecture.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

namespace ast {
struct Expression;
}  // namespace ast

namespace type {
void Array::EmitInit(ir::Register id_reg, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(Func({Ptr(this)}, {}),
                                    ast::FnParams<ast::Expression *>(1));

    CURRENT_FUNC(init_func_) {
      ir::BasicBlock::Current = init_func_->entry();

      auto ptr = ir::Index(type::Ptr(this), init_func_->Argument(0), 0);
      auto end_ptr =
          ir::PtrIncr(ptr, static_cast<i32>(len), type::Ptr(data_type));

      using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            data_type->EmitInit(std::get<0>(phis).reg_, ctx);
            return tup{
                ir::PtrIncr(std::get<0>(phis).reg_, 1, type::Ptr(data_type))};
          },
          std::tuple<type::Type const *>{type::Ptr(data_type)}, tup{ptr});

      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  ir::Call(ir::AnyFunc{init_func_}, std::move(call_args));
}

void Primitive::EmitInit(ir::Register id_reg, Context *ctx) const {
  switch (type_) {
    case PrimType::Type_: ir::Store(type::Void(), id_reg); break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: ir::Store(false, id_reg); break;
    case PrimType::Int8: ir::Store(static_cast<i8>(0), id_reg); break;
    case PrimType::Int16: ir::Store(static_cast<i16>(0), id_reg); break;
    case PrimType::Int32: ir::Store(static_cast<i32>(0), id_reg); break;
    case PrimType::Int64: ir::Store(static_cast<i64>(0), id_reg); break;
    case PrimType::Nat8: ir::Store(static_cast<u8>(0), id_reg); break;
    case PrimType::Nat16: ir::Store(static_cast<u16>(0), id_reg); break;
    case PrimType::Nat32: ir::Store(static_cast<u32>(0), id_reg); break;
    case PrimType::Nat64: ir::Store(static_cast<u64>(0), id_reg); break;
    case PrimType::Float32: ir::Store(0.0f, id_reg); break;
    case PrimType::Float64: ir::Store(0.0, id_reg); break;
    default: UNREACHABLE();
  }
}

void Enum::EmitInit(ir::Register id_reg, Context *ctx) const {
  UNREACHABLE("Enums must be initialized");
}

void Flags::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::FlagsVal{0}, id_reg);
}

void Variant::EmitInit(ir::Register, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Pointer::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::Addr::Null(), id_reg);
}

void Function::EmitInit(ir::Register id_reg, Context *ctx) const {
  UNREACHABLE();
}

using InitFnType = void (*)(Type const *, Type const *, ir::Val const &,
                            ir::Register, Context *ctx);

template <InitFnType InitFn>
static ir::Func *ArrayInitializationWith(const Array *from_type,
                                         const Array *to_type, Context *ctx) {
  static base::guarded<base::unordered_map<
      const Array *, base::unordered_map<const Array *, ir::Func *>>>
      init_fns;

  auto handle         = init_fns.lock();
  auto[iter, success] = (*handle)[to_type].emplace(from_type, nullptr);
  if (success) {
    auto *fn =
        ctx->mod_->AddFunc(type::Func({from_type, type::Ptr(to_type)}, {}),
                           ast::FnParams<ast::Expression *>(2));
    iter->second = fn;

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      auto from_arg           = fn->Argument(0);
      auto to_arg             = fn->Argument(1);
      auto phi_block          = ir::Func::Current->AddBlock();
      auto body_block         = ir::Func::Current->AddBlock();
      auto exit_block         = ir::Func::Current->AddBlock();
      auto from_start         = ir::Index(type::Ptr(from_type), from_arg, 0);
      auto to_start           = ir::Index(type::Ptr(to_type), to_arg, 0);
      auto from_end           = ir::PtrIncr(from_start, from_type->len,
                                  type::Ptr(from_type->data_type));
      ir::UncondJump(phi_block);

      ir::BasicBlock::Current = phi_block;
      auto from_phi_index     = ir::Phi(type::Ptr(from_type->data_type));
      auto to_phi_index       = ir::Phi(type::Ptr(from_type->data_type));
      auto from_phi_reg = ir::Func::Current->Command(from_phi_index).result;
      type::Pointer const *from_phi_reg_type = type::Ptr(from_type->data_type);
      auto to_phi_reg = ir::Func::Current->Command(to_phi_index).result;
      type::Pointer const *to_phi_reg_type = type::Ptr(to_type->data_type);

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
    auto *fn = iter->second =
        ctx->mod_->AddFunc(Func({Ptr(struct_type), Ptr(struct_type)}, {}),
                           ast::FnParams<ast::Expression *>(2));

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

void EmitCopyInit(Type const *from_type, Type const *to_type,
                  ir::Val const &from_val, ir::Register to_var, Context *ctx) {
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
    to_type->as<Variant>().EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Tuple>()) {
    // TODO destruction in assignment may cause problems.
    to_type->as<Tuple>().EmitAssign(from_type, from_val, to_var, ctx);
  } else {
    UNREACHABLE(to_type->to_string(), from_type->to_string());
  }
}

void EmitMoveInit(Type const *from_type, Type const *to_type,
                  ir::Val const &from_val, ir::Register to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>()) {
    ASSERT(to_type == from_type);
    to_type->EmitAssign(from_type, from_val, to_var, ctx);

  } else if (to_type->is<Array>()) {
    auto *to_array_type   = &to_type->as<Array>();
    auto *from_array_type = &from_type->as<Array>();

    ir::Arguments call_args;
    call_args.append(from_val);
    call_args.append(to_var);
    ir::Func *f = ArrayInitializationWith<EmitMoveInit>(
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
  } else if (to_type->is<Function>()) {
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  }
}

}  // namespace type
