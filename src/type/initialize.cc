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
    to_type->EmitCopyAssign(from_type, from_val, to_var, ctx);
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
    to_type->as<Variant>().EmitCopyAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Tuple>()) {
    // TODO destruction in assignment may cause problems.
    to_type->as<Tuple>().EmitCopyAssign(from_type, from_val, to_var, ctx);
  } else {
    UNREACHABLE(to_type->to_string(), from_type->to_string());
  }
}

void EmitMoveInit(Type const *from_type, Type const *to_type,
                  ir::Val const &from_val, ir::Register to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() || to_type->is<Flags>() ||
      to_type->is<Pointer>()) {
    ASSERT(to_type == from_type);
    to_type->EmitCopyAssign(from_type, from_val, to_var, ctx);

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
    to_type->EmitCopyAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitCopyAssign(from_type, from_val, to_var, ctx);
  }
}

}  // namespace type
