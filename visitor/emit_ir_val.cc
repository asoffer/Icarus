#include "visitor/emit_ir.h"

#include "ast/ast.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "ir/builtin_ir.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "ir/phi.h"
#include "ir/register.h"
#include "misc/context.h"
#include "type/generic_struct.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace ir {
// TODO duplicated in verify_type
static type::Type const *BuiltinType(core::Builtin b) {
  switch (b) {
#define ICARUS_CORE_BUILTIN_X(enumerator, str, t)                              \
  case core::Builtin::enumerator:                                              \
    return t;
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
  }
  UNREACHABLE();
}

// TODO moved these here because we can't have them in ir:builtin or else
// they'll be in the formatter target. figure out what's going on here.
type::Type const *BuiltinType(core::Builtin);

// TODO: The functions here that modify struct fields typically do so by
// modifying the last field, since we always build them in order. This saves us
// from having to pass extra information and thereby bloating all commands. At
// some point we should switch to a buffer-chunk system so that one won't bloat
// another.
Reg CreateStruct(core::Scope const *scope, ast::StructLiteral const *parent);
void CreateStructField(Reg struct_type, RegisterOr<type::Type const *> type);
void SetStructFieldName(Reg struct_type, std::string_view field_name);
void AddHashtagToField(Reg struct_type, ast::Hashtag hashtag);
void AddHashtagToStruct(Reg struct_type, ast::Hashtag hashtag);
Reg FinalizeStruct(Reg r);

RegisterOr<type::Type const *> Variant(
    std::vector<RegisterOr<type::Type const *>> const &vals);

RegisterOr<type::Type const *> Tup(
    std::vector<RegisterOr<type::Type const *>> const &entries);

// TODO as a general rule we let ast reach into ir but not the other direction.
// Fix this.
Reg CreateEnum(ast::EnumLiteral::Kind kind, ::Module *mod) {
  switch (kind) {
    case ast::EnumLiteral::Kind::Enum: {
      auto &cmd = MakeCmd(type::Type_, Op::CreateEnum);
      cmd.mod_  = mod;
      return cmd.result;
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto &cmd = MakeCmd(type::Type_, Op::CreateFlags);
      cmd.mod_  = mod;
      return cmd.result;
    } break;
    default: UNREACHABLE();
  }
}

void AddEnumerator(ast::EnumLiteral::Kind kind, Reg reg,
                   std::string_view token) {
  switch (kind) {
    case ast::EnumLiteral::Kind::Enum: {
      auto &cmd           = MakeCmd(type::Type_, Op::AddEnumerator);
      cmd.add_enumerator_ = {reg, token};
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto &cmd           = MakeCmd(type::Type_, Op::AddFlag);
      cmd.add_enumerator_ = {reg, token};
    } break;
    default: UNREACHABLE();
  }
}

void SetEnumerator(Reg reg, RegisterOr<int32_t> val) {
  auto &cmd           = MakeCmd(type::Type_, Op::SetEnumerator);
  cmd.set_enumerator_ = {reg, val};
}

TypedRegister<type::Type const *> FinalizeEnum(ast::EnumLiteral::Kind kind,
                                               ir::Reg reg) {
  switch (kind) {
    case ast::EnumLiteral::Kind::Enum: {
      auto &cmd = MakeCmd(type::Type_, Op::FinalizeEnum);
      cmd.reg_  = reg;
      return cmd.result;
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto &cmd = MakeCmd(type::Type_, Op::FinalizeFlags);
      cmd.reg_  = reg;
      return cmd.result;
    } break;
    default: UNREACHABLE();
  }
}

}  // namespace ir

namespace visitor {
using ::matcher::InheritsFrom;

static void MakeAllStackAllocations(core::FnScope const *fn_scope,
                                    Context *ctx) {
  for (auto *scope : fn_scope->innards_) {
    for (const auto &[key, val] : scope->decls_) {
      for (auto *decl : val) {
        if (decl->const_ || decl->is_fn_param_ ||
            decl->is<ast::MatchDeclaration>()) {
          continue;
        }

        // TODO it's wrong to use a default BoundConstants, but it's even more
        // wrong to store the address on the declaration, so you can fix those
        // together.
        ctx->set_addr(decl, ir::Alloca(ctx->type_of(decl)));
      }
    }
  }
}

static void MakeAllDestructions(EmitIr const *visitor,
                                core::ExecScope const *exec_scope,
                                Context *ctx) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<ast::Declaration *> ordered_decls;
  for (auto &[name, decls] : exec_scope->decls_) {
    ordered_decls.insert(ordered_decls.end(), decls.begin(), decls.end());
  }

  // TODO eek, don't use line number to determine destruction order!
  std::sort(ordered_decls.begin(), ordered_decls.end(),
            [](ast::Declaration *lhs, ast::Declaration *rhs) {
              return (lhs->span.start.line_num > rhs->span.start.line_num) ||
                     (lhs->span.start.line_num == rhs->span.start.line_num &&
                      lhs->span.start.offset > rhs->span.start.offset);
            });

  for (auto *decl : ordered_decls) {
    auto *t = ASSERT_NOT_NULL(ctx->type_of(decl));
    if (!t->HasDestructor()) { continue; }
    t->EmitDestroy(visitor, ctx->addr(decl), ctx);
  }
}

static void EmitIrForStatements(EmitIr const *visitor,
                                ast::NodeSpan<ast::Node const> span, Context *ctx) {
  std::vector<type::Typed<ir::Reg>> to_destroy;
  auto *old_tmp_ptr = std::exchange(ctx->temporaries_to_destroy_, &to_destroy);
  bool old_more_stmts_allowed = std::exchange(ctx->more_stmts_allowed_, true);
  base::defer d([&] {
    ctx->temporaries_to_destroy_ = old_tmp_ptr;
    ctx->more_stmts_allowed_     = old_more_stmts_allowed;
  });

  for (auto *stmt : span) {
    stmt->EmitIr(visitor, ctx);
    for (auto iter = to_destroy.rbegin(); iter != to_destroy.rend(); ++iter) {
      iter->type()->EmitDestroy(visitor, iter->get(), ctx);
    }
    to_destroy.clear();
  }
}

static void CompleteBody(EmitIr const *visitor,
                         ast::FunctionLiteral const *node, Context *ctx) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = ctx->type_of(node);

  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[node];

  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block        = ir::CompiledFn::Current->AddBlock();
    ir::BasicBlock::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (int32_t i = 0; i < static_cast<int32_t>(node->inputs_.size()); ++i) {
      ctx->set_addr(node->inputs_.at(i).value.get(), ir::Reg::Arg(i));
    }

    MakeAllStackAllocations(node->fn_scope_.get(), ctx);

    for (size_t i = 0; i < node->outputs_.size(); ++i) {
      auto *out_decl = node->outputs_[i]->if_as<ast::Declaration>();
      if (!out_decl) { continue; }
      auto *out_decl_type = ASSERT_NOT_NULL(ctx->type_of(out_decl));
      auto alloc = out_decl_type->is_big() ? ir::GetRet(i, out_decl_type)
                                           : ir::Alloca(out_decl_type);

      ctx->set_addr(out_decl, alloc);
      if (out_decl->IsDefaultInitialized()) {
        out_decl_type->EmitDefaultInit(visitor, alloc, ctx);
      } else {
        out_decl_type->EmitCopyAssign(visitor, out_decl_type,
                                      out_decl->init_val->EmitIr(visitor, ctx),
                                      alloc, ctx);
      }
    }

    {
      std::vector<type::Typed<ir::Reg>> to_destroy;
      auto *old_tmp_ptr =
          std::exchange(ctx->temporaries_to_destroy_, &to_destroy);
      bool old_more_stmts_allowed =
          std::exchange(ctx->more_stmts_allowed_, true);
      base::defer d([&] {
        ctx->temporaries_to_destroy_ = old_tmp_ptr;
        ctx->more_stmts_allowed_     = old_more_stmts_allowed;
      });
      for (auto &stmt : node->statements_) {
        stmt->EmitIr(visitor, ctx);
        for (int i = static_cast<int>(to_destroy.size()) - 1; i >= 0; --i) {
          auto &reg = to_destroy.at(i);
          reg.type()->EmitDestroy(visitor, reg.get(), ctx);
        }
        to_destroy.clear();
      }
    }

    MakeAllDestructions(visitor, node->fn_scope_.get(), ctx);

    if (t->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      ir::ReturnJump();
    }

    ir::BasicBlock::Current = ir_func->entry();
    ir::UncondJump(start_block);
    ir_func->work_item = nullptr;
  }
}

static void CompleteBody(EmitIr const *visitor, ast::StructLiteral const *node,
                         Context *ctx) {
  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[node];
  for (size_t i = 0; i < node->args_.size(); ++i) {
    ctx->set_addr(&node->args_[i], ir::Reg::Arg(i));
  }

  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    auto cache_slot_addr    = ir::ArgumentCache(node);
    auto cache_slot         = ir::Load<type::Type const *>(cache_slot_addr);

    auto land_block         = ir::CompiledFn::Current->AddBlock();
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        land_block,
        ir::Eq(cache_slot, static_cast<type::Type const *>(nullptr)));
    auto ctx_reg    = ir::CreateContext(ctx->mod_);
    auto struct_reg = ir::CreateStruct(node->scope_, node);

    // TODO why isn't implicit TypedRegister -> RegisterOr cast working on
    // either of these? On the first it's clear because we don't even return a
    // typedRegister, but this is a note to remind you to make that work. On the
    // second... I don't know.
    ir::Store(static_cast<ir::RegisterOr<type::Type const *>>(struct_reg),
              cache_slot_addr);
    for (auto &arg : node->args_) {  // TODO const-ref
      ir::AddBoundConstant(ctx_reg, &arg, ctx->addr(&arg));
    }

    for (auto &field : node->fields_) {  // TODO const-ref
      ir::VerifyType(&field, ctx_reg);

      // TODO exit early if verifytype fails.

      auto type_reg = ir::EvaluateAsType(field.type_expr.get(), ctx_reg);

      ir::CreateStructField(struct_reg, type_reg);
      ir::SetStructFieldName(struct_reg, field.id_);

      for (auto const &hashtag : field.hashtags_) {
        ir::AddHashtagToField(struct_reg, hashtag);
      }
    }

    for (auto hashtag : node->hashtags_) {
      ir::AddHashtagToStruct(struct_reg, hashtag);
    }

    ir::RegisterOr<type::Type const *> result = ir::FinalizeStruct(struct_reg);
    ir::DestroyContext(ctx_reg);

    // Exit path from creating a new struct.
    ir::SetRet(0, static_cast<ir::RegisterOr<type::Type const *>>(result));
    ir::Store(static_cast<ir::RegisterOr<type::Type const *>>(result),
              cache_slot_addr);
    ir::ReturnJump();

    // Exit path from finding the cache
    ir::BasicBlock::Current = land_block;
    ir::SetRet(0, static_cast<ir::RegisterOr<type::Type const *>>(cache_slot));
    ir::ReturnJump();
  }
}

ir::Results EmitIr::Val(ast::Node const *node, Context *ctx) const {
  UNREACHABLE();
}

ir::Results EmitIr::Val(ast::Access const *node, Context *ctx) const {
  if (ctx->type_of(node->operand()) == type::Module) {
    // TODO we already did this evaluation in type verification. Can't we just
    // save and reuse it?
    return backend::EvaluateAs<Module const *>(node->operand(), ctx)
        ->GetDecl(node->member_name())
        ->EmitIr(this, ctx);
  }

  auto *this_type = ctx->type_of(node);
  if (this_type->is<type::Enum>()) {
    auto lit = this_type->as<type::Enum>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else {
    auto reg = node->operand()->EmitLVal(this, ctx)[0];
    auto *t  = ctx->type_of(node->operand());

    if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
    while (t->is<type::Pointer>()) {
      t   = t->as<type::Pointer>().pointee;
      reg = ir::Load<ir::Addr>(reg, t);
    }

    ASSERT(t, InheritsFrom<type::Struct>());
    auto *struct_type = &t->as<type::Struct>();
    auto field =
        ir::Field(reg, struct_type, struct_type->index(node->member_name()));
    return ir::Results{ir::PtrFix(field.get(), this_type)};
  }
}

ir::Results EmitIr::Val(ast::ArrayLiteral const *node, Context *ctx) const {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = ctx->type_of(node);
  auto alloc      = ir::TmpAlloca(this_type, ctx);
  if (!node->empty()) {
    auto *data_type = this_type->as<type::Array>().data_type;
    for (size_t i = 0; i < node->size(); ++i) {
      MoveInit(data_type, node->elem(i)->EmitIr(this, ctx),
               type::Typed<ir::Reg>(ir::Index(type::Ptr(this_type), alloc,
                                              static_cast<int32_t>(i)),
                                    type::Ptr(data_type)),
               ctx);
    }
  }
  return ir::Results{alloc};
}

ir::Results EmitIr::Val(ast::ArrayType const *node, Context *ctx) const {
  auto result = node->data_type()->EmitIr(this, ctx).get<type::Type const *>(0);
  // Size must be at least 1 by construction, so `.size() - 1` will not
  // overflow.
  for (int i = node->lengths().size() - 1; i >= 0; --i) {
    result =
        ir::Array(node->length(i)->EmitIr(this, ctx).get<int64_t>(0), result);
  }
  return ir::Results{result};
}

ir::Results EmitIr::Val(ast::Binop const *node, Context *ctx) const {
  auto *lhs_type = ctx->type_of(node->lhs());
  auto *rhs_type = ctx->type_of(node->rhs());

  if (auto *dispatch_table = ctx->dispatch_table(node)) {
    // TODO struct is not exactly right. we really mean user-defined
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
            {std::pair(node->lhs(), node->lhs()->EmitIr(this, ctx)),
             std::pair(node->rhs(), node->rhs()->EmitIr(this, ctx))},
            {}),
        ctx);
  }

  switch (node->op()) {
    case frontend::Operator::Add: {
      auto lhs_ir = node->lhs()->EmitIr(this, ctx);
      auto rhs_ir = node->rhs()->EmitIr(this, ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Add(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Sub: {
      auto lhs_ir = node->lhs()->EmitIr(this, ctx);
      auto rhs_ir = node->rhs()->EmitIr(this, ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Sub(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = node->lhs()->EmitIr(this, ctx);
      auto rhs_ir = node->rhs()->EmitIr(this, ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Mul(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = node->lhs()->EmitIr(this, ctx);
      auto rhs_ir = node->rhs()->EmitIr(this, ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Div(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = node->lhs()->EmitIr(this, ctx);
      auto rhs_ir = node->rhs()->EmitIr(this, ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Mod(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Arrow: {
      // TODO ugly hack.
      std::vector<ir::RegisterOr<type::Type const *>> lhs_vals, rhs_vals;
      if (auto *l = node->lhs()->if_as<ast::CommaList>()) {
        for (auto &e : l->exprs_) {
          lhs_vals.push_back(e->EmitIr(this, ctx).get<type::Type const *>(0));
        }
      } else {
        lhs_vals.push_back(
            node->lhs()->EmitIr(this, ctx).get<type::Type const *>(0));
      }
      if (auto *r = node->rhs()->if_as<ast::CommaList>()) {
        for (auto &e : r->exprs_) {
          rhs_vals.push_back(e->EmitIr(this, ctx).get<type::Type const *>(0));
        }
      } else {
        rhs_vals.push_back(
            node->rhs()->EmitIr(this, ctx).get<type::Type const *>(0));
      }

      auto reg_or_type = ir::Arrow(ir::Tup(lhs_vals), ir::Tup(rhs_vals));
      return ir::Results{reg_or_type};
    } break;
    case frontend::Operator::Assign: {
      // TODO support splatting.
      auto lhs_lvals = node->lhs()->EmitLVal(this, ctx);
      if (lhs_lvals.size() != 1) { NOT_YET(); }

      auto rhs_vals = node->rhs()->EmitIr(this, ctx);
      lhs_type->EmitMoveAssign(this, rhs_type, rhs_vals, lhs_lvals[0], ctx);

      return ir::Results{};
    } break;
    case frontend::Operator::OrEq: {
      auto *this_type = ctx->type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
        ir::Store(
            ir::OrFlags(&this_type->as<type::Flags>(),
                        ir::Load<ir::FlagsVal>(lhs_lval, this_type),
                        node->rhs()->EmitIr(this, ctx).get<ir::FlagsVal>(0)),
            lhs_lval);
        return ir::Results{};
      }
      auto land_block = ir::CompiledFn::Current->AddBlock();
      auto more_block = ir::CompiledFn::Current->AddBlock();

      auto lhs_val       = node->lhs()->EmitIr(this, ctx).get<bool>(0);
      auto lhs_end_block = ir::BasicBlock::Current;
      ir::CondJump(lhs_val, land_block, more_block);

      ir::BasicBlock::Current = more_block;
      auto rhs_val            = node->rhs()->EmitIr(this, ctx).get<bool>(0);
      auto rhs_end_block      = ir::BasicBlock::Current;
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{
          ir::MakePhi<bool>(ir::Phi(type::Bool),
                            {{lhs_end_block, true}, {rhs_end_block, rhs_val}})};
    } break;
    case frontend::Operator::AndEq: {
      auto *this_type = ctx->type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
        ir::Store(
            ir::AndFlags(&this_type->as<type::Flags>(),
                         ir::Load<ir::FlagsVal>(lhs_lval, this_type),
                         node->rhs()->EmitIr(this, ctx).get<ir::FlagsVal>(0)),
            lhs_lval);
        return ir::Results{};
      }

      auto land_block = ir::CompiledFn::Current->AddBlock();
      auto more_block = ir::CompiledFn::Current->AddBlock();

      auto lhs_val       = node->lhs()->EmitIr(this, ctx).get<bool>(0);
      auto lhs_end_block = ir::BasicBlock::Current;
      ir::CondJump(lhs_val, more_block, land_block);

      ir::BasicBlock::Current = more_block;
      auto rhs_val            = node->rhs()->EmitIr(this, ctx).get<bool>(0);
      auto rhs_end_block      = ir::BasicBlock::Current;
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{ir::MakePhi<bool>(
          ir::Phi(type::Bool),
          {{lhs_end_block, rhs_val}, {rhs_end_block, false}})};
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
      auto rhs_ir   = node->rhs()->EmitIr(this, ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Add(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
      auto rhs_ir   = node->rhs()->EmitIr(this, ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Sub(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
      auto rhs_ir   = node->rhs()->EmitIr(this, ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
      auto rhs_ir   = node->rhs()->EmitIr(this, ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t>(rhs_type, [&](auto type_holder) {
        using T = typename decltype(type_holder)::type;
        ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)), lhs_lval);
      });
      return ir::Results{};
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
      auto rhs_ir   = node->rhs()->EmitIr(this, ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Mul(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = node->lhs()->EmitLVal(this, ctx)[0];
        auto rhs_ir   = node->rhs()->EmitIr(this, ctx).get<bool>(0);
        ir::Store(ir::XorBool(ir::Load<bool>(lhs_lval), rhs_ir), lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = node->lhs()->EmitLVal(this, ctx)[0];
        auto rhs_ir      = node->rhs()->EmitIr(this, ctx).get<ir::FlagsVal>(0);
        ir::Store(
            ir::XorFlags(flags_type,
                         ir::Load<ir::FlagsVal>(lhs_lval, flags_type), rhs_ir),
            lhs_lval);
      } else {
        UNREACHABLE(lhs_type);
      }
      return ir::Results{};
    } break;
    default: UNREACHABLE(*node);
  }
  UNREACHABLE(*node);
}

ir::Results EmitIr::Val(ast::BlockLiteral const *node, Context *ctx) const {
  ir::BlockSequence seq;
  seq.append(ir::Block(node));
  return ir::Results{seq};
}

ir::Results EmitIr::Val(ast::BlockNode const *node, Context *ctx) const {
  EmitIrForStatements(this, node->stmts(), ctx);
  MakeAllDestructions(this, node->body_scope(), ctx);
  return ir::Results{};
}

ir::Results EmitIr::Val(ast::BuiltinFn const *node, Context *ctx) const {
  return ir::Results{node->value()};
}

ir::Results EmitIr::Val(ast::Call const *node, Context *ctx) const {
  if (auto *b = node->fn_->if_as<ast::BuiltinFn>()) {
    switch (b->value()) {
      case core::Builtin::Foreign: {
        auto name =
            backend::EvaluateAs<std::string_view>(node->args_.at(0).get(), ctx);
        auto *foreign_type = backend::EvaluateAs<type::Type const *>(
            node->args_.at(1).get(), ctx);
        return ir::Results{ir::LoadSymbol(name, foreign_type).get()};
      } break;

      case core::Builtin::Opaque:
        return ir::Results{static_cast<ir::Reg>(ir::NewOpaqueType(ctx->mod_))};

      case core::Builtin::Bytes: {
        auto const &fn_type =
            ir::BuiltinType(core::Builtin::Bytes)->as<type::Function>();
        ir::Arguments call_args{&fn_type, node->args_.at(0)->EmitIr(this, ctx)};

        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        ir::Call(ir::BytesFn(), std::move(call_args), std::move(outs));

        return ir::Results{reg};
      } break;

      case core::Builtin::Alignment: {
        auto const &fn_type =
            ir::BuiltinType(core::Builtin::Alignment)->as<type::Function>();
        ir::Arguments call_args{&fn_type, node->args_.at(0)->EmitIr(this, ctx)};

        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        ir::Call(ir::AlignmentFn(), std::move(call_args), std::move(outs));

        return ir::Results{reg};
      } break;

#ifdef DBG
      case core::Builtin::DebugIr: ir::DebugIr(); return ir::Results{};
#endif  // DBG
    }
    UNREACHABLE();
  }

  auto const &dispatch_table = *ASSERT_NOT_NULL(ctx->dispatch_table(node));
  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args_.Transform(
      [this, ctx](std::unique_ptr<ast::Expression> const &expr)
          -> std::pair<ast::Expression const *, ir::Results> {
        return std::pair(expr.get(), expr->EmitIr(this, ctx));
      });

  return dispatch_table.EmitCall(
      args, ctx,
      node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}

ir::Results EmitIr::Val(ast::Cast const *node, Context *ctx) const {
  if (auto *dispatch_table = ctx->dispatch_table(node)) {
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
            {std::pair(node->expr_.get(), node->expr_->EmitIr(this, ctx)),
             std::pair(node->type_.get(), node->type_->EmitIr(this, ctx))},
            {}),
        ctx);
  }

  auto *this_type = ASSERT_NOT_NULL(ctx->type_of(node));
  auto results    = node->expr_->EmitIr(this, ctx);
  if (this_type == type::Type_) {
    std::vector<type::Type const *> entries;
    entries.reserve(results.size());
    for (size_t i = 0; i < results.size(); ++i) {
      // TODO what about incomplete structs?
      entries.push_back(results.get<type::Type const *>(i).val_);
    }
    return ir::Results{type::Tup(entries)};
  }
  return ir::Cast(ctx->type_of(node->expr_.get()), this_type, results);
}

static base::guarded<absl::flat_hash_map<
    type::Array const *,
    absl::flat_hash_map<type::Array const *, ir::CompiledFn *>>>
    eq_funcs;
static base::guarded<absl::flat_hash_map<
    type::Array const *,
    absl::flat_hash_map<type::Array const *, ir::CompiledFn *>>>
    ne_funcs;
// TODO this should early exit if the types aren't equal.
ir::Results ArrayCompare(type::Array const *lhs_type, ir::Results const &lhs_ir,
                         type::Array const *rhs_type, ir::Results const &rhs_ir,
                         bool equality, Context *ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto [iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto *fn = ctx->mod_->AddFunc(
        type::Func({type::Ptr(lhs_type), type::Ptr(rhs_type)}, {type::Bool}),
        core::FnParams(core::Param{"",
                                   type::Typed<ast::Expression const *>{
                                       nullptr, type::Ptr(lhs_type)}},
                       core::Param{"", type::Typed<ast::Expression const *>{
                                           nullptr, type::Ptr(rhs_type)}}));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto equal_len_block = ir::CompiledFn::Current->AddBlock();
      auto true_block      = ir::CompiledFn::Current->AddBlock();
      auto false_block     = ir::CompiledFn::Current->AddBlock();
      auto phi_block       = ir::CompiledFn::Current->AddBlock();
      auto body_block      = ir::CompiledFn::Current->AddBlock();
      auto incr_block      = ir::CompiledFn::Current->AddBlock();

      ir::CondJump(ir::Eq(lhs_type->len, rhs_type->len), equal_len_block,
                   false_block);

      ir::BasicBlock::Current = true_block;
      ir::SetRet(0, true);
      ir::ReturnJump();

      ir::BasicBlock::Current = false_block;
      ir::SetRet(0, false);
      ir::ReturnJump();

      ir::BasicBlock::Current = equal_len_block;
      auto lhs_start          = ir::Index(Ptr(lhs_type), ir::Reg::Arg(0), 0);
      auto rhs_start          = ir::Index(Ptr(rhs_type), ir::Reg::Arg(1), 0);
      auto lhs_end =
          ir::PtrIncr(lhs_start, lhs_type->len, Ptr(rhs_type->data_type));
      ir::UncondJump(phi_block);

      ir::BasicBlock::Current = phi_block;
      auto lhs_phi_index      = ir::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi_index      = ir::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg = ir::CompiledFn::Current->Command(lhs_phi_index).result;
      auto rhs_phi_reg = ir::CompiledFn::Current->Command(rhs_phi_index).result;

      ir::CondJump(ir::Eq(ir::RegisterOr<ir::Addr>(lhs_phi_reg), lhs_end),
                   true_block, body_block);

      ir::BasicBlock::Current = body_block;
      // TODO what if data type is an array?
      ir::CondJump(ir::Eq(ir::Load<ir::Addr>(lhs_phi_reg, lhs_type->data_type),
                          ir::Load<ir::Addr>(rhs_phi_reg, rhs_type->data_type)),
                   incr_block, false_block);

      ir::BasicBlock::Current = incr_block;
      auto lhs_incr = ir::PtrIncr(lhs_phi_reg, 1, Ptr(lhs_type->data_type));
      auto rhs_incr = ir::PtrIncr(rhs_phi_reg, 1, Ptr(rhs_type->data_type));
      ir::UncondJump(phi_block);

      ir::MakePhi<ir::Addr>(lhs_phi_index, {{equal_len_block, lhs_start},
                                            {incr_block, lhs_incr}});
      ir::MakePhi<ir::Addr>(rhs_phi_index, {{equal_len_block, rhs_start},
                                            {incr_block, rhs_incr}});
    }
  }

  ir::Arguments call_args{iter->second->type_, ir::Results{lhs_ir, rhs_ir}};
  ir::OutParams outs;
  auto result = outs.AppendReg(type::Bool);

  ir::Call(ir::AnyFunc{iter->second}, std::move(call_args), std::move(outs));
  return ir::Results{result};
}

static ir::RegisterOr<bool> EmitChainOpPair(ast::ChainOp const *chain_op,
                                            size_t index,
                                            ir::Results const &lhs_ir,
                                            ir::Results const &rhs_ir,
                                            Context *ctx) {
  auto *lhs_type = ctx->type_of(chain_op->exprs[index].get());
  auto *rhs_type = ctx->type_of(chain_op->exprs[index + 1].get());
  auto op        = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    using ::matcher::Eq;
    ASSERT(op, Eq(frontend::Operator::Eq) || Eq(frontend::Operator::Ne));
    return ArrayCompare(&lhs_type->as<type::Array>(), lhs_ir,
                        &rhs_type->as<type::Array>(), rhs_ir,
                        op == frontend::Operator::Eq, ctx)
        .get<bool>(0);
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    auto results =
        ASSERT_NOT_NULL(
            ctx->dispatch_table(reinterpret_cast<ast::Expression *>(
                reinterpret_cast<uintptr_t>(chain_op->exprs[index].get()) |
                0x1)))
            ->EmitCall(
                core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                    {std::pair(chain_op->exprs[index].get(), lhs_ir),
                     std::pair(chain_op->exprs[index + 1].get(), rhs_ir)},
                    {}),
                ctx);
    ASSERT(results.size() == 1u);
    return results.get<bool>(0);

  } else {
    switch (op) {
      case frontend::Operator::Lt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Lt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Le(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Eq:
        if (lhs_type == type::Block || lhs_type == type::OptBlock ||
            lhs_type == type::RepBlock) {
          auto val1 = lhs_ir.get<ir::BlockSequence>(0);
          auto val2 = rhs_ir.get<ir::BlockSequence>(0);
          if (!val1.is_reg_ && !val2.is_reg_) { return val1.val_ == val2.val_; }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto type_holder) {
              using T = typename decltype(type_holder)::type;
              return ir::Eq(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ne:
        if (lhs_type == type::Block || lhs_type == type::OptBlock ||
            lhs_type == type::RepBlock) {
          auto val1 = lhs_ir.get<ir::BlockSequence>(0);
          auto val2 = rhs_ir.get<ir::BlockSequence>(0);
          if (!val1.is_reg_ && !val2.is_reg_) { return val1.val_ == val2.val_; }
        } else if (lhs_type == type::Bool) {
          return ir::XorBool(lhs_ir.get<bool>(0), rhs_ir.get<bool>(0));
        }
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto type_holder) {
              using T = typename decltype(type_holder)::type;
              return ir::Ne(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Ge(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Gt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
        // TODO case frontend::Operator::And: cmp = lhs_ir; break;
      default: UNREACHABLE();
    }
  }
}

ir::Results EmitIr::Val(ast::ChainOp const *node, Context *ctx) const {
  auto *t = ctx->type_of(node);
  if (node->ops[0] == frontend::Operator::Xor) {
    if (t == type::Bool) {
      return ir::Results{std::accumulate(
          node->exprs.begin(), node->exprs.end(), ir::RegisterOr<bool>(false),
          [&](ir::RegisterOr<bool> acc, auto &expr) {
            return ir::XorBool(acc,
                               expr->EmitIr(this, ctx).template get<bool>(0));
          })};
    } else if (t->is<type::Flags>()) {
      return ir::Results{std::accumulate(
          node->exprs.begin(), node->exprs.end(),
          ir::RegisterOr<ir::FlagsVal>(ir::FlagsVal{0}),
          [&](ir::RegisterOr<ir::FlagsVal> acc, auto &expr) {
            return ir::XorFlags(
                &t->as<type::Flags>(), acc,
                expr->EmitIr(this, ctx).template get<ir::FlagsVal>(0));
          })};
    } else {
      UNREACHABLE();
    }

  } else if (node->ops[0] == frontend::Operator::Or && t->is<type::Flags>()) {
    auto iter = node->exprs.begin();
    auto val  = (*iter)->EmitIr(this, ctx).get<ir::FlagsVal>(0);
    while (++iter != node->exprs.end()) {
      val = ir::OrFlags(&t->as<type::Flags>(), val,
                        (*iter)->EmitIr(this, ctx).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops[0] == frontend::Operator::And && t->is<type::Flags>()) {
    auto iter = node->exprs.begin();
    auto val  = (*iter)->EmitIr(this, ctx).get<ir::FlagsVal>(0);
    while (++iter != node->exprs.end()) {
      val = ir::AndFlags(&t->as<type::Flags>(), val,
                         (*iter)->EmitIr(this, ctx).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops[0] == frontend::Operator::Or && t == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<ir::RegisterOr<type::Type const *>> args;
    args.reserve(node->exprs.size());
    for (const auto &expr : node->exprs) {
      args.push_back(expr->EmitIr(this, ctx).get<type::Type const *>(0));
    }
    auto reg_or_type = ir::Variant(args);
    return ir::Results{reg_or_type};
  } else if (node->ops[0] == frontend::Operator::Or &&
             (t == type::Block || t == type::OptBlock)) {
    ir::BlockSequence seq;
    for (auto &expr : node->exprs) {
      auto reg_or_seq = expr->EmitIr(this, ctx).get<ir::BlockSequence>(0);
      ASSERT(reg_or_seq.is_reg_ == false);
      seq |= reg_or_seq.val_;
    }
    return ir::Results{seq};
  } else if (node->ops[0] == frontend::Operator::And ||
             node->ops[0] == frontend::Operator::Or) {
    auto land_block = ir::CompiledFn::Current->AddBlock();

    absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_args;
    bool is_or = (node->ops[0] == frontend::Operator::Or);
    for (size_t i = 0; i + 1 < node->exprs.size(); ++i) {
      auto val = node->exprs[i]->EmitIr(this, ctx).get<bool>(0);

      auto next_block = ir::CompiledFn::Current->AddBlock();
      ir::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.emplace(ir::BasicBlock::Current, is_or);

      ir::BasicBlock::Current = next_block;
    }

    phi_args.emplace(ir::BasicBlock::Current,
                     node->exprs.back()->EmitIr(this, ctx).get<bool>(0));
    ir::UncondJump(land_block);

    ir::BasicBlock::Current = land_block;

    return ir::Results{ir::MakePhi<bool>(ir::Phi(type::Bool), phi_args)};

  } else {
    if (node->ops.size() == 1) {
      auto lhs_ir = node->exprs[0]->EmitIr(this, ctx);
      auto rhs_ir = node->exprs[1]->EmitIr(this, ctx);
      return ir::Results{EmitChainOpPair(node, 0, lhs_ir, rhs_ir, ctx)};

    } else {
      absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_args;
      auto lhs_ir     = node->exprs.front()->EmitIr(this, ctx);
      auto land_block = ir::CompiledFn::Current->AddBlock();
      for (size_t i = 0; i < node->ops.size() - 1; ++i) {
        auto rhs_ir = node->exprs[i + 1]->EmitIr(this, ctx);
        auto cmp    = EmitChainOpPair(node, i, lhs_ir, rhs_ir, ctx);

        phi_args.emplace(ir::BasicBlock::Current, false);
        auto next_block = ir::CompiledFn::Current->AddBlock();
        ir::CondJump(cmp, next_block, land_block);
        ir::BasicBlock::Current = next_block;
        lhs_ir                  = std::move(rhs_ir);
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = node->exprs.back()->EmitIr(this, ctx);
      phi_args.emplace(
          ir::BasicBlock::Current,
          EmitChainOpPair(node, node->exprs.size() - 2, lhs_ir, rhs_ir, ctx));
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{ir::MakePhi<bool>(ir::Phi(type::Bool), phi_args)};
    }
  }
}

ir::Results EmitIr::Val(ast::CommaList const *node, Context *ctx) const {
  auto *tuple_type = &ctx->type_of(node)->as<type::Tuple>();
  // TODO this is a hack. I'm still not sure what counts as a tuple and what
  // counts as atype
  if (tuple_type->entries_.empty()) { return ir::Results{type::Tup({})}; }

  auto tuple_alloc = ir::TmpAlloca(tuple_type, ctx);

  size_t index = 0;
  for (auto &expr : node->exprs_) {
    if (expr->needs_expansion()) {
      auto results = expr->EmitIr(this, ctx);
      for (size_t i = 0; i < results.size(); ++i) {
        CopyInit(tuple_type->entries_[index], results.GetResult(i),
                 ir::Field(tuple_alloc, tuple_type, index), ctx);
        ++index;
      }
    } else {
      CopyInit(tuple_type->entries_[index], expr->EmitIr(this, ctx),
               ir::Field(tuple_alloc, tuple_type, index), ctx);
      ++index;
    }
  }
  return ir::Results{tuple_alloc};
}

ir::Results EmitIr::Val(ast::Declaration const *node, Context *ctx) const {
  bool swap_bc    = ctx->mod_ != node->mod_;
  Module *old_mod = std::exchange(ctx->mod_, node->mod_);
  if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
  base::defer d([&] {
    ctx->mod_ = old_mod;
    if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
  });

  if (node->const_) {
    // TODO
    if (node->is_fn_param_) {
      if (auto result = ctx->current_constants_.get_constant(node);
          !result.empty()) {
        return result;
      } else if (auto result = ctx->constants_->first.get_constant(node);
                 !result.empty()) {
        return result;
      } else {
        UNREACHABLE();
      }
    } else {
      auto *t = ctx->type_of(node);
      if (!t) {
        base::Log() << DumpAst::ToString(node);
        UNREACHABLE();
      }

      auto slot = ctx->constants_->second.constants_.reserve_slot(node, t);
      if (auto *result = std::get_if<ir::Results>(&slot)) {
        return std::move(*result);
      }

      auto &[data_offset, num_bytes] =
          std::get<std::pair<size_t, core::Bytes>>(slot);

      if (node->IsCustomInitialized()) {
        // TODO there's a lot of inefficiency here. `buf` is copied into the
        // constants slot and the copied to an ir::Results object to be
        // returned. In reality, we could write directly to the buffer and only
        // copy once if Evaluate* took an out-parameter.
        base::untyped_buffer buf = backend::EvaluateToBuffer(
            type::Typed<ast::Expression const *>(node->init_val.get(), t), ctx);
        if (ctx->num_errors() > 0u) { return ir::Results{}; }
        return ctx->constants_->second.constants_.set_slot(
            data_offset, buf.raw(0), num_bytes);
      } else if (node->IsDefaultInitialized()) {
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
    }
    UNREACHABLE(DumpAst::ToString(node));
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIr. Declaration::EmitIr is just used to set the
    // value.
    ASSERT(node->scope_->Containing<core::FnScope>() != nullptr);

    // TODO these checks actually overlap and could be simplified.
    if (node->IsUninitialized()) { return ir::Results{}; }
    auto *t   = ctx->type_of(node);
    auto addr = ctx->addr(node);
    if (node->IsCustomInitialized()) {
      node->init_val->EmitMoveInit(this, type::Typed(addr, type::Ptr(t)), ctx);
    } else {
      if (!node->is_fn_param_) { t->EmitDefaultInit(this, addr, ctx); }
    }
    return ir::Results{addr};
  }
  UNREACHABLE();
}

ir::Results EmitIr::Val(ast::EnumLiteral const *node, Context *ctx) const {
  auto reg = ir::CreateEnum(node->kind_, ctx->mod_);
  for (auto &elem : node->elems_) {
    if (auto *id = elem->if_as<ast::Identifier>()) {
      ir::AddEnumerator(node->kind_, reg, id->token);
    } else if (auto *decl = elem->if_as<ast::Declaration>()) {
      ir::AddEnumerator(node->kind_, reg, decl->id_);
      if (!decl->IsCustomInitialized()) {
        ir::SetEnumerator(reg,
                          decl->init_val->EmitIr(this, ctx).get<int32_t>(0));
      }
    }
  }
  return ir::Results{ir::FinalizeEnum(node->kind_, reg)};
}

ir::Results EmitIr::Val(ast::FunctionLiteral const *node, Context *ctx) const {
  for (auto const &param : node->inputs_) {
    auto *p = param.value.get();
    if (p->const_ && !ctx->constants_->first.contains(p)) {
      return ir::Results{node};
    }

    for (auto *dep : node->param_dep_graph_.sink_deps(param.value.get())) {
      if (!ctx->constants_->first.contains(dep)) { return ir::Results{node}; }
    }
  }

  // TODO Use correct constants
  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[node];
  if (!ir_func) {
    std::function<void()> *work_item_ptr = nullptr;
    work_item_ptr                        = &ctx->mod_->deferred_work_.emplace(
        [constants{ctx->constants_}, node, this, mod{ctx->mod_}]() mutable {
          Context ctx(mod);
          ctx.constants_ = constants;
          CompleteBody(this, node, &ctx);
        });

    auto *fn_type = &ctx->type_of(node)->as<type::Function>();

    ir_func = ctx->mod_->AddFunc(
        fn_type, node->inputs_.Transform(
                     [fn_type, i = 0](
                         std::unique_ptr<ast::Declaration> const &e) mutable {
                       return type::Typed<ast::Expression const *>(
                           e->init_val.get(), fn_type->input.at(i++));
                     }));
    if (work_item_ptr) { ir_func->work_item = work_item_ptr; }
  }

  return ir::Results{ir_func};
}

ir::Results EmitIr::Val(ast::Identifier const *node, Context *ctx) const {
  ASSERT(node->decl_ != nullptr) << DumpAst::ToString(node);
  if (node->decl_->const_) { return node->decl_->EmitIr(this, ctx); }
  if (node->decl_->is_fn_param_) {
    auto *t     = ctx->type_of(node);
    ir::Reg reg = ctx->addr(node->decl_);
    if (ctx->inline_) {
      ir::Results reg_results = (*ctx->inline_)[reg];
      if (!reg_results.is_reg(0)) { return reg_results; }
      reg = reg_results.get<ir::Reg>(0);
    }

    return ir::Results{
        node->decl_->is_output_ && !t->is_big() ? ir::Load(reg, t) : reg};
  } else if (node->decl_->is<ast::MatchDeclaration>()) {
    // TODO is there a better way to do look up? look up in parent too?
    UNREACHABLE(node->decl_);

  } else {
    auto *t   = ASSERT_NOT_NULL(ctx->type_of(node));
    auto lval = node->EmitLVal(this, ctx)[0];
    if (!lval.is_reg_) { NOT_YET(); }
    return ir::Results{ir::PtrFix(lval.reg_, t)};
  }
}

ir::Results EmitIr::Val(ast::Import const *node, Context *ctx) const {
  return ir::Results{node->module_.get()};
}

ir::Results EmitIr::Val(ast::Index const *node, Context *ctx) const {
  return ir::Results{
      ir::PtrFix(node->EmitLVal(this, ctx)[0].reg_, ctx->type_of(node))};
}

ir::Results EmitIr::Val(ast::Interface const *node, Context *ctx) const {
  // TODO this needs to be serialized as instructions so that we can evaluate
  // functions which return interfaces. For example,
  // HasFoo ::= (T: type) => interface {
  //   foo: T
  // }
  return ir::Results{ir::FinalizeInterface(ir::CreateInterface(node->scope_))};
}

ir::Results EmitIr::Val(ast::MatchDeclaration const *node, Context *ctx) const {
  auto results = ctx->constants_->first.get_constant(node);
  if (!results.empty()) { return results; }
  return ir::Results{
      backend::EvaluateAs<type::Interface const *>(node->type_expr.get(), ctx)};
}

ir::Results EmitIr::Val(ast::RepeatedUnop const *node, Context *ctx) const {
  if (node->op() == frontend::Operator::Jump) {
    ASSERT(node->exprs().size() == 1u);
    auto &call        = node->expr(0)->as<ast::Call>();
    auto *called_expr = call.fn_.get();

    // TODO stop calculating this so many times.
    auto block_seq = backend::EvaluateAs<ir::BlockSequence>(
        type::Typed<ast::Expression const *>(call.fn_.get(), type::Block), ctx);
    auto block = block_seq.at(0);
    if (block == ir::Block::Start()) {
      // Do nothing. You'll just end up jumping to this location and the body
      // will be emit elsewhere.
    } else if (block == ir::Block::Exit()) {
    } else {
      ASSERT_NOT_NULL(ctx->dispatch_table(ast::ExprPtr{&call, 0x01}))
          ->EmitInlineCall({}, {}, ctx);
    }
    ir::JumpPlaceholder(backend::EvaluateAs<ir::BlockSequence>(
        type::Typed<ast::Expression const *>(called_expr, type::Block), ctx));
    return ir::Results{};
  }

  std::vector<ir::Results> arg_vals;
  // TODO expansion
  // if (node->args_.needs_expansion()) {
  //   for (auto &expr : node->args_.exprs_) {
  //     auto vals = expr->EmitIr(this, ctx);
  //     for (size_t i = 0; i < vals.size(); ++i) {
  //       arg_vals.push_back(vals.GetResult(i));
  //     }
  //   }
  // } else {
  for (auto *expr : node->exprs()) {
    arg_vals.push_back(expr->EmitIr(this, ctx));
  }
  //}

  switch (node->op()) {
    case frontend::Operator::Return: {
      size_t offset = 0;
      auto *fn_scope =
          ASSERT_NOT_NULL(node->scope_->Containing<core::FnScope>());
      auto *fn_lit = ASSERT_NOT_NULL(fn_scope->fn_lit_);

      auto *fn_type =
          &ASSERT_NOT_NULL(ctx->type_of(fn_lit))->as<type::Function>();
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        // TODO return type maybe not the same as type actually returned?
        ir::SetRet(i, type::Typed{arg_vals[i], fn_type->output.at(i)}, ctx);
      }

      // Rather than doing this on each block it'd be better to have each
      // scope's destructors jump you to the correct next block for destruction.
      auto *scope = node->scope_;
      while (auto *exec = scope->if_as<core::ExecScope>()) {
        MakeAllDestructions(this, exec, ctx);
        scope = exec->parent;
      }

      ctx->more_stmts_allowed_ = false;
      ir::ReturnJump();
      return ir::Results{};
    }
    case frontend::Operator::Yield: {
      // TODO store this as an exec_scope.
      MakeAllDestructions(this, &node->scope_->as<core::ExecScope>(), ctx);
      // TODO pretty sure this is all wrong.

      // Can't return these because we need to pass them up at least through the
      // containing statements this and maybe further if we allow labelling
      // scopes to be yielded to.
      ctx->yields_stack_.back().clear();
      ctx->yields_stack_.back().reserve(arg_vals.size());
      // TODO one problem with this setup is that we look things up in a context
      // after returning, so the `after` method has access to a different
      // (smaller) collection of bound constants. This can change the meaning of
      // things or at least make them not compile if the `after` function takes
      // a compile-time constant argument.
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        ctx->yields_stack_.back().emplace_back(node->expr(i), arg_vals[i]);
      }
      ctx->more_stmts_allowed_ = false;
      return ir::Results{};
    }
    case frontend::Operator::Print: {
      size_t index = 0;
      // TODO this is wrong if you use the <<(...) spread operator.
      for (auto &val : arg_vals) {
        if (auto const *dispatch_table =
                ctx->dispatch_table(ast::ExprPtr{node->expr(index), 0x01})) {
          dispatch_table->EmitCall(
              core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                  {std::pair(node->expr(index), std::move(val))}, {}),
              ctx);
        } else {
          ctx->type_of(node->expr(index))->EmitPrint(this, val, ctx);
        }
        ++index;
      }
      return ir::Results{};
    } break;
    default: UNREACHABLE("Operator is ", static_cast<int>(node->op()));
  }
}

ir::Results EmitIr::Val(ast::ScopeLiteral const *node, Context *ctx) const {
  return ir::Results{node};
}

ir::Results EmitIr::Val(ast::ScopeNode const *node, Context *ctx) const {
  ctx->yields_stack_.emplace_back();
  base::defer d([&]() { ctx->yields_stack_.pop_back(); });

  auto init_block = ir::CompiledFn::Current->AddBlock();
  auto land_block = ir::CompiledFn::Current->AddBlock();

  absl::flat_hash_map<ir::Block, ir::BlockIndex> block_map{
      {ir::Block::Start(), init_block}, {ir::Block::Exit(), land_block}};

  absl::flat_hash_map<std::string_view,
                      std::tuple<ir::Block, ast::BlockNode const *>>
      name_to_block;
  auto *scope_lit = backend::EvaluateAs<ast::ScopeLiteral *>(node->name(), ctx);
  for (auto const *decl : scope_lit->decls()) {
    if (decl->id_ == "init") {
      continue;
    } else if (decl->id_ == "done") {
      continue;
    } else {
      auto bs = backend::EvaluateAs<ir::BlockSequence>(
          type::Typed<ast::Expression const *>{decl, type::Block}, ctx);
      ASSERT(bs.size() == 1u);
      name_to_block.emplace(std::piecewise_construct,
                            std::forward_as_tuple(decl->id_),
                            std::forward_as_tuple(bs.at(0), nullptr));
    }
  }

  for (auto &block_node : node->blocks()) {
    auto &block        = name_to_block.at(block_node.name());
    std::get<1>(block) = &block_node;
    block_map.emplace(std::get<0>(block), ir::CompiledFn::Current->AddBlock());
  }

  ir::UncondJump(init_block);

  ir::BasicBlock::Current = init_block;

  // TODO this lambda thing is an awful hack.
  ASSERT_NOT_NULL([&] {
    auto *mod       = scope_lit->decl(0)->mod_;
    bool swap_bc    = ctx->mod_ != mod;
    Module *old_mod = std::exchange(ctx->mod_, mod);
    if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
    base::defer d([&] {
      ctx->mod_ = old_mod;
      if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
    });
    return ctx->dispatch_table(ast::ExprPtr{node, 0x02});
  }())
      ->EmitInlineCall(
          node->args().Transform(
              [this, ctx](std::unique_ptr<ast::Expression> const &expr) {
                return std::pair<ast::Expression const *, ir::Results>(
                    expr.get(), expr->EmitIr(this, ctx));
              }),
          block_map, ctx);

  for (auto [block_name, block_and_node] : name_to_block) {
    if (block_name == "init" || block_name == "done") { continue; }
    auto &[block, n] = block_and_node;
    auto iter        = block_map.find(block);
    if (iter == block_map.end()) { continue; }
    ir::BasicBlock::Current = iter->second;
    ASSERT_NOT_NULL(n)->EmitIr(this, ctx);

    ASSERT_NOT_NULL([&] {
      auto *mod       = scope_lit->decl(0)->mod_;
      bool swap_bc    = ctx->mod_ != mod;
      Module *old_mod = std::exchange(ctx->mod_, mod);
      if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
      base::defer d([&] {
        ctx->mod_ = old_mod;
        if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
      });
      return ctx->dispatch_table(ast::ExprPtr{block.get(), 0x02});
    }())
        ->EmitInlineCall({}, block_map, ctx);
  }

  ir::UncondJump(land_block);
  ir::BasicBlock::Current = land_block;

  // TODO this lambda thing is an awful hack.
  return ASSERT_NOT_NULL([&] {
           auto *mod       = scope_lit->decl(0)->mod_;
           bool swap_bc    = ctx->mod_ != mod;
           Module *old_mod = std::exchange(ctx->mod_, mod);
           if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
           base::defer d([&] {
             ctx->mod_ = old_mod;
             if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
           });
           return ctx->dispatch_table(node);
         }())
      ->EmitInlineCall({}, {}, ctx);
}

static ir::TypedRegister<type::Type const *> GenerateStruct(
    EmitIr const *visitor, ast::StructLiteral const *sl, ir::Reg struct_reg,
    Context *ctx) {
  for (auto const &field : sl->fields_) {
    // TODO initial values? hashatgs?

    // NOTE: CreateStructField may invalidate all other struct fields, so it's
    // not safe to access these registers returned by CreateStructField after
    // a subsequent call to CreateStructField.
    ir::CreateStructField(
        struct_reg,
        field.type_expr->EmitIr(visitor, ctx).get<type::Type const *>(0));
    ir::SetStructFieldName(struct_reg, field.id_);
    for (auto const &hashtag : field.hashtags_) {
      ir::AddHashtagToField(struct_reg, hashtag);
    }
  }

  for (auto hashtag : sl->hashtags_) {
    ir::AddHashtagToStruct(struct_reg, hashtag);
  }
  return ir::FinalizeStruct(struct_reg);
}

ir::Results EmitIr::Val(ast::StructLiteral const *node, Context *ctx) const {
  if (node->args_.empty()) {
    return ir::Results{
        GenerateStruct(this, node, ir::CreateStruct(node->scope_, node), ctx)};
  }

  // TODO A bunch of things need to be fixed here.
  // * Lock access during creation so two requestors don't clobber each other.
  // * Add a way way for one requestor to wait for another to have created the
  // object and be notified.
  //
  // For now, it's safe to do this from within a single module compilation
  // (which is single-threaded).
  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[node];
  if (!ir_func) {
    auto &work_item = ctx->mod_->deferred_work_.emplace(
        [constants{ctx->constants_}, node, this, mod{ctx->mod_}]() mutable {
          Context ctx(mod);
          ctx.constants_ = constants;
          CompleteBody(this, node, &ctx);
        });

    auto const &arg_types = ctx->type_of(node)->as<type::GenericStruct>().deps_;

    core::FnParams<type::Typed<ast::Expression const *>> params;
    params.reserve(node->args_.size());
    size_t i = 0;
    for (auto const &d : node->args_) {
      params.append(d.id_, type::Typed<ast::Expression const *>(
                               d.init_val.get(), arg_types.at(i++)));
    }

    ir_func = node->mod_->AddFunc(type::Func(arg_types, {type::Type_}),
                                  std::move(params));

    ir_func->work_item = &work_item;
  }

  return ir::Results{ir::AnyFunc{ir_func}};
}

ir::Results EmitIr::Val(ast::StructType const *node, Context *ctx) const {
  NOT_YET();
}

ir::Results EmitIr::Val(ast::Switch const *node, Context *ctx) const {
  absl::flat_hash_map<ir::BlockIndex, ir::Results> phi_args;
  auto land_block = ir::CompiledFn::Current->AddBlock();
  auto *t         = ctx->type_of(node);
  // TODO this is not precisely accurate if you have regular void.
  bool all_paths_jump = (t == type::Void());

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. this is very wrong.

  // TODO handle switching on tuples/multiple values?
  ir::Results expr_results;
  type::Type const *expr_type = nullptr;
  if (node->expr_) {
    expr_results = node->expr_->EmitIr(this, ctx);
    expr_type    = ctx->type_of(node->expr_.get());
  }

  for (size_t i = 0; i + 1 < node->cases_.size(); ++i) {
    auto &[body, match_cond] = node->cases_[i];
    auto expr_block          = ir::CompiledFn::Current->AddBlock();

    ir::Results match_val = match_cond->EmitIr(this, ctx);
    ir::RegisterOr<bool> cond =
        node->expr_ ? ir::EmitEq(ctx->type_of(match_cond.get()), match_val,
                                 expr_type, expr_results)
                    : match_val.get<bool>(0);

    auto next_block = ir::EarlyExitOn<true>(expr_block, cond);

    ir::BasicBlock::Current = expr_block;
    if (body->is<ast::Expression>()) {
      phi_args[ir::BasicBlock::Current] = body->EmitIr(this, ctx);
      ir::UncondJump(land_block);
    } else {
      // It must be a jump/yield/return, which we've verified in VerifyType.
      body->EmitIr(this, ctx);
      if (!all_paths_jump) { ctx->more_stmts_allowed_ = true; }
    }

    ir::BasicBlock::Current = next_block;
  }

  if (node->cases_.back().first->is<ast::Expression>()) {
    phi_args[ir::BasicBlock::Current] =
        node->cases_.back().first->EmitIr(this, ctx);
    ir::UncondJump(land_block);
  } else {
    // It must be a jump/yield/return, which we've verified in VerifyType.
    node->cases_.back().first->EmitIr(this, ctx);
    if (!all_paths_jump) { ctx->more_stmts_allowed_ = true; }
  }

  ir::BasicBlock::Current = land_block;
  if (t == type::Void()) {
    return ir::Results{};
  } else {
    return ir::MakePhi(t, ir::Phi(t->is_big() ? type::Ptr(t) : t), phi_args);
  }
}

ir::Results EmitIr::Val(ast::SwitchWhen const *node, Context *ctx) const {
  UNREACHABLE();
}

ir::Results EmitIr::Val(ast::Terminal const *node, Context *ctx) const {
  if (node->type_ == type::Block) {
    ir::BlockSequence seq;
    seq.append(node->results_.get<ir::Block>(0).val_);
    return ir::Results{seq};
  }

  return node->results_;
}

ir::Results EmitIr::Val(ast::Unop const *node, Context *ctx) const {
  auto *operand_type = ctx->type_of(node->operand.get());
  if (auto const *dispatch_table = ctx->dispatch_table(node)) {
    // TODO struct is not exactly right. we really mean user-defined
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
            {std::pair(node->operand.get(), node->operand->EmitIr(this, ctx))},
            {}),
        ctx);
  }

  switch (node->op) {
    case frontend::Operator::Copy: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      CopyInit(operand_type, node->operand->EmitIr(this, ctx),
               type::Typed<ir::Reg>(reg, operand_type), ctx);
      return ir::Results{reg};
    } break;
    case frontend::Operator::Move: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      MoveInit(operand_type, node->operand->EmitIr(this, ctx),
               type::Typed<ir::Reg>(reg, operand_type), ctx);
      return ir::Results{reg};
    } break;
    case frontend::Operator::BufPtr:
      return ir::Results{ir::BufPtr(
          node->operand->EmitIr(this, ctx).get<type::Type const *>(0))};
    case frontend::Operator::Not: {
      auto *t = ctx->type_of(node->operand.get());
      if (t == type::Bool) {
        return ir::Results{
            ir::Not(node->operand->EmitIr(this, ctx).get<bool>(0))};
      } else if (t->is<type::Flags>()) {
        return ir::Results{
            ir::Not(type::Typed<ir::RegisterOr<ir::FlagsVal>, type::Flags>(
                node->operand->EmitIr(this, ctx).get<ir::FlagsVal>(0),
                &t->as<type::Flags>()))};
      } else {
        NOT_YET();
      }
    } break;
    case frontend::Operator::Sub: {
      auto operand_ir = node->operand->EmitIr(this, ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          ctx->type_of(node->operand.get()), [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Neg(operand_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::TypeOf:
      return ir::Results{ctx->type_of(node->operand.get())};
    case frontend::Operator::Which:
      return ir::Results{ir::Load<type::Type const *>(
          ir::VariantType(node->operand->EmitIr(this, ctx).get<ir::Reg>(0)))};
    case frontend::Operator::And:
      return ir::Results{node->operand->EmitLVal(this, ctx)[0]};
    case frontend::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      // TODO what if there's an error during evaluation?
      return backend::Evaluate(node->operand.get(), ctx);
    }
    case frontend::Operator::Mul:
      return ir::Results{
          ir::Ptr(node->operand->EmitIr(this, ctx).get<type::Type const *>(0))};
    case frontend::Operator::At: {
      auto *t = ctx->type_of(node);
      return ir::Results{
          ir::Load(node->operand->EmitIr(this, ctx).get<ir::Reg>(0), t)};
    }
    case frontend::Operator::Needs: {
      // TODO validate requirements are well-formed?
      ir::CompiledFn::Current->precondition_exprs_.push_back(
          node->operand.get());
      return ir::Results{};
    } break;
    case frontend::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      ir::CompiledFn::Current->postcondition_exprs_.push_back(
          node->operand.get());
      return ir::Results{};
    } break;
    case frontend::Operator::Expand: {
      ir::Results tuple_val = node->operand->EmitIr(this, ctx);
      ir::Reg tuple_reg     = tuple_val.get<ir::Reg>(0);
      type::Tuple const *tuple_type =
          &ctx->type_of(node->operand.get())->as<type::Tuple>();
      ir::Results results;
      for (size_t i = 0; i < tuple_type->size(); ++i) {
        results.append(ir::PtrFix(ir::Field(tuple_reg, tuple_type, i).get(),
                                  tuple_type->entries_[i]));
      }
      return results;
    }
    default: UNREACHABLE("Operator is ", static_cast<int>(node->op));
  }
}

}  // namespace visitor
