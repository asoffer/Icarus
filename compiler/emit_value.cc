#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/methods/dump.h"
#include "ast/scope/exec.h"
#include "backend/eval.h"
#include "backend/exec.h"
#include "base/guarded.h"
#include "compiler/executable_module.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/builtin_ir.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/components.h"
#include "ir/jump.h"
#include "ir/reg.h"
#include "type/jump.h"
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

}  // namespace ir

namespace compiler {
using ::matcher::InheritsFrom;

VerifyResult VerifyBody(Compiler *compiler, ast::FunctionLiteral const *node);
void VerifyBody(Compiler *compiler, ast::Jump const *node);

namespace {

void MakeAllStackAllocations(Compiler *compiler, ast::FnScope const *fn_scope) {
  for (auto *scope : fn_scope->descendants()) {
    for (const auto &[key, val] : scope->decls_) {
      for (auto *decl : val) {
        if (decl->flags() &
            (ast::Declaration::f_IsConst | ast::Declaration::f_IsFnParam)) {
          continue;
        }

        // TODO it's wrong to use a default BoundConstants, but it's even more
        // wrong to store the address on the declaration, so you can fix those
        // together.
        compiler->set_addr(decl,
                           compiler->builder().Alloca(compiler->type_of(decl)));
      }
    }
  }
}

void MakeAllDestructions(Compiler *compiler, ast::ExecScope const *exec_scope) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<ast::Declaration *> ordered_decls;
  for (auto &[name, decls] : exec_scope->decls_) {
    ordered_decls.insert(ordered_decls.end(), decls.begin(), decls.end());
  }

  // TODO eek, don't use line number to determine destruction order!
  absl::c_sort(ordered_decls, [](ast::Declaration *lhs, ast::Declaration *rhs) {
    return (lhs->span.begin() > rhs->span.begin());
  });

  for (auto *decl : ordered_decls) {
    auto *t = ASSERT_NOT_NULL(compiler->type_of(decl));
    if (not t->HasDestructor()) { continue; }
    compiler->Visit(t, compiler->addr(decl), EmitDestroyTag{});
  }
}

void EmitIrForStatements(Compiler *compiler,
                         base::PtrSpan<ast::Node const> span) {
  ICARUS_SCOPE(ir::SetTemporaries(compiler->builder())) {
    for (auto *stmt : span) {
      compiler->Visit(stmt, EmitValueTag{});
      compiler->builder().FinishTemporariesWith(
          [compiler](type::Typed<ir::Reg> r) {
            compiler->Visit(r.type(), r.get(), EmitDestroyTag{});
          });
      if (compiler->builder().more_stmts_allowed()) { break; };
    }
  }
}

void CompleteBody(Compiler *compiler, ast::FunctionLiteral const *node) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = compiler->type_of(node);

  ir::CompiledFn *&ir_func = compiler->data_.constants_->second.ir_funcs_[node];

  ICARUS_SCOPE(ir::SetCurrentFunc(ir_func)) {
    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(compiler, node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        auto *out_decl_type = ASSERT_NOT_NULL(compiler->type_of(out_decl));
        auto alloc          = out_decl_type->is_big()
                         ? ir::GetRet(i, out_decl_type)
                         : compiler->builder().Alloca(out_decl_type);

        compiler->set_addr(out_decl, alloc);
        if (out_decl->IsDefaultInitialized()) {
          compiler->Visit(out_decl_type, alloc, EmitDefaultInitTag{});
        } else {
          compiler->Visit(
              out_decl_type, alloc,
              type::Typed{compiler->Visit(out_decl->init_val(), EmitValueTag{}),
                          out_decl_type},
              EmitCopyAssignTag{});
        }
      }
    }

    ICARUS_SCOPE(ir::SetTemporaries(compiler->builder())) {
      for (auto const *stmt : node->stmts()) {
        compiler->Visit(stmt, EmitValueTag{});

        compiler->builder().FinishTemporariesWith(
            [compiler](type::Typed<ir::Reg> r) {
              compiler->Visit(r.type(), r.get(), EmitDestroyTag{});
            });
      }
    }

    MakeAllDestructions(compiler, node->body_scope());

    if (t->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      compiler->builder().ReturnJump();
    }

    ir_func->work_item = nullptr;
  }
}

void CompleteBody(Compiler *compiler, ast::StructLiteral const *node) {
  NOT_YET();
  //   ir::CompiledFn *&ir_func = data_.constants_->second.ir_funcs_[node];
  //   for (size_t i = 0; i < node->args_.size(); ++i) {
  //     set_addr(&node->args_[i], ir::Reg::Arg(i));
  //   }
  //
  //   ICARUS_SCOPE(ir::SetCurrentFunc(ir_func)) {
  //     ir::GetBuilder().CurrentBlock() = ir_func->entry();
  //     auto cache_slot_addr    = ir::ArgumentCache(node);
  //     auto cache_slot         = ir::Load<type::Type const
  //     *>(cache_slot_addr);
  //
  //     auto land_block         = builder().AddBlock();
  //     ir::GetBuilder().CurrentBlock() = ir::EarlyExitOn<false>(
  //         land_block,
  //         ir::Eq(cache_slot, static_cast<type::Type const *>(nullptr)));
  //     auto ctx_reg    = ir::CreateContext(module());
  //     auto struct_reg = ir::CreateStruct(node->scope_, node);
  //
  //     // TODO why isn't implicit TypedRegister -> RegOr cast working on
  //     // either of these? On the first it's clear because we don't even
  //     return
  //     a
  //     // typedRegister, but this is a note to remind you to make that work.
  //     On the
  //     // second... I don't know.
  //     ir::Store(static_cast<ir::RegOr<type::Type const *>>(struct_reg),
  //               cache_slot_addr);
  //     for (auto &arg : node->args_) {  // TODO const-ref
  //       ir::AddBoundConstant(ctx_reg, &arg, addr(&arg));
  //     }
  //
  //     for (auto &field : node->fields_) {  // TODO const-ref
  //       ir::VerifyType(&field, ctx_reg);
  //
  //       // TODO exit early if verifytype fails.
  //
  //       auto type_reg = ir::EvaluateAsType(field.type_expr(), ctx_reg);
  //
  //       ir::CreateStructField(struct_reg, type_reg);
  //       ir::SetStructFieldName(struct_reg, field.id());
  //
  //       // for (auto const &hashtag : field.hashtags_) {
  //       //   ir::AddHashtagToField(struct_reg, hashtag);
  //       // }
  //     }
  //
  //     // for (auto hashtag : node->hashtags_) {
  //     //   ir::AddHashtagToStruct(struct_reg, hashtag);
  //     // }
  //
  //     ir::RegOr<type::Type const *> result = ir::FinalizeStruct(struct_reg);
  //     ir::DestroyContext(ctx_reg);
  //
  //     // Exit path from creating a new struct.
  //     ir::SetRet(0, static_cast<ir::RegOr<type::Type const *>>(result));
  //     ir::Store(static_cast<ir::RegOr<type::Type const *>>(result),
  //               cache_slot_addr);
  //     compiler->builder().ReturnJump();
  //
  //     // Exit path from finding the cache
  //     ir::GetBuilder().CurrentBlock() = land_block;
  //     ir::SetRet(0, static_cast<ir::RegOr<type::Type const *>>(cache_slot));
  //     compiler->builder().ReturnJump();
  //   }
}

void CompleteBody(Compiler *compiler, ast::Jump const *node) {
  ir::CompiledFn *&ir_func = compiler->data_.constants_->second.ir_funcs_[node];

  ICARUS_SCOPE(ir::SetCurrentFunc(ir_func)) {
    compiler->builder().CurrentBlock() = ir_func->entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    for (int32_t i = 0; i < static_cast<int32_t>(node->input().size()); ++i) {
      compiler->set_addr(node->input()[i], ir::Reg::Arg(i));
    }

    MakeAllStackAllocations(compiler, node->body_scope());

    ICARUS_SCOPE(ir::SetTemporaries(compiler->builder())) {
      for (auto *stmt : node->stmts()) {
        compiler->Visit(stmt, EmitValueTag{});

        compiler->builder().FinishTemporariesWith(
            [compiler](type::Typed<ir::Reg> r) {
              compiler->Visit(r.type(), r.get(), EmitDestroyTag{});
            });
      }
    }

    MakeAllDestructions(compiler, node->body_scope());

    compiler->builder().ReturnJump();
    ir_func->work_item = nullptr;
  }
}

template <typename NodeType>
base::move_func<void()> *DeferBody(Compiler *compiler, NodeType const *node) {
  // It's safe to capture `compiler` because we know this lambda will be
  // executed as part of work deferral of `compiler` before the compiler is
  // destroyed.
  return compiler->AddWork(node, [compiler, node]() mutable {
    if constexpr (std::is_same_v<NodeType, ast::FunctionLiteral> or
                  std::is_same_v<NodeType, ast::Jump>) {
      VerifyBody(compiler, node);
    }
    CompleteBody(compiler, node);
  });
}

}  // namespace

std::unique_ptr<module::BasicModule> CompileExecutableModule(
    frontend::Source *src) {
  auto mod = std::make_unique<ExecutableModule>(
      [](base::PtrSpan<ast::Node const> nodes, CompiledModule *mod) {
        // TODO remove reinterpret_cast
        auto exec_mod = reinterpret_cast<ExecutableModule *>(mod);
        compiler::Compiler c(mod);

        // Do one pass of verification over constant declarations. Then come
        // back a second time to handle the remaining.
        // TODO this may be necessary in library modules too.
        std::vector <ast::Node const*> deferred;
        for (ast::Node const *node : nodes) {
          if (auto const *decl = node->if_as<ast::Declaration>()) {
            if (decl->flags() & ast::Declaration::f_IsConst) {
              c.Visit(decl, VerifyTypeTag{});
              continue;
            }
          }
          deferred.push_back(node);
        }

        for (ast::Node const *node : deferred) {
          c.Visit(node, VerifyTypeTag{});
        }
        if (c.num_errors() > 0) { return; }

        ast::ModuleScope *mod_scope =
            &nodes.front()->scope_->as<ast::ModuleScope>();
        ICARUS_SCOPE(ir::SetCurrentFunc(exec_mod->main(), &c.builder())) {
          MakeAllStackAllocations(&c, mod_scope);
          ICARUS_SCOPE(ir::SetTemporaries(c.builder())) {
            for (auto const *stmt : nodes) {
              c.Visit(stmt, EmitValueTag{});
              c.builder().FinishTemporariesWith([&](type::Typed<ir::Reg> r) {
                c.Visit(r.type(), r.get(), EmitDestroyTag{});
              });
            }
          }

          MakeAllDestructions(&c, mod_scope);
          // TODO determine under which scenarios destructors can be skipped.

          c.builder().ReturnJump();
        }

        c.CompleteDeferredBodies();

        mod->dep_data_   = std::move(c.data_.dep_data_);
        mod->fns_        = std::move(c.data_.fns_);
        mod->scope_defs_ = std::move(c.data_.scope_defs_);
        mod->block_defs_ = std::move(c.data_.block_defs_);
      });
  mod->Process(frontend::Parse(src));
  return mod;
}



ir::Results Compiler::Visit(ast::Access const *node, EmitValueTag) {
  if (type_of(node->operand()) == type::Module) {
    // TODO we already did this evaluation in type verification. Can't we just
    // save and reuse it?
    auto decls = backend::EvaluateAs<module::BasicModule const *>(
                     MakeThunk(node->operand(), type::Module))
                     ->declarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1: return Visit(decls[0], EmitValueTag{});
      default: NOT_YET();
    }
  }

  auto *this_type = type_of(node);
  if (this_type->is<type::Enum>()) {
    auto lit = this_type->as<type::Enum>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else {
    auto reg = Visit(node->operand(), EmitRefTag{})[0];
    auto *t  = type_of(node->operand());

    if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
    while (auto *p = t->if_as<type::Pointer>()) {
      t   = p->pointee;
      reg = ir::Load<ir::Addr>(reg);
    }

    ASSERT(t, InheritsFrom<type::Struct>());
    auto *struct_type = &t->as<type::Struct>();
    auto field =
        ir::Field(reg, struct_type, struct_type->index(node->member_name()));
    return ir::Results{ir::PtrFix(field.get(), this_type)};
  }
}

ir::Results Compiler::Visit(ast::ArrayLiteral const *node, EmitValueTag) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = type_of(node);
  auto alloc      = builder().TmpAlloca(this_type);
  if (not node->empty()) {
    auto *data_type = this_type->as<type::Array>().data_type;
    for (size_t i = 0; i < node->size(); ++i) {
      EmitMoveInit(data_type, Visit(node->elem(i), EmitValueTag{}),
                   type::Typed<ir::Reg>(ir::Index(type::Ptr(this_type), alloc,
                                                  static_cast<int32_t>(i)),
                                        type::Ptr(data_type)));
    }
  }
  return ir::Results{alloc};
}

ir::Results Compiler::Visit(ast::ArrayType const *node, EmitValueTag) {
  auto result =
      Visit(node->data_type(), EmitValueTag{}).get<type::Type const *>(0);
  // Size must be at least 1 by construction, so `.size() - 1` will not
  // overflow.
  for (int i = node->lengths().size() - 1; i >= 0; --i) {
    result = ir::Array(Visit(node->length(i), EmitValueTag{}).get<int64_t>(0),
                       result);
  }
  return ir::Results{result};
}

ir::Results Compiler::Visit(ast::Binop const *node, EmitValueTag) {
  auto *lhs_type = type_of(node->lhs());
  auto *rhs_type = type_of(node->rhs());

  if (auto *table = dispatch_table(node)) {
    // TODO struct is not exactly right. we really mean user-defined
    return table->EmitCall(
        this, core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                  {std::pair(node->lhs(), Visit(node->lhs(), EmitValueTag{})),
                   std::pair(node->rhs(), Visit(node->rhs(), EmitValueTag{}))},
                  {}));
  }

  switch (node->op()) {
    case frontend::Operator::Add: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{ir::Add(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Sub: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{ir::Sub(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{ir::Mul(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{ir::Div(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{ir::Mod(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Arrow: {
      // TODO ugly hack.
      std::vector<ir::RegOr<type::Type const *>> lhs_vals, rhs_vals;
      if (auto *l = node->lhs()->if_as<ast::CommaList>()) {
        for (auto &e : l->exprs_) {
          lhs_vals.push_back(
              Visit(e.get(), EmitValueTag{}).get<type::Type const *>(0));
        }
      } else {
        lhs_vals.push_back(
            Visit(node->lhs(), EmitValueTag{}).get<type::Type const *>(0));
      }
      if (auto *r = node->rhs()->if_as<ast::CommaList>()) {
        for (auto &e : r->exprs_) {
          rhs_vals.push_back(
              Visit(e.get(), EmitValueTag{}).get<type::Type const *>(0));
        }
      } else {
        rhs_vals.push_back(
            Visit(node->rhs(), EmitValueTag{}).get<type::Type const *>(0));
      }

      return ir::Results{ir::Arrow(lhs_vals, rhs_vals)};
    } break;
    case frontend::Operator::Assign: {
      // TODO support splatting.
      auto lhs_lvals = Visit(node->lhs(), EmitRefTag{});
      if (lhs_lvals.size() != 1) { NOT_YET(); }

      auto rhs_vals = Visit(node->rhs(), EmitValueTag{});
      Visit(lhs_type, lhs_lvals[0], type::Typed{rhs_vals, rhs_type},
            EmitMoveAssignTag{});

      return ir::Results{};
    } break;
    case frontend::Operator::OrEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
        ir::Store(ir::OrFlags(
                      ir::Load<ir::FlagsVal>(lhs_lval),
                      Visit(node->rhs(), EmitValueTag{}).get<ir::FlagsVal>(0)),
                  lhs_lval);
        return ir::Results{};
      }
      auto *land_block = builder().AddBlock();
      auto *more_block = builder().AddBlock();

      auto lhs_val       = Visit(node->lhs(), EmitValueTag{}).get<bool>(0);
      auto lhs_end_block = builder().CurrentBlock();
      builder().CondJump(lhs_val, land_block, more_block);

      builder().CurrentBlock() = more_block;
      auto rhs_val       = Visit(node->rhs(), EmitValueTag{}).get<bool>(0);
      auto rhs_end_block = builder().CurrentBlock();
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      return ir::Results{ir::Phi<bool>({lhs_end_block, rhs_end_block},
                                       {ir::RegOr<bool>(true), rhs_val})};
    } break;
    case frontend::Operator::AndEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
        ir::Store(ir::AndFlags(
                      ir::Load<ir::FlagsVal>(lhs_lval),
                      Visit(node->rhs(), EmitValueTag{}).get<ir::FlagsVal>(0)),
                  lhs_lval);
        return ir::Results{};
      }

      auto *land_block = builder().AddBlock();
      auto *more_block = builder().AddBlock();

      auto lhs_val       = Visit(node->lhs(), EmitValueTag{}).get<bool>(0);
      auto lhs_end_block = builder().CurrentBlock();
      builder().CondJump(lhs_val, more_block, land_block);

      builder().CurrentBlock() = more_block;
      auto rhs_val       = Visit(node->rhs(), EmitValueTag{}).get<bool>(0);
      auto rhs_end_block = builder().CurrentBlock();
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      // TODO this looks like a bug.
      return ir::Results{ir::Phi<bool>({lhs_end_block, rhs_end_block},
                                       {rhs_val, ir::RegOr<bool>(false)})};
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
      auto rhs_ir   = Visit(node->rhs(), EmitValueTag{});
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            ir::Store(ir::Add(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
      auto rhs_ir   = Visit(node->rhs(), EmitValueTag{});
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            ir::Store(ir::Sub(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
      auto rhs_ir   = Visit(node->rhs(), EmitValueTag{});
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
      auto rhs_ir   = Visit(node->rhs(), EmitValueTag{});
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t>(rhs_type, [&](auto tag) {
        using T = typename decltype(tag)::type;
        ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)), lhs_lval);
      });
      return ir::Results{};
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
      auto rhs_ir   = Visit(node->rhs(), EmitValueTag{});
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            ir::Store(ir::Mul(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
        auto rhs_ir   = Visit(node->rhs(), EmitValueTag{}).get<bool>(0);
        ir::Store(ir::Ne(ir::Load<bool>(lhs_lval), rhs_ir), lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = Visit(node->lhs(), EmitRefTag{})[0];
        auto rhs_ir = Visit(node->rhs(), EmitValueTag{}).get<ir::FlagsVal>(0);
        ir::Store(ir::XorFlags(ir::Load<ir::FlagsVal>(lhs_lval), rhs_ir),
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

ir::Results Compiler::Visit(ast::BlockLiteral const *node, EmitValueTag) {
  std::vector<ir::RegOr<ir::AnyFunc>> befores;
  std::vector<ir::RegOr<ir::Jump const *>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(Visit(decl, EmitValueTag{}).get<ir::AnyFunc>(0));
  }

  for (auto const &decl : node->after()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    afters.push_back(
        Visit(decl, EmitValueTag{}).get<ir::Jump const *>(0));
  }

  return ir::Results{ir::BlockHandler(data_.add_block(), befores, afters)};
}

template <typename T>
struct PushVec : public base::UseWithScope {
  template <typename... Args>
  PushVec(std::vector<T> *vec, Args &&... args) : vec_(vec) {
    vec_->emplace_back(std::forward<Args>(args)...);
  }

  ~PushVec() { vec_->pop_back(); }

 private:
  std::vector<T> *vec_;
};
template <typename T, typename... Args>
PushVec(std::vector<T> *, Args &&...)->PushVec<T>;

ir::Results Compiler::Visit(ast::BlockNode const *node, EmitValueTag) {
  ICARUS_SCOPE(PushVec(&data_.yields_stack_)) {
    EmitIrForStatements(this, node->stmts());

    //   // TODO yield args can just be this pair type, making this conversion
    //   // unnecessary.
    //   std::vector<std::pair<ast::Expression const *, ir::Results>>
    //   yield_args; for (auto &arg : data_.yields_stack_.back()) {
    //     yield_args.emplace_back(arg.expr_, arg.value());
    //   }
    //
    //   // TODO this is tricky. We can easily destroy parameters that we're
    //   trying to
    //   // pass to the next scope. Need to really treat these like function
    //   args and
    //   // destroy them at the end of the inline call.
    //   MakeAllDestructions(this, node->body_scope());
    //
    //   ASSERT_NOT_NULL(jump_table(node, ""))
    //       ->EmitInlineCall(
    //           core::FnArgs<std::pair<ast::Expression const *, ir::Results>>{
    //               std::move(yield_args), {}},
    //           *block_map, ctx);
    //
    NOT_YET();
  }
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::BuiltinFn const *node, EmitValueTag) {
  return ir::Results{node->value()};
}

ir::Results Compiler::Visit(ast::Call const *node, EmitValueTag) {
  auto *b = node->callee()->if_as<ast::BuiltinFn>();
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    switch (b->value()) {
      case core::Builtin::Foreign: {
        auto name = backend::EvaluateAs<std::string_view>(
            MakeThunk(node->args().at(0), type::ByteView));
        auto *foreign_type = backend::EvaluateAs<type::Type const *>(
            MakeThunk(node->args().at(1), type::Type_));
        return ir::Results{ir::LoadSymbol(name, foreign_type).get()};
      } break;

      case core::Builtin::Opaque: return ir::Results{ir::OpaqueType(module())};
      case core::Builtin::Bytes: {
        auto const &fn_type =
            ir::BuiltinType(core::Builtin::Bytes)->as<type::Function>();
        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        builder().Call(ir::BytesFn(), &fn_type,
                       {Visit(node->args().at(0), EmitValueTag{})}, outs);

        return ir::Results{reg};
      } break;

      case core::Builtin::Alignment: {
        auto const &fn_type =
            ir::BuiltinType(core::Builtin::Alignment)->as<type::Function>();
        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        builder().Call(ir::AlignmentFn(), &fn_type,
                       {Visit(node->args().at(0), EmitValueTag{})}, outs);

        return ir::Results{reg};
      } break;

#if defined(ICARUS_DEBUG)
      case core::Builtin::DebugIr: builder().DebugIr(); return ir::Results{};
#endif  // defined(ICARUS_DEBUG)
    }
    UNREACHABLE();
  }

  auto const &table = *ASSERT_NOT_NULL(data_.dispatch_table(node));
  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(Visit(expr, EmitValueTag{}), type_of(expr));
  });

  return table.EmitCall(this, args);
  // TODO    node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}

ir::Results Compiler::Visit(ast::Cast const *node, EmitValueTag) {
  if (auto *table = dispatch_table(node)) {
    return table->EmitCall(
        this,
        core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
            {std::pair(node->expr(), Visit(node->expr(), EmitValueTag{})),
             std::pair(node->type(), Visit(node->type(), EmitValueTag{}))},
            {}));
  }

  auto *to_type = ASSERT_NOT_NULL(type_of(node));
  auto results  = Visit(node->expr(), EmitValueTag{});
  if (to_type == type::Type_) {
    std::vector<type::Type const *> entries;
    entries.reserve(results.size());
    for (size_t i = 0; i < results.size(); ++i) {
      // TODO what about incomplete structs?
      entries.push_back(results.get<type::Type const *>(i).value());
    }
    return ir::Results{type::Tup(entries)};
  }
  auto *from_type = type_of(node->expr());
  // TODO enum, flags, ptrs?
  return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                          uint32_t, uint64_t, float, double>(
      to_type, [&](auto tag) {
        return ir::Results{
            ir::CastTo<typename decltype(tag)::type>(from_type, results)};
      });
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
ir::Results ArrayCompare(Compiler *compiler, type::Array const *lhs_type,
                         ir::Results const &lhs_ir, type::Array const *rhs_type,
                         ir::Results const &rhs_ir, bool equality) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto [iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto const *fn_type =
        type::Func({type::Ptr(lhs_type), type::Ptr(rhs_type)}, {type::Bool});
    auto *fn = compiler->AddFunc(fn_type, fn_type->AnonymousFnParams());

    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      compiler->builder().CurrentBlock() = fn->entry();

      auto *equal_len_block = compiler->builder().AddBlock();
      auto *true_block      = compiler->builder().AddBlock();
      auto *false_block     = compiler->builder().AddBlock();
      auto *phi_block       = compiler->builder().AddBlock();
      auto *body_block      = compiler->builder().AddBlock();
      auto *incr_block      = compiler->builder().AddBlock();

      compiler->builder().CondJump(ir::Eq(lhs_type->len, rhs_type->len),
                                   equal_len_block, false_block);

      compiler->builder().CurrentBlock() = true_block;
      ir::SetRet(0, true);
      compiler->builder().ReturnJump();

      compiler->builder().CurrentBlock() = false_block;
      ir::SetRet(0, false);
      compiler->builder().ReturnJump();

      compiler->builder().CurrentBlock() = equal_len_block;
      auto lhs_start = ir::Index(Ptr(lhs_type), ir::Reg::Arg(0), 0);
      auto rhs_start = ir::Index(Ptr(rhs_type), ir::Reg::Arg(1), 0);
      auto lhs_end =
          ir::PtrIncr(lhs_start, lhs_type->len, Ptr(rhs_type->data_type));
      compiler->builder().UncondJump(phi_block);

      compiler->builder().CurrentBlock() = phi_block;

      ir::Reg lhs_phi_reg = ir::MakeResult<ir::Addr>();
      ir::Reg rhs_phi_reg = ir::MakeResult<ir::Addr>();

      compiler->builder().CondJump(
          ir::Eq(ir::RegOr<ir::Addr>(lhs_phi_reg), lhs_end), true_block,
          body_block);

      compiler->builder().CurrentBlock() = body_block;
      // TODO what if data type is an array?
      compiler->builder().CondJump(ir::Eq(ir::Load<ir::Addr>(lhs_phi_reg),
                                          ir::Load<ir::Addr>(rhs_phi_reg)),
                                   incr_block, false_block);

      compiler->builder().CurrentBlock() = incr_block;
      auto lhs_incr = ir::PtrIncr(lhs_phi_reg, 1, Ptr(lhs_type->data_type));
      auto rhs_incr = ir::PtrIncr(rhs_phi_reg, 1, Ptr(rhs_type->data_type));
      compiler->builder().UncondJump(phi_block);

      ir::Phi<ir::Addr>(lhs_phi_reg, {equal_len_block, incr_block},
                        {lhs_start, lhs_incr});
      ir::Phi<ir::Addr>(rhs_phi_reg, {equal_len_block, incr_block},
                        {rhs_start, rhs_incr});
    }
  }

  ir::OutParams outs;
  auto result = outs.AppendReg(type::Bool);

  compiler->builder().Call(ir::AnyFunc{iter->second}, iter->second->type_,
                           {lhs_ir, rhs_ir}, std::move(outs));
  return ir::Results{result};
}

static ir::RegOr<bool> EmitChainOpPair(Compiler *compiler,
                                       ast::ChainOp const *chain_op,
                                       size_t index, ir::Results const &lhs_ir,
                                       ir::Results const &rhs_ir) {
  auto *lhs_type = compiler->type_of(chain_op->exprs()[index]);
  auto *rhs_type = compiler->type_of(chain_op->exprs()[index + 1]);
  auto op        = chain_op->ops()[index];

  if (lhs_type->is<type::Array>() and rhs_type->is<type::Array>()) {
    using ::matcher::Eq;
    ASSERT(op, Eq(frontend::Operator::Eq) or Eq(frontend::Operator::Ne));
    return ArrayCompare(compiler, &lhs_type->as<type::Array>(), lhs_ir,
                        &rhs_type->as<type::Array>(), rhs_ir,
                        op == frontend::Operator::Eq)
        .get<bool>(0);
  } else if (lhs_type->is<type::Struct>() or rhs_type->is<type::Struct>()) {
    auto results =
        ASSERT_NOT_NULL(
            compiler->dispatch_table(reinterpret_cast<ast::Expression *>(
                reinterpret_cast<uintptr_t>(chain_op->exprs()[index]) | 0x1)))
            ->EmitCall(
                compiler,
                core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                    {std::pair(chain_op->exprs()[index], lhs_ir),
                     std::pair(chain_op->exprs()[index + 1], rhs_ir)},
                    {}));
    ASSERT(results.size() == 1u);
    return results.get<bool>(0);

  } else {
    switch (op) {
      case frontend::Operator::Lt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return ir::Lt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return ir::Le(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Eq:
        if (lhs_type == type::Block) {
          auto val1 = lhs_ir.get<ir::BlockDef *>(0);
          auto val2 = rhs_ir.get<ir::BlockDef *>(0);
          if (not val1.is_reg() and not val2.is_reg()) {
            return val1.value() == val2.value();
          }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto tag) {
              using T = typename decltype(tag)::type;
              return ir::Eq(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ne:
        if (lhs_type == type::Block) {
          auto val1 = lhs_ir.get<ir::BlockDef *>(0);
          auto val2 = rhs_ir.get<ir::BlockDef *>(0);
          if (not val1.is_reg() and not val2.is_reg()) {
            return val1.value() == val2.value();
          }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto tag) {
              using T = typename decltype(tag)::type;
              return ir::Ne(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return ir::Ge(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return ir::Gt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
        // TODO case frontend::Operator::And: cmp = lhs_ir; break;
      default: UNREACHABLE();
    }
  }
}

ir::Results Compiler::Visit(ast::ChainOp const *node, EmitValueTag) {
  auto *t = type_of(node);
  if (node->ops()[0] == frontend::Operator::Xor) {
    if (t == type::Bool) {
      return ir::Results{std::accumulate(
          node->exprs().begin(), node->exprs().end(), ir::RegOr<bool>(false),
          [&](ir::RegOr<bool> acc, auto *expr) {
            return ir::Ne(acc,
                          Visit(expr, EmitValueTag{}).template get<bool>(0));
          })};
    } else if (t->is<type::Flags>()) {
      return ir::Results{std::accumulate(
          node->exprs().begin(), node->exprs().end(),
          ir::RegOr<ir::FlagsVal>(ir::FlagsVal{0}),
          [&](ir::RegOr<ir::FlagsVal> acc, auto *expr) {
            return ir::XorFlags(
                acc, Visit(expr, EmitValueTag{}).template get<ir::FlagsVal>(0));
          })};
    } else {
      UNREACHABLE();
    }

  } else if (node->ops()[0] == frontend::Operator::Or and
             t->is<type::Flags>()) {
    auto iter = node->exprs().begin();
    auto val  = Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0);
    while (++iter != node->exprs().end()) {
      val = ir::OrFlags(val, Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops()[0] == frontend::Operator::And and
             t->is<type::Flags>()) {
    auto iter = node->exprs().begin();
    auto val  = Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0);
    while (++iter != node->exprs().end()) {
      val =
          ir::AndFlags(val, Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops()[0] == frontend::Operator::Or and t == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<ir::RegOr<type::Type const *>> args;
    args.reserve(node->exprs().size());
    for (auto const *expr : node->exprs()) {
      args.push_back(Visit(expr, EmitValueTag{}).get<type::Type const *>(0));
    }
    auto reg_or_type = ir::Var(args);
    return ir::Results{reg_or_type};
  } else if (node->ops()[0] == frontend::Operator::Or and t == type::Block) {
    NOT_YET();
  } else if (node->ops()[0] == frontend::Operator::And or
             node->ops()[0] == frontend::Operator::Or) {
    auto *land_block = builder().AddBlock();

    std::vector<ir::BasicBlock *> phi_blocks;
    std::vector<ir::RegOr<bool>> phi_results;
    bool is_or = (node->ops()[0] == frontend::Operator::Or);
    for (size_t i = 0; i + 1 < node->exprs().size(); ++i) {
      auto val = Visit(node->exprs()[i], EmitValueTag{}).get<bool>(0);

      auto *next_block = builder().AddBlock();
      builder().CondJump(val, is_or ? land_block : next_block,
                         is_or ? next_block : land_block);
      phi_blocks.push_back(builder().CurrentBlock());
      phi_results.push_back(is_or);

      builder().CurrentBlock() = next_block;
    }

    phi_blocks.push_back(builder().CurrentBlock());
    phi_results.push_back(
        Visit(node->exprs().back(), EmitValueTag{}).get<bool>(0));
    builder().UncondJump(land_block);

    builder().CurrentBlock() = land_block;

    return ir::Results{ir::Phi<bool>(phi_blocks, phi_results)};

  } else {
    if (node->ops().size() == 1) {
      auto lhs_ir = Visit(node->exprs()[0], EmitValueTag{});
      auto rhs_ir = Visit(node->exprs()[1], EmitValueTag{});
      return ir::Results{EmitChainOpPair(this, node, 0, lhs_ir, rhs_ir)};

    } else {
      std::vector<ir::BasicBlock *> phi_blocks;
      std::vector<ir::RegOr<bool>> phi_values;
      auto lhs_ir      = Visit(node->exprs().front(), EmitValueTag{});
      auto *land_block = builder().AddBlock();
      for (size_t i = 0; i + 1 < node->ops().size(); ++i) {
        auto rhs_ir = Visit(node->exprs()[i + 1], EmitValueTag{});
        auto cmp    = EmitChainOpPair(this, node, i, lhs_ir, rhs_ir);

        phi_blocks.push_back(builder().CurrentBlock());
        phi_values.push_back(false);
        auto *next_block = builder().AddBlock();
        builder().CondJump(cmp, next_block, land_block);
        builder().CurrentBlock() = next_block;
        lhs_ir                   = std::move(rhs_ir);
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = Visit(node->exprs().back(), EmitValueTag{});
      phi_blocks.push_back(builder().CurrentBlock());
      phi_values.push_back(EmitChainOpPair(this, node, node->exprs().size() - 2,
                                           lhs_ir, rhs_ir));
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      return ir::Results{ir::Phi<bool>(phi_blocks, phi_values)};
    }
  }
  UNREACHABLE();
}

ir::Results Compiler::Visit(ast::CommaList const *node, EmitValueTag) {
  auto *tuple_type = &type_of(node)->as<type::Tuple>();
  // TODO this is a hack. I'm still not sure what counts as a tuple and what
  // counts as atype
  if (tuple_type->entries_.empty()) { return ir::Results{type::Tup({})}; }

  auto tuple_alloc = builder().TmpAlloca(tuple_type);

  size_t index = 0;
  for (auto &expr : node->exprs_) {
    if (expr->needs_expansion()) {
      auto results = Visit(expr.get(), EmitValueTag{});
      for (size_t i = 0; i < results.size(); ++i) {
        EmitCopyInit(tuple_type->entries_[index], results.GetResult(i),
                     ir::Field(tuple_alloc, tuple_type, index));
        ++index;
      }
    } else {
      EmitCopyInit(tuple_type->entries_[index], Visit(expr.get(), EmitValueTag{}),
                   ir::Field(tuple_alloc, tuple_type, index));
      ++index;
    }
  }
  return ir::Results{tuple_alloc};
}

ir::Results Compiler::Visit(ast::Declaration const *node, EmitValueTag) {
  // TODO swap contexts?
  if (node->flags() & ast::Declaration::f_IsConst) {
    // TODO
    if (node->flags() & ast::Declaration::f_IsFnParam) {
      if (auto result = data_.current_constants_.get_constant(node);
          not result.empty()) {
        return result;
      } else if (auto result = data_.constants_->first.get_constant(node);
                 not result.empty()) {
        return result;
      } else {
        UNREACHABLE();
      }
    } else {
      auto *t = type_of(node);
      if (not t) {
        DEBUG_LOG()(ast::Dump::ToString(node));
        UNREACHABLE();
      }

      auto slot = data_.constants_->second.constants_.reserve_slot(node, t);
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
        base::untyped_buffer buf =
            backend::EvaluateToBuffer(MakeThunk(node->init_val(), t));
        if (num_errors() > 0u) { return ir::Results{}; }
        return data_.constants_->second.constants_.set_slot(data_offset, buf.raw(0),
                                                      num_bytes);
      } else if (node->IsDefaultInitialized()) {
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
    }
    UNREACHABLE(ast::Dump::ToString(node));
  } else {
    // TODO these checks actually overlap and could be simplified.
    if (node->IsUninitialized()) { return ir::Results{}; }
    auto *t = type_of(node);
    auto a  = addr(node);
    if (node->IsCustomInitialized()) {
      Visit(node->init_val(), type::Typed(a, type::Ptr(t)), EmitMoveInitTag{});
    } else {
      if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
        Visit(t, a, EmitDefaultInitTag{});
      }
    }
    return ir::Results{a};
  }
  UNREACHABLE();
}

ir::Results Compiler::Visit(ast::EnumLiteral const *node, EmitValueTag) {
  using enum_t = uint64_t;
  std::vector<std::string_view> names;
  absl::flat_hash_map<uint64_t, ir::RegOr<enum_t>> specified_values;

  for (auto const *elem : node->elems()) {
    if (auto *id = elem->if_as<ast::Identifier>()) {
      names.push_back(id->token());
    } else if (auto *decl = elem->if_as<ast::Declaration>()) {
      names.push_back(decl->id());
      if (not decl->IsCustomInitialized()) {
        specified_values.emplace(
            names.size() - 1,
            Visit(decl->init_val(), EmitValueTag{}).get<enum_t>(0));
      }
    }
  }

  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum:
      return ir::Results{ir::Enum(module(), names, specified_values)};
    case ast::EnumLiteral::Kind::Flags:
      return ir::Results{ir::Flags(module(), names, specified_values)};
    default: UNREACHABLE();
  }
}

ir::Results Compiler::Visit(ast::FunctionLiteral const *node, EmitValueTag) {
  for (auto const &param : node->params()) {
    auto *p = param.value.get();
    if ((p->flags() & ast::Declaration::f_IsConst) and
        not data_.constants_->first.contains(p)) {
      return ir::Results{node};
    }

    for (auto *dep : node->param_dep_graph_.sink_deps(param.value.get())) {
      if (not data_.constants_->first.contains(dep)) { return ir::Results{node}; }
    }
  }

  // TODO Use correct constants
  ir::CompiledFn *&ir_func = data_.constants_->second.ir_funcs_[node];
  if (not ir_func) {
    auto *work_item_ptr = DeferBody(this, node);

    auto *fn_type = &type_of(node)->as<type::Function>();

    ir_func = AddFunc(
        fn_type, node->params().Transform(
                     [fn_type, i = 0](
                         std::unique_ptr<ast::Declaration> const &d) mutable {
                       return type::Typed<ast::Declaration const *>(
                           d.get(), fn_type->input.at(i++));
                     }));
    if (work_item_ptr) { ir_func->work_item = work_item_ptr; }
  }

  return ir::Results{ir_func};
}

ir::Results Compiler::Visit(ast::Identifier const *node, EmitValueTag) {
  ASSERT(node->decl() != nullptr) << ast::Dump::ToString(node);
  if (node->decl()->flags() & ast::Declaration::f_IsConst) {
    return Visit(node->decl(), EmitValueTag{});
  }
  if (node->decl()->flags() & ast::Declaration::f_IsFnParam) {
    auto *t     = type_of(node);
    ir::Reg reg = addr(node->decl());
    if (data_.inline_) {
      ir::Results reg_results = (*data_.inline_)[reg];
      if (not reg_results.is_reg(0)) { return reg_results; }
      reg = reg_results.get<ir::Reg>(0);
    }

    return ir::Results{
        (node->decl()->flags() & ast::Declaration::f_IsOutput) and
                not t->is_big()
            ? ir::Load(reg, t)
            : reg};
  } else {
    auto *t   = ASSERT_NOT_NULL(type_of(node));
    auto lval = Visit(node, EmitRefTag{})[0];
    if (not lval.is_reg()) { NOT_YET(); }
    return ir::Results{ir::PtrFix(lval.reg(), t)};
  }
}

ir::Results Compiler::Visit(ast::Import const *node, EmitValueTag) {
  return ir::Results{ASSERT_NOT_NULL(pending_module(node))->get()};
}

ir::Results Compiler::Visit(ast::Index const *node, EmitValueTag) {
  return ir::Results{
      ir::PtrFix(Visit(node, EmitRefTag{})[0].reg(), type_of(node))};
}

ir::Results Compiler::Visit(ast::Goto const *node, EmitValueTag) {
  std::vector<std::string_view> names;
  names.reserve(node->options().size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(node->options().size());
  auto current_block = builder().CurrentBlock();

  for (auto const &opt : node->options()) {
    ir::BasicBlock *block = builder().AddBlock();
    blocks.push_back(block);
    names.push_back(opt.block());

    builder().CurrentBlock() = block;
    // TODO emit code for each possible jumped-to block
  }

  builder().CurrentBlock() = current_block;
  builder().ChooseJump(names, blocks);
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::Jump const *node, EmitValueTag) {
  ir::CompiledFn *&ir_func = data_.constants_->second.ir_funcs_[node];
  if (not ir_func) {
    auto work_item_ptr = DeferBody(this, node);
    auto *jmp_type     = &type_of(node)->as<type::Jump>();

    core::FnParams<type::Typed<ast::Declaration const *>> params(
        node->input().size());
    for (size_t i = 0; i < node->input().size(); ++i) {
      auto const *decl = node->input()[i];
      params.set(i, core::Param<type::Typed<ast::Declaration const *>>{
                        decl->id(), type::Typed(decl, jmp_type->args()[i])});
    }

    ir_func = AddJump(jmp_type, std::move(params));
    if (work_item_ptr) { ir_func->work_item = work_item_ptr; }
  }
  return ir::Results{ir_func};
}

static std::vector<std::pair<ast::Expression const *, ir::Results>>
EmitValueWithExpand(Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, ir::Results>> results;
  for (auto *expr : exprs) {
    results.emplace_back(expr, v->Visit(expr, EmitValueTag{}));
  }
  return results;
}

ir::Results Compiler::Visit(ast::PrintStmt const *node, EmitValueTag) {
  auto results = EmitValueWithExpand(this, node->exprs());
  for (auto &result : results) {
    if (auto const *table = dispatch_table(ast::ExprPtr{result.first, 0x01})) {
      table->EmitCall(
          this, core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                    {std::move(result)}, {}));
    } else {
      Visit(type_of(result.first), result.second, EmitPrintTag{});
    }
  }
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::ReturnStmt const *node, EmitValueTag) {
  auto arg_vals  = EmitValueWithExpand(this, node->exprs());
  auto *fn_scope = ASSERT_NOT_NULL(node->scope_->Containing<ast::FnScope>());
  auto *fn_lit   = ASSERT_NOT_NULL(fn_scope->fn_lit_);

  auto *fn_type = &ASSERT_NOT_NULL(type_of(fn_lit))->as<type::Function>();
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    // TODO return type maybe not the same as type actually returned?
    ir::SetRet(i, type::Typed{arg_vals[i].second, fn_type->output.at(i)});
  }

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  auto *scope = node->scope_;
  while (auto *exec = scope->if_as<ast::ExecScope>()) {
    MakeAllDestructions(this, exec);
    scope = exec->parent;
  }

  builder().disallow_more_stmts();
  builder().ReturnJump();
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::YieldStmt const *node, EmitValueTag) {
  auto arg_vals = EmitValueWithExpand(this, node->exprs());
  // TODO store this as an exec_scope.
  MakeAllDestructions(this, &node->scope_->as<ast::ExecScope>());
  // TODO pretty sure this is all wrong.

  // Can't return these because we need to pass them up at least through the
  // containing statements this and maybe further if we allow labelling
  // scopes to be yielded to.
  data_.yields_stack_.back().clear();
  data_.yields_stack_.back().reserve(arg_vals.size());
  // TODO one problem with this setup is that we look things up in a context
  // after returning, so the `after` method has access to a different
  // (smaller) collection of bound constants. This can change the meaning of
  // things or at least make them not compile if the `after` function takes
  // a compile-time constant argument.
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    data_.yields_stack_.back().emplace_back(node->exprs()[i], arg_vals[i].second);
  }

  builder().disallow_more_stmts();
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::ScopeLiteral const *node, EmitValueTag) {
  absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks;
  std::vector<ir::RegOr<ir::Jump const *>> inits;
  std::vector<ir::RegOr<ir::AnyFunc>> dones;
  for (auto const *decl : node->decls()) {
    if (decl->id() == "init") {
      inits.push_back(
          Visit(decl, EmitValueTag{}).get<ir::Jump const *>(0));
    } else if (decl->id() == "done") {
      dones.push_back(Visit(decl, EmitValueTag{}).get<ir::AnyFunc>(0));
    } else {
      blocks.emplace(
          decl->id(),
          Visit(decl, EmitValueTag{}).get<ir::BlockDef *>(0).value());
    }
  }

  return ir::Results{
      ir::ScopeHandler(data_.add_scope(module()), inits, dones, blocks)};
}

ir::Results InitializeAndEmitBlockNode(Compiler *compiler,
                                       ir::Results const &results,
                                       ast::BlockNode const *block_node) {
  // TODO this initialization should be the same as what's done with function
  // calls, so you should share that code. It's tricky because you need to worry
  // about conversions to/from variants.
  ASSERT(block_node->args().size() == results.size());
  size_t i = 0;
  for (auto *arg : block_node->args()) {
    ASSERT(arg->is<ast::Declaration>() == true);
    auto *t   = compiler->type_of(arg->if_as<ast::Declaration>());
    auto addr = compiler->addr(arg->if_as<ast::Declaration>());
    type::ApplyTypes<bool, uint8_t, uint16_t, uint32_t, uint64_t, int8_t,
                     int16_t, int32_t, int64_t, float, double, ir::Addr>(
        t, [&](auto tag) {
          using T = typename decltype(tag)::type;
          ir::Store(results.get<T>(i), addr);
        });
    i++;
  }
  return compiler->Visit(block_node, EmitValueTag{});
}

// Represents the data extracted from a scope literal ready for application
// locally to a scope node.
struct LocalScopeInterpretation {
  explicit LocalScopeInterpretation(
      ir::Builder &bldr,
      absl::flat_hash_map<std::string_view, ir::BlockDef *> const &block_defs,
      ast::ScopeNode const *node)
      : node_(node) {
    for (auto const &[name, block] : block_defs) {
      blocks_.emplace(std::piecewise_construct, std::forward_as_tuple(name),
                      std::forward_as_tuple(block, nullptr));
    }

    block_ptrs_.emplace(ir::BlockDef::Start(), bldr.AddBlock());
    block_ptrs_.emplace(ir::BlockDef::Exit(), bldr.AddBlock());

    for (auto const &block_node : node->blocks()) {
      auto &block        = blocks_.at(block_node.name());
      std::get<1>(block) = &block_node;
      block_ptrs_.emplace(std::get<0>(block), bldr.AddBlock());
    }
  }

  ir::BasicBlock *init_block() const {
    return block_ptrs_.at(ir::BlockDef::Start());
  }
  ir::BasicBlock *land_block() const {
    return block_ptrs_.at(ir::BlockDef::Exit());
  }

  ast::ScopeNode const *node_;
  absl::flat_hash_map<std::string_view,
                      std::tuple<ir::BlockDef const *, ast::BlockNode const *>>
      blocks_;
  absl::flat_hash_map<ir::BlockDef const *, ir::BasicBlock *> block_ptrs_;
};

ir::Results Compiler::Visit(ast::ScopeNode const *node, EmitValueTag) {
  DEBUG_LOG("ScopeNode")("Emitting IR for ScopeNode");

  DEBUG_LOG("ScopeNode")("scope_def ... evaluating.");
  auto *scope_def =
      backend::EvaluateAs<ir::ScopeDef *>(MakeThunk(node->name(), type::Scope));
  DEBUG_LOG("ScopeNode")("          ... completing work.");
  if (scope_def->work_item) { std::move (*scope_def->work_item)(); }
  DEBUG_LOG("ScopeNode")("          ... done.");

  DEBUG_LOG("ScopeNode")("Constructing interpretation");
  LocalScopeInterpretation interp(builder(), scope_def->blocks_, node);
  DEBUG_LOG("ScopeNode")("          ... done");

  builder().UncondJump(interp.init_block());
  builder().CurrentBlock() = interp.init_block();

  DEBUG_LOG("ScopeNode")("Inlining entry handler at ", ast::ExprPtr{node});
  ASSERT_NOT_NULL(jump_table(node, nullptr))
      ->EmitInlineCall(
          this, node->args().Transform([this](ast::Expression const *expr) {
            return std::pair(expr, Visit(expr, EmitValueTag{}));
          }),
          interp.block_ptrs_);

  DEBUG_LOG("ScopeNode")("Emit each block:");
  for (auto [block_name, block_and_node] : interp.blocks_) {
    if (block_name == "init" or block_name == "done") { continue; }
    DEBUG_LOG("ScopeNode")("... ", block_name);
    auto &[block, block_node] = block_and_node;
    auto iter                 = interp.block_ptrs_.find(block);
    if (iter == interp.block_ptrs_.end()) { continue; }
    builder().CurrentBlock() = iter->second;
    auto results =
        ASSERT_NOT_NULL(jump_table(ast::ExprPtr{block_node, 0x01}, nullptr))
            ->EmitInlineCall(this, {}, interp.block_ptrs_);
    InitializeAndEmitBlockNode(this, results, block_node);
  }

  // builder().CurrentBlock() = interp.land_block();
  //
  //   // TODO currently the block you end up on here is where EmitInlineCall
  //   thinks
  //   // you should end up, but that's not necessarily well-defined for
  //   things that
  //   // end up jumping to more than one possible location.
  //
  //   DEBUG_LOG("ScopeNode")("Inlining exit handler");
  //   {
  //     auto *mod       = const_cast<module::BasicModule
  //     *>(scope_def->module()); bool swap_bc    = module() != mod;
  //     module::BasicModule *old_mod = std::exchange(module(), mod);
  //     if (swap_bc) { data_.constants_ = &module()->dep_data_.front(); }
  //     base::defer d([&] {
  //       module() = old_mod;
  //       if (swap_bc) { data_.constants_ = &module()->dep_data_.front(); }
  //     });
  //   }
  //   auto result =
  //       ASSERT_NOT_NULL(dispatch_table(node))->EmitInlineCall({},
  //       {}, ctx);
  //
  //   DEBUG_LOG("ScopeNode")("Done emitting IR for ScopeNode");
  //   return result;
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::StructLiteral const *node, EmitValueTag) {
  if (node->args_.empty()) {
    // TODO what about handling incomplete types?
    std::vector<std::tuple<std::string_view, ir::RegOr<type::Type const *>>>
        fields;
    fields.reserve(node->fields_.size());
    for (auto const &field : node->fields_) {
      // TODO hashtags?
      fields.emplace_back(
          field.id(),
          Visit(field.type_expr(), EmitValueTag{}).get<type::Type const *>(0));
    }
    return ir::Results{ir::Struct(node->scope_, module(), std::move(fields))};
  } else {
    NOT_YET();
  }

  // // TODO A bunch of things need to be fixed here.
  // // * Lock access during creation so two requestors don't clobber each
  // other.
  // // * Add a way way for one requestor to wait for another to have created
  // the
  // // object and be notified.
  // //
  // // For now, it's safe to do this from within a single module compilation
  // // (which is single-threaded).
  // ir::CompiledFn *&ir_func = data_.constants_->second.ir_funcs_[node];
  // if (not ir_func) {
  auto work_item_ptr = DeferBody(this, node);

  //   auto const &arg_types =
  //   type_of(node)->as<type::GenericStruct>().deps_;

  //   core::FnParams<type::Typed<ast::Expression const *>> params;
  //   params.reserve(node->args_.size());
  //   size_t i = 0;
  //   for (auto const &d : node->args_) {
  //     params.append(d.id(), type::Typed<ast::Expression const *>(
  //                               d.init_val(), arg_types.at(i++)));
  //   }
  //
  //   ir_func = AddFunc(type::Func(arg_types, {type::Type_}),
  //   std::move(params));
  //
  //   ir_func->work_item = work_item_ptr;
  // }

  // return ir::Results{ir::AnyFunc{ir_func}};
}

ir::Results Compiler::Visit(ast::StructType const *node, EmitValueTag) {
  NOT_YET();
}

ir::Results Compiler::Visit(ast::Switch const *node, EmitValueTag) {
  absl::flat_hash_map<ir::BasicBlock *, ir::Results> phi_args;

  auto *land_block = builder().AddBlock();
  auto *t          = type_of(node);
  // TODO this is not precisely accurate if you have regular void.
  bool all_paths_jump = (t == type::Void());

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. this is very wrong.

  // TODO handle switching on tuples/multiple values?
  ir::Results expr_results;
  type::Type const *expr_type = nullptr;
  if (node->expr_) {
    expr_results = Visit(node->expr_.get(), EmitValueTag{});
    expr_type    = type_of(node->expr_.get());
  }

  for (size_t i = 0; i + 1 < node->cases_.size(); ++i) {
    auto &[body, match_cond] = node->cases_[i];
    auto *expr_block         = builder().AddBlock();

    ir::Results match_val = Visit(match_cond.get(), EmitValueTag{});
    ir::RegOr<bool> cond  = node->expr_
                               ? ir::EmitEq(type_of(match_cond.get()),
                                            match_val, expr_type, expr_results)
                               : match_val.get<bool>(0);

    auto next_block = ir::EarlyExitOn<true>(expr_block, cond);

    builder().CurrentBlock() = expr_block;
    if (body->is<ast::Expression>()) {
      phi_args.emplace(builder().CurrentBlock(),
                       Visit(body.get(), EmitValueTag{}));
      builder().UncondJump(land_block);
    } else {
      // It must be a jump/yield/return, which we've verified in VerifyType.
      Visit(body.get(), EmitValueTag{});

      if (not all_paths_jump) { builder().allow_more_stmts(); }
    }

    builder().CurrentBlock() = next_block;
  }

  if (node->cases_.back().first->is<ast::Expression>()) {
    phi_args.emplace(builder().CurrentBlock(),
                     Visit(node->cases_.back().first.get(), EmitValueTag{}));
    builder().UncondJump(land_block);
  } else {
    // It must be a jump/yield/return, which we've verified in VerifyType.
    Visit(node->cases_.back().first.get(), EmitValueTag{});
    if (not all_paths_jump) { builder().allow_more_stmts(); }
  }

  builder().CurrentBlock() = land_block;
  if (t == type::Void()) {
    return ir::Results{};
  } else {
    return ir::Phi(t->is_big() ? type::Ptr(t) : t, phi_args);
  }
}

ir::Results Compiler::Visit(ast::Terminal const *node, EmitValueTag) {
  return node->value();
}

ir::Results Compiler::Visit(ast::Unop const *node, EmitValueTag) {
  auto *operand_type = type_of(node->operand());
  if (auto const *table = dispatch_table(node)) {
    // TODO struct is not exactly right. we really mean user-defined
    return table->EmitCall(
        this, core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                  {std::pair(node->operand(),
                             Visit(node->operand(), EmitValueTag{}))},
                  {}));
  }

  switch (node->op()) {
    case frontend::Operator::Copy: {
      auto reg = builder().TmpAlloca(operand_type);
      EmitCopyInit(operand_type, Visit(node->operand(), EmitValueTag{}),
                   type::Typed<ir::Reg>(reg, operand_type));
      return ir::Results{reg};
    } break;
    case frontend::Operator::Move: {
      auto reg = builder().TmpAlloca(operand_type);
      EmitMoveInit(operand_type, Visit(node->operand(), EmitValueTag{}),
                   type::Typed<ir::Reg>(reg, operand_type));
      return ir::Results{reg};
    } break;
    case frontend::Operator::BufPtr:
      return ir::Results{ir::BufPtr(
          Visit(node->operand(), EmitValueTag{}).get<type::Type const *>(0))};
    case frontend::Operator::Not: {
      auto *t = type_of(node->operand());
      if (t == type::Bool) {
        return ir::Results{
            ir::Not(Visit(node->operand(), EmitValueTag{}).get<bool>(0))};
      } else if (t->is<type::Flags>()) {
        return ir::Results{ir::XorFlags(
            Visit(node->operand(), EmitValueTag{}).get<ir::FlagsVal>(0),
            ir::FlagsVal{t->as<type::Flags>().All})};

      } else {
        NOT_YET();
      }
    } break;
    case frontend::Operator::Sub: {
      auto operand_ir = Visit(node->operand(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          type_of(node->operand()), [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{ir::Neg(operand_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::TypeOf:
      return ir::Results{type_of(node->operand())};
    case frontend::Operator::Which:
      return ir::Results{ir::Load<type::Type const *>(ir::VariantType(
          Visit(node->operand(), EmitValueTag{}).get<ir::Reg>(0)))};
    case frontend::Operator::And:
      return ir::Results{Visit(node->operand(), EmitRefTag{})[0]};
    case frontend::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      // TODO what if there's an error during evaluation?
      return backend::Evaluate(
          MakeThunk(node->operand(), type_of(node->operand())));
    }
    case frontend::Operator::Mul:
      return ir::Results{ir::Ptr(
          Visit(node->operand(), EmitValueTag{}).get<type::Type const *>(0))};
    case frontend::Operator::At: {
      auto *t = type_of(node);
      return ir::Results{
          ir::Load(Visit(node->operand(), EmitValueTag{}).get<ir::Reg>(0), t)};
    }
    case frontend::Operator::Needs: {
      NOT_YET();
    } break;
    case frontend::Operator::Ensure: {
      NOT_YET();
    } break;
    case frontend::Operator::Expand: {
      ir::Results tuple_val = Visit(node->operand(), EmitValueTag{});
      ir::Reg tuple_reg     = tuple_val.get<ir::Reg>(0);
      type::Tuple const *tuple_type =
          &type_of(node->operand())->as<type::Tuple>();
      ir::Results results;
      for (size_t i = 0; i < tuple_type->size(); ++i) {
        results.append(ir::PtrFix(ir::Field(tuple_reg, tuple_type, i).get(),
                                  tuple_type->entries_[i]));
      }
      return results;
    }
    case frontend::Operator::VariadicPack: {
      NOT_YET();
    } break;
    default: UNREACHABLE("Operator is ", static_cast<int>(node->op()));
  }
}

}  // namespace compiler
