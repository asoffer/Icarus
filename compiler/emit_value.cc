#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "base/guarded.h"
#include "compiler/executable_module.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "interpretter/evaluate.h"
#include "ir/builder.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "ir/builtin_ir.h"
#include "ir/components.h"
#include "ir/jump.h"
#include "ir/reg.h"
#include "ir/struct_field.h"
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

type::QualType VerifyBody(Compiler *compiler, ast::FunctionLiteral const *node);
void VerifyBody(Compiler *compiler, ast::Jump const *node);

namespace {

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

ir::Results Compiler::Visit(ast::Access const *node, EmitValueTag) {
  auto *this_type = type_of(node);
  if (this_type == type::Module) {
    // TODO we already did this evaluation in type verification. Can't we just
    // save and reuse it?
    auto decls = interpretter::EvaluateAs<module::BasicModule const *>(
                     MakeThunk(node->operand(), type::Module))
                     ->declarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1: return Visit(decls[0], EmitValueTag{});
      default: NOT_YET();
    }
  }

  if (this_type->is<type::Enum>()) {
    auto lit = this_type->as<type::Enum>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else if (type_of(node->operand()) == type::ByteView) {
    ASSERT(node->member_name() == "length");
    return ir::Results{builder().ByteViewLength(
        Visit(node->operand(), EmitValueTag{}).get<std::string_view>(0))};
  } else {
    auto reg = Visit(node->operand(), EmitRefTag{})[0];
    auto *t  = this_type;

    if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
    while (auto *p = t->if_as<type::Pointer>()) {
      t   = p->pointee;
      reg = ir::Load<ir::Addr>(reg);
    }

    ASSERT(t, InheritsFrom<type::Struct>());
    auto *struct_type = &t->as<type::Struct>();
    auto field        = builder().Field(reg, struct_type,
                                 struct_type->index(node->member_name()));
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
    result = builder().Array(
        Visit(node->length(i), EmitValueTag{}).get<int64_t>(0), result);
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
            return ir::Results{
                builder().Add(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Sub: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{
                builder().Sub(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{
                builder().Mul(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{
                builder().Div(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = Visit(node->lhs(), EmitValueTag{});
      auto rhs_ir = Visit(node->rhs(), EmitValueTag{});
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Results{
                builder().Mod(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
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

      return ir::Results{builder().Arrow(lhs_vals, rhs_vals)};
    } break;
    case frontend::Operator::Assign: {
      // TODO support splatting.
      auto lhs_lvals = Visit(node->lhs(), EmitRefTag{});
      auto rhs_vals  = Visit(node->rhs(), EmitValueTag{});
      ASSERT(lhs_lvals.size() == rhs_vals.size());
      if (lhs_lvals.size() == 1) {
        Visit(lhs_type, lhs_lvals[0], type::Typed{rhs_vals, rhs_type},
              EmitMoveAssignTag{});

      } else {
        auto const &rhs_tup_type = rhs_type->as<type::Tuple>();
        auto const &lhs_tup_type = lhs_type->as<type::Tuple>();
        for (size_t i = 0; i < lhs_lvals.size(); ++i) {
          Visit(lhs_tup_type.entries_[i], lhs_lvals[i],
                type::Typed{rhs_vals.GetResult(i), rhs_tup_type.entries_[i]},
                EmitMoveAssignTag{});
        }
      }

      return ir::Results{};
    } break;
    case frontend::Operator::OrEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
        ir::Store(builder().OrFlags(
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

      return ir::Results{builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                             {ir::RegOr<bool>(true), rhs_val})};
    } break;
    case frontend::Operator::AndEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
        ir::Store(builder().AndFlags(
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
      return ir::Results{builder().Phi<bool>(
          {lhs_end_block, rhs_end_block}, {rhs_val, ir::RegOr<bool>(false)})};
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
      auto rhs_ir   = Visit(node->rhs(), EmitValueTag{});
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            ir::Store(builder().Add(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
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
            ir::Store(builder().Sub(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
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
            ir::Store(builder().Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
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
        ir::Store(builder().Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                  lhs_lval);
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
            ir::Store(builder().Mul(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = Visit(node->lhs(), EmitRefTag{})[0];
        auto rhs_ir   = Visit(node->rhs(), EmitValueTag{}).get<bool>(0);
        ir::Store(builder().Ne(ir::Load<bool>(lhs_lval), rhs_ir), lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = Visit(node->lhs(), EmitRefTag{})[0];
        auto rhs_ir = Visit(node->rhs(), EmitValueTag{}).get<ir::FlagsVal>(0);
        ir::Store(builder().XorFlags(ir::Load<ir::FlagsVal>(lhs_lval), rhs_ir),
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
  std::vector<ir::RegOr<ir::Jump *>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(Visit(decl, EmitValueTag{}).get<ir::AnyFunc>(0));
  }

  for (auto const &decl : node->after()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    afters.push_back(Visit(decl, EmitValueTag{}).get<ir::Jump *>(0));
  }

  return ir::Results{builder().MakeBlock(data_.add_block(), std::move(befores),
                                         std::move(afters))};
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
  }
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::BuiltinFn const *node, EmitValueTag) {
  return ir::Results{node->value()};
}

ir::Results EmitBuiltinCall(
    Compiler *c, ast::BuiltinFn const *callee,
    core::FnArgs<ast::Expression const *, std::string_view> const &args) {
  switch (callee->value()) {
    case core::Builtin::Foreign: {
      auto name = interpretter::EvaluateAs<std::string_view>(
          c->MakeThunk(args.at(0), type::ByteView));
      auto *foreign_type = interpretter::EvaluateAs<type::Type const *>(
          c->MakeThunk(args.at(1), type::Type_));
      return ir::Results{c->builder().LoadSymbol(name, foreign_type).get()};
    } break;

    case core::Builtin::Opaque:
      return ir::Results{c->builder().OpaqueType(c->module())};
    case core::Builtin::Bytes: {
      auto const &fn_type =
          ir::BuiltinType(core::Builtin::Bytes)->as<type::Function>();
      ir::OutParams outs = c->builder().OutParams(fn_type.output);
      ir::Reg reg        = outs[0];
      c->builder().Call(ir::BytesFn(), &fn_type,
                        {c->Visit(args.at(0), EmitValueTag{})},
                        std::move(outs));

      return ir::Results{reg};
    } break;

    case core::Builtin::Alignment: {
      auto const &fn_type =
          ir::BuiltinType(core::Builtin::Alignment)->as<type::Function>();
      ir::OutParams outs = c->builder().OutParams(fn_type.output);
      ir::Reg reg        = outs[0];
      c->builder().Call(ir::AlignmentFn(), &fn_type,
                        {c->Visit(args.at(0), EmitValueTag{})},
                        std::move(outs));

      return ir::Results{reg};
    } break;

#if defined(ICARUS_DEBUG)
    case core::Builtin::DebugIr: c->builder().DebugIr(); return ir::Results{};
#endif  // defined(ICARUS_DEBUG)
  }
  UNREACHABLE();
}

ir::Results Compiler::Visit(ast::Call const *node, EmitValueTag) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    return EmitBuiltinCall(this, b, node->args());
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
  // TODO node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
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
  auto &bldr  = compiler->builder();
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto[iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto const *fn_type =
        type::Func({type::Ptr(lhs_type), type::Ptr(rhs_type)}, {type::Bool});
    auto *fn = compiler->AddFunc(fn_type, fn_type->AnonymousFnParams());

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      bldr.CurrentBlock() = fn->entry();

      auto *equal_len_block = bldr.AddBlock();
      auto *true_block      = bldr.AddBlock();
      auto *false_block     = bldr.AddBlock();
      auto *phi_block       = bldr.AddBlock();
      auto *body_block      = bldr.AddBlock();
      auto *incr_block      = bldr.AddBlock();

      bldr.CondJump(bldr.Eq(lhs_type->len, rhs_type->len), equal_len_block,
                    false_block);

      bldr.CurrentBlock() = true_block;
      ir::SetRet(0, true);
      bldr.ReturnJump();

      bldr.CurrentBlock() = false_block;
      ir::SetRet(0, false);
      bldr.ReturnJump();

      bldr.CurrentBlock() = equal_len_block;
      auto lhs_start      = ir::Index(Ptr(lhs_type), ir::Reg::Arg(0), 0);
      auto rhs_start      = ir::Index(Ptr(rhs_type), ir::Reg::Arg(1), 0);
      auto lhs_end =
          bldr.PtrIncr(lhs_start, lhs_type->len, Ptr(rhs_type->data_type));
      bldr.UncondJump(phi_block);

      bldr.CurrentBlock() = phi_block;

      ir::Reg lhs_phi_reg = bldr.CurrentGroup()->Reserve();
      ir::Reg rhs_phi_reg = bldr.CurrentGroup()->Reserve();

      bldr.CondJump(bldr.Eq(ir::RegOr<ir::Addr>(lhs_phi_reg), lhs_end),
                    true_block, body_block);

      bldr.CurrentBlock() = body_block;
      // TODO what if data type is an array?
      bldr.CondJump(bldr.Eq(ir::Load<ir::Addr>(lhs_phi_reg),
                            ir::Load<ir::Addr>(rhs_phi_reg)),
                    incr_block, false_block);

      bldr.CurrentBlock() = incr_block;
      auto lhs_incr = bldr.PtrIncr(lhs_phi_reg, 1, Ptr(lhs_type->data_type));
      auto rhs_incr = bldr.PtrIncr(rhs_phi_reg, 1, Ptr(rhs_type->data_type));
      bldr.UncondJump(phi_block);

      bldr.Phi<ir::Addr>(lhs_phi_reg, {equal_len_block, incr_block},
                         {lhs_start, lhs_incr});
      bldr.Phi<ir::Addr>(rhs_phi_reg, {equal_len_block, incr_block},
                         {rhs_start, rhs_incr});
    }
  }

  ir::OutParams outs = compiler->builder().OutParams({type::Bool});
  auto result = outs[0];
  bldr.Call(ir::AnyFunc{iter->second}, iter->second->type_, {lhs_ir, rhs_ir},
            std::move(outs));
  return ir::Results{result};
}

static ir::RegOr<bool> EmitChainOpPair(Compiler *compiler,
                                       ast::ChainOp const *chain_op,
                                       size_t index, ir::Results const &lhs_ir,
                                       ir::Results const &rhs_ir) {
  auto &bldr     = compiler->builder();
  auto *lhs_type = compiler->type_of(chain_op->exprs()[index]);
  auto *rhs_type = compiler->type_of(chain_op->exprs()[index + 1]);
  auto op        = chain_op->ops()[index];

  if (lhs_type->is<type::Array>() and rhs_type->is<type::Array>()) {
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
          return bldr.Lt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Le(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
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
              return bldr.Eq(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
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
              return bldr.Ne(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Ge(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Gt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
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
            return builder().Ne(
                acc, Visit(expr, EmitValueTag{}).template get<bool>(0));
          })};
    } else if (t->is<type::Flags>()) {
      return ir::Results{std::accumulate(
          node->exprs().begin(), node->exprs().end(),
          ir::RegOr<ir::FlagsVal>(ir::FlagsVal{0}),
          [&](ir::RegOr<ir::FlagsVal> acc, auto *expr) {
            return builder().XorFlags(
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
      val = builder().OrFlags(
          val, Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops()[0] == frontend::Operator::And and
             t->is<type::Flags>()) {
    auto iter = node->exprs().begin();
    auto val  = Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0);
    while (++iter != node->exprs().end()) {
      val = builder().AndFlags(
          val, Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0));
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
    auto reg_or_type = builder().Var(args);
    return ir::Results{reg_or_type};
  } else if (node->ops()[0] == frontend::Operator::Or and t == type::Block) {
    NOT_YET();
  } else if (node->ops()[0] == frontend::Operator::And or
             node->ops()[0] == frontend::Operator::Or) {
    auto *land_block = builder().AddBlock();

    std::vector<ir::BasicBlock const *> phi_blocks;
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

    return ir::Results{
        builder().Phi<bool>(std::move(phi_blocks), std::move(phi_results))};

  } else {
    if (node->ops().size() == 1) {
      auto lhs_ir = Visit(node->exprs()[0], EmitValueTag{});
      auto rhs_ir = Visit(node->exprs()[1], EmitValueTag{});
      return ir::Results{EmitChainOpPair(this, node, 0, lhs_ir, rhs_ir)};

    } else {
      std::vector<ir::BasicBlock const *> phi_blocks;
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

      return ir::Results{builder().Phi<bool>(std::move(phi_blocks),std::move( phi_values))};
    }
  }
  UNREACHABLE();
}

ir::Results Compiler::Visit(ast::CommaList const *node, EmitValueTag) {
  auto *tuple_type = &type_of(node)->as<type::Tuple>();
  // TODO this is a hack. I'm still not sure what counts as a tuple and what
  // counts as atype
  if (tuple_type->entries_.empty()) { return ir::Results{type::Tup({})}; }

  ir::Results results;
  for (auto &expr : node->exprs_) {
    results.append(Visit(expr.get(), EmitValueTag{}));
  }
  return results;
}

ir::Results Compiler::Visit(ast::Declaration const *node, EmitValueTag) {
  DEBUG_LOG("EmitValueDeclaration")(node->id());
  if (node->flags() & ast::Declaration::f_IsConst) {
    if (node->module() != module()) {
      // TODO: This is wrong. Looking up in *any* dependent data is not what we
      // want to do. We want to find it in the correct dependent data. But we
      // need to rework contant bindings anyway.
      auto result = node->module()
                        ->as<CompiledModule>()
                        .data_.constants_->first.get_constant(node);
      if (result.size() == 1) { return result; }

      for (auto &[constant_binding, dep_data] :
           node->module()->as<CompiledModule>().data_.dep_data_) {
        auto result = dep_data.constants_.get_constant(node);
        if (result.size() == 1) { return result; }
      }
      UNREACHABLE("should have found it already.");
    }

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
      auto *t = ASSERT_NOT_NULL(type_of(node));

      auto slot = data_.constants_->second.constants_.reserve_slot(node, t);
      if (auto *result = std::get_if<ir::Results>(&slot)) {
        return std::move(*result);
      }

      auto & [ data_offset, num_bytes ] =
          std::get<std::pair<size_t, core::Bytes>>(slot);

      if (node->IsCustomInitialized()) {
        // TODO there's a lot of inefficiency here. `buf` is copied into the
        // constants slot and the copied to an ir::Results object to be
        // returned. In reality, we could write directly to the buffer and only
        // copy once if Evaluate* took an out-parameter.
        base::untyped_buffer buf =
            interpretter::EvaluateToBuffer(MakeThunk(node->init_val(), t));
        if (num_errors() > 0u) {
          // TODO we reserved a slot and haven't cleaned it up. Do we care?
          NOT_YET("Found errors but haven't handeled them.");
          return ir::Results{};
        }
        return data_.constants_->second.constants_.set_slot(
            data_offset, buf.raw(0), num_bytes);
      } else if (node->IsDefaultInitialized()) {
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
    }
    UNREACHABLE(node->DebugString());
  } else {
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
      return ir::Results{builder().Enum(module(), names, specified_values)};
    case ast::EnumLiteral::Kind::Flags:
      return ir::Results{builder().Flags(module(), names, specified_values)};
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
      if (not data_.constants_->first.contains(dep)) {
        return ir::Results{node};
      }
    }
  }

  // TODO Use correct constants
  ir::CompiledFn *&ir_func = data_.constants_->second.ir_funcs_[node];
  if (not ir_func) {
    auto *work_item_ptr = DeferBody(this, node);

    auto *fn_type = &type_of(node)->as<type::Function>();

    ir_func = AddFunc(fn_type,
                      node->params().Transform([ fn_type, i = 0 ](
                          std::unique_ptr<ast::Declaration> const &d) mutable {
                        return type::Typed<ast::Declaration const *>(
                            d.get(), fn_type->input.at(i++));
                      }));
    if (work_item_ptr) { ir_func->work_item = work_item_ptr; }
  }

  return ir::Results{ir_func};
}

ir::Results Compiler::Visit(ast::Identifier const *node, EmitValueTag) {
  ASSERT(node->decl() != nullptr) << node->DebugString();
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
  auto *pending_mod = ASSERT_NOT_NULL(pending_module(node));
  DEBUG_LOG("Import")("Waiting for ", pending_mod);
  auto *mod = pending_mod->get();
  DEBUG_LOG("Import")("Completed compilation of ", pending_mod, " as ", mod);
  return ir::Results{mod};
}

ir::Results Compiler::Visit(ast::Index const *node, EmitValueTag) {
  if (type_of(node->lhs()) == type::ByteView) {
    auto data = builder().ByteViewData(
        Visit(node->lhs(), EmitValueTag{}).get<std::string_view>(0));
    auto addr = builder().PtrIncr(
        data, Visit(node->rhs(), EmitValueTag{}).get<int64_t>(0),
        type::Ptr(type::Nat8));
    return ir::Results{ir::Load(addr, type::Nat8)};
  }
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
  return ir::Results{data_.add_jump(node, [this, node] {
    auto work_item_ptr = DeferBody(this, node);
    auto *jmp_type     = &type_of(node)->as<type::Jump>();

    size_t i = 0;
    auto params = node->params().Transform([&](auto const &decl) {
      return type::Typed<ast::Declaration const *>(decl.get(),
                                                   jmp_type->args()[i++]);
    });

    DEBUG_LOG("Jump")("Jump type = ", jmp_type->to_string());
    ir::Jump jmp(jmp_type, std::move(params));
    if (work_item_ptr) { jmp.work_item = work_item_ptr; }
    return jmp;
  })};
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
    auto *ret_type = fn_type->output[i];
    if (ret_type->is_big()) {
      // TODO must `r` be holding a register?
      // TODO guaranteed move-elision
      ASSERT(arg_vals[i].second.size() == 1u);
      EmitMoveInit(ret_type, arg_vals[i].second,
                   type::Typed(ir::GetRet(i, ret_type), type::Ptr(ret_type)));

    } else {
      ir::SetRet(i, type::Typed{arg_vals[i].second, ret_type});
    }
  }

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  auto *scope = node->scope_;
  while (auto *exec = scope->if_as<ast::ExecScope>()) {
    MakeAllDestructions(this, exec);
    if (not exec->parent) { break; }
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
    data_.yields_stack_.back().emplace_back(node->exprs()[i],
                                            arg_vals[i].second);
  }

  builder().disallow_more_stmts();
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::ScopeLiteral const *node, EmitValueTag) {
  absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks;
  std::vector<ir::RegOr<ir::Jump *>> inits;
  std::vector<ir::RegOr<ir::AnyFunc>> dones;
  for (auto const *decl : node->decls()) {
    if (decl->id() == "init") {
      inits.push_back(Visit(decl, EmitValueTag{}).get<ir::Jump *>(0));
    } else if (decl->id() == "done") {
      dones.push_back(Visit(decl, EmitValueTag{}).get<ir::AnyFunc>(0));
    } else {
      blocks.emplace(
          decl->id(),
          Visit(decl, EmitValueTag{}).get<ir::BlockDef *>(0).value());
    }
  }

  return ir::Results{builder().MakeScope(data_.add_scope(module()),
                                         std::move(inits), std::move(dones),
                                         std::move(blocks))};
}

ir::Results Compiler::Visit(ast::ScopeNode const *node, EmitValueTag) {
  DEBUG_LOG("ScopeNode")("Emitting IR for ScopeNode");

  auto const &scope_dispatch_table =
      *ASSERT_NOT_NULL(data_.scope_dispatch_table(node));

  // Jump to a new block in case some scope ends up with `goto start()` in order
  // to re-evealuate arguments.
  auto *args_block = builder().AddBlock();
  builder().UncondJump(args_block);
  builder().CurrentBlock() = args_block;

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(Visit(expr, EmitValueTag{}), type_of(expr));
  });

  return scope_dispatch_table.EmitCall(this, args);
}

ir::Results Compiler::Visit(ast::StructLiteral const *node, EmitValueTag) {
  std::vector<ir::StructField> fields;
  fields.reserve(node->fields().size());
  for (auto const &field : node->fields()) {
    // TODO hashtags and initial values.
    fields.emplace_back(
        field.id(),
        Visit(field.type_expr(), EmitValueTag{}).get<type::Type const *>(0));
  }

  return ir::Results{builder().Struct(node->scope_, fields)};
}

ir::Results Compiler::Visit(ast::ParameterizedStructLiteral const *node,
                            EmitValueTag) {
  NOT_YET();
  // // TODO A bunch of things need to be fixed here.
  // // * Lock access during creation so two requestors don't clobber each
  // other.
  // // * Add a way for one requestor to wait for another to have created
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
  //   params.reserve(node->params().size());
  //   size_t i = 0;
  //   for (auto const &d : node->params()) {
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

  absl::flat_hash_map<ir::BasicBlock *, ir::Results> phi_args;
  for (size_t i = 0; i + 1 < node->cases_.size(); ++i) {
    auto & [ body, match_cond ] = node->cases_[i];
    auto *expr_block            = builder().AddBlock();

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
    } else if (body->is<ast::PrintStmt>()) {
      Visit(body.get(), EmitValueTag{});
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
  } else if (node->cases_.back().first->is<ast::PrintStmt>()) {
    Visit(node->cases_.back().first.get(), EmitValueTag{});
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
    DEBUG_LOG("switch")
    ("phi node is holding a ", (t->is_big() ? type::Ptr(t) : t)->to_string());
    auto r = type::Apply(t->is_big() ? type::Ptr(t) : t, [&](auto tag) {
      using T = typename decltype(tag)::type;
      std::vector<ir::RegOr<T>> vals;
      vals.reserve(phi_args.size());
      std::vector<ir::BasicBlock const *> blocks;
      blocks.reserve(phi_args.size());
      for (auto const & [ key, val ] : phi_args) {
        blocks.push_back(key);
        vals.push_back(val.template get<T>(0));
      }
      return ir::Results{builder().Phi<T>(blocks, vals)};
    });
    return r;
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
      return ir::Results{builder().BufPtr(
          Visit(node->operand(), EmitValueTag{}).get<type::Type const *>(0))};
    case frontend::Operator::Not: {
      auto *t = type_of(node->operand());
      if (t == type::Bool) {
        return ir::Results{
            builder().Not(Visit(node->operand(), EmitValueTag{}).get<bool>(0))};
      } else if (t->is<type::Flags>()) {
        return ir::Results{builder().XorFlags(
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
            return ir::Results{builder().Neg(operand_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::TypeOf:
      return ir::Results{type_of(node->operand())};
    case frontend::Operator::Which:
      return ir::Results{ir::Load<type::Type const *>(builder().VariantType(
          Visit(node->operand(), EmitValueTag{}).get<ir::Reg>(0)))};
    case frontend::Operator::And:
      return ir::Results{Visit(node->operand(), EmitRefTag{})[0]};
    case frontend::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      // TODO what if there's an error during evaluation?
      return interpretter::Evaluate(
          MakeThunk(node->operand(), type_of(node->operand())));
    }
    case frontend::Operator::Mul:
      return ir::Results{builder().Ptr(
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
        results.append(
            ir::PtrFix(builder().Field(tuple_reg, tuple_type, i).get(),
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
