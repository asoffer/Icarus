#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "base/guarded.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/executable_module.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "interpretter/evaluate.h"
#include "ir/builder.h"
#include "ir/jump.h"
#include "ir/struct_field.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/reg.h"
#include "type/jump.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
using ::matcher::InheritsFrom;

type::QualType VerifyBody(Compiler *compiler, ast::FunctionLiteral const *node);

namespace {

template <typename NodeType>
base::move_func<void()> *DeferBody(Compiler *compiler, NodeType const *node) {
  // It's safe to capture `compiler` because we know this lambda will be
  // executed as part of work deferral of `compiler` before the compiler is
  // destroyed.
  return compiler->AddWork(node, [compiler, node]() mutable {
    if constexpr (std::is_same_v<NodeType, ast::FunctionLiteral>) {
      VerifyBody(compiler, node);
    }
    CompleteBody(compiler, node);
  });
}

}  // namespace

ir::Results Compiler::Visit(ast::Access const *node, EmitValueTag) {
  if (type_of(node->operand()) == type::Module) {
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

  auto *this_type = type_of(node);
  if (this_type->is<type::Enum>()) {
    auto lit = this_type->as<type::Enum>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(node->member_name());
    return ir::Results{lit};
  } else if (type_of(node->operand()) == type::ByteView) {
    ASSERT(node->member_name() == "length");
    return ir::Results{builder().ByteViewLength(
        Visit(node->operand(), EmitValueTag{}).get<ir::String>(0))};
  } else {
    auto reg = Visit(node->operand(), EmitRefTag{})[0];
    auto *t  = type_of(node->operand());

    if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
    while (auto *p = t->if_as<type::Pointer>()) {
      t   = p->pointee;
      reg = ir::Load<ir::Addr>(reg);
    }

    ASSERT(t, InheritsFrom<type::Struct>()) << t->to_string();
    auto *struct_type = &t->as<type::Struct>();
    auto field        = builder().Field(reg, struct_type,
                                 struct_type->index(node->member_name()));
    return ir::Results{builder().PtrFix(field.get(), this_type)};
  }
}

ir::Results Compiler::Visit(ast::ArrayLiteral const *node, EmitValueTag) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = type_of(node);
  auto alloc      = builder().TmpAlloca(this_type);
  if (not node->empty()) {
    auto *data_type = this_type->as<type::Array>().data_type;
    for (size_t i = 0; i < node->size(); ++i) {
      EmitMoveInit(
          data_type, Visit(node->elem(i), EmitValueTag{}),
          type::Typed<ir::Reg>(builder().Index(type::Ptr(this_type), alloc,
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
  std::vector<ir::RegOr<ir::Fn>> befores;
  std::vector<ir::RegOr<ir::Jump *>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(Visit(decl, EmitValueTag{}).get<ir::Fn>(0));
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

Compiler::YieldResult Compiler::EmitBlockNode(
    ast::BlockNode const *node) {
  core::FnArgs<std::pair<ir::Results, type::QualType>> results;
  ICARUS_SCOPE(PushVec(&yields_stack_)) {
    EmitIrForStatements(this, node->stmts());
    return yields_stack_.back();
  }
}

ir::Results Compiler::Visit(ast::BlockNode const *node, EmitValueTag) {
  UNREACHABLE("Should be called via Compiler::EmitBlockNode");
}

ir::Results Compiler::Visit(ast::BuiltinFn const *node, EmitValueTag) {
  return ir::Results{node->value()};
}

ir::Results EmitBuiltinCall(
    Compiler *c, ast::BuiltinFn const *callee,
    core::FnArgs<ast::Expression const *, std::string_view> const &args) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      auto name = interpretter::EvaluateAs<ir::String>(
          c->MakeThunk(args.at(0), type::ByteView));
      auto *foreign_type = interpretter::EvaluateAs<type::Type const *>(
          c->MakeThunk(args.at(1), type::Type_));
      return ir::Results{c->builder().LoadSymbol(name, foreign_type).get()};
    } break;

    case ir::BuiltinFn::Which::Opaque:
      return ir::Results{c->builder().OpaqueType(c->module())};
    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::BuiltinFn::Bytes().type();
      ir::OutParams outs = c->builder().OutParams(fn_type.output());
      ir::Reg reg        = outs[0];
      c->builder().Call(ir::Fn{ir::BuiltinFn::Bytes()}, &fn_type,
                        {c->Visit(args.at(0), EmitValueTag{})},
                        std::move(outs));

      return ir::Results{reg};
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      auto const &fn_type = *ir::BuiltinFn::Alignment().type();
      ir::OutParams outs = c->builder().OutParams(fn_type.output());
      ir::Reg reg        = outs[0];
      c->builder().Call(ir::Fn{ir::BuiltinFn::Alignment()}, &fn_type,
                        {c->Visit(args.at(0), EmitValueTag{})},
                        std::move(outs));

      return ir::Results{reg};
    } break;

    case ir::BuiltinFn::Which::DebugIr:
      c->builder().DebugIr();
      return ir::Results{};
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
  if (type::IsNumeric(from_type)) {
    if (type::IsIntegral(from_type)) {
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double,
                              ir::EnumVal, ir::FlagsVal>(
          to_type, [&](auto tag) {
            return ir::Results{
                ir::CastTo<typename decltype(tag)::type>(from_type, results)};
          });
    } else {
      return type::ApplyTypes<float, double>(to_type, [&](auto tag) {
        return ir::Results{
            ir::CastTo<typename decltype(tag)::type>(from_type, results)};
      });
    }
  } else {
    NOT_YET();
  }
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
      base::untyped_buffer_view result = node->module()
                                             ->as<CompiledModule>()
                                             .root_node()
                                             ->binding()
                                             .get_constant(node);
      if (not result.empty()) { return ir::Results::FromRaw(result); }

      UNREACHABLE("should have found it already.");
    }

    // TODO
    if (node->flags() & ast::Declaration::f_IsFnParam) {
      if (auto result = current_constants_->binding().get_constant(node);
          not result.empty()) {
        return ir::Results::FromRaw(result);
      } else {
        UNREACHABLE();
      }
    } else {
      auto *t = ASSERT_NOT_NULL(type_of(node));

      base::untyped_buffer_view slot =
          current_constants_->binding().reserve_slot(node, t);
      if (not slot.empty()) { return ir::Results::FromRaw(slot); }

      if (node->IsCustomInitialized()) {
        // TODO there's a lot of inefficiency here. `buf` is copied into the
        // constants slot and the copied to an ir::Results object to be
        // returned. In reality, we could write directly to the buffer and only
        // copy once if Evaluate* took an out-parameter.
        base::untyped_buffer buf =
            interpretter::EvaluateToBuffer(MakeThunk(node->init_val(), t));
        if (diag().num_consumed() > 0u) {
          // TODO we reserved a slot and haven't cleaned it up. Do we care?
          NOT_YET("Found errors but haven't handled them.");
          return ir::Results{};
        }
        current_constants_->binding().set_slot(node, buf);
        return ir::Results::FromRaw(buf);
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

ir::Results Compiler::Visit(ast::DesignatedInitializer const *node, EmitValueTag) {
  // TODO actual initialization with these field members.
  auto *t    = type_of(node);
  auto alloc = builder().TmpAlloca(t);

  auto &struct_type  = t->as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto &[field_name, expr] : node->assignments()) {
      // Skip default initialization if we're going to use the designated
      // initializer.
      if (field_name == field.name) { goto next_field; }
    }

    Visit(field.type, builder().Field(alloc, &struct_type, i).get(),
          EmitDefaultInitTag{});
  next_field:;
  }

  // TODO initialize fields not listed in the designated initializer.
  for (auto &[field, expr] : node->assignments()) {
    auto *f            = struct_type.field(field);
    size_t field_index = struct_type.index(f->name);
    Visit(expr.get(), builder().Field(alloc, &struct_type, field_index),
          EmitMoveInitTag{});
  }
  return ir::Results{alloc};
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
      if (decl->IsCustomInitialized()) {
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
    if (p->flags() & ast::Declaration::f_IsConst) { return ir::Results{}; }
  }

  // TODO Use correct constants
  ir::NativeFn ir_func =
      data_.ir_funcs_
          .emplace(node, base::lazy_convert([&] {
                     auto *fn_type = &type_of(node)->as<type::Function>();

                     auto f = AddFunc(
                         fn_type,
                         node->params().Transform(
                             [fn_type, i = 0](auto const &d) mutable {
                               return type::Typed<ast::Declaration const *>(
                                   d.get(), fn_type->params().at(i++).value);
                             }));
                     f->work_item = DeferBody(this, node);
                     return f;
                   }))
          .first->second;

  return ir::Results{ir::Fn{ir_func}};
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
    return ir::Results{builder().PtrFix(lval.reg(), t)};
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
        Visit(node->lhs(), EmitValueTag{}).get<ir::String>(0));
    auto addr = builder().PtrIncr(
        data, Visit(node->rhs(), EmitValueTag{}).get<int64_t>(0),
        type::Ptr(type::Nat8));
    return ir::Results{ir::Load(addr, type::Nat8)};
  }
  return ir::Results{
      builder().PtrFix(Visit(node, EmitRefTag{})[0].reg(), type_of(node))};
}

ir::Results Compiler::Visit(ast::Goto const *node, EmitValueTag) {
  std::vector<std::string_view> names;
  names.reserve(node->options().size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(node->options().size());

  std::vector<core::FnArgs<type::Typed<ir::Results>>> args;
  args.reserve(node->options().size());

  auto current_block = builder().CurrentBlock();

  for (auto const &opt : node->options()) {
    ir::BasicBlock *block = builder().AddBlock();
    blocks.push_back(block);
    names.push_back(opt.block());

    builder().CurrentBlock() = block;

    args.push_back(opt.args().Transform([this](auto const &expr) {
      return type::Typed(Visit(expr.get(), EmitValueTag{}),
                         type_of(expr.get()));
    }));
  }

  builder().CurrentBlock() = current_block;
  builder().ChooseJump(std::move(names), std::move(blocks), std::move(args));
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::Label const *node, EmitValueTag) {
  return ir::Results{node->value()};
}

ir::Results Compiler::Visit(ast::Jump const *node, EmitValueTag) {
  return ir::Results{data_.add_jump(node, [this, node] {
    auto work_item_ptr = DeferBody(this, node);
    auto *jmp_type     = &type_of(node)->as<type::Jump>();

    size_t i    = 0;
    auto params = node->params().Transform([&](auto const &decl) {
      return type::Typed<ast::Declaration const *>(
          decl.get(), jmp_type->args().at(i++).value);
    });

    DEBUG_LOG("Jump")("Jump type = ", jmp_type->to_string());
    ir::Jump jmp(jmp_type, std::move(params));
    if (work_item_ptr) { jmp.work_item = work_item_ptr; }
    return jmp;
  })};
}

static std::vector<std::pair<ast::Expression const *, ir::Results>>
EmitValueWithExpand(Compiler *c, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, ir::Results>> results;
  for (auto *expr : exprs) {
    results.emplace_back(expr, c->Visit(expr, EmitValueTag{}));
  }
  return results;
}

ir::Results Compiler::Visit(ast::ReturnStmt const *node, EmitValueTag) {
  auto arg_vals  = EmitValueWithExpand(this, node->exprs());
  auto *fn_scope = ASSERT_NOT_NULL(node->scope()->Containing<ast::FnScope>());
  auto *fn_lit   = ASSERT_NOT_NULL(fn_scope->fn_lit_);

  auto *fn_type = &ASSERT_NOT_NULL(type_of(fn_lit))->as<type::Function>();
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    // TODO return type maybe not the same as type actually returned?
    auto *ret_type = fn_type->output()[i];
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
  auto *scope = node->scope();
  while (auto *exec = scope->if_as<ast::ExecScope>()) {
    MakeAllDestructions(this, exec);
    if (not exec->parent) { break; }
    scope = exec->parent;
  }

  builder().ReturnJump();
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::YieldStmt const *node, EmitValueTag) {
  auto arg_vals = EmitValueWithExpand(this, node->exprs());
  // TODO store this as an exec_scope.
  MakeAllDestructions(this, &node->scope()->as<ast::ExecScope>());
  // TODO pretty sure this is all wrong.

  // Can't return these because we need to pass them up at least through the
  // containing statements this and maybe further if we allow labelling
  // scopes to be yielded to.
  auto &yield_result = yields_stack_.back();

  // TODO one problem with this setup is that we look things up in a context
  // after returning, so the `after` method has access to a different
  // (smaller) collection of bound constants. This can change the meaning of
  // things or at least make them not compile if the `after` function takes
  // a compile-time constant argument.
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    auto qt = qual_type_of(node->exprs()[i]);
    ASSERT(qt.has_value() == true);
    yield_result.vals.pos_emplace(arg_vals[i].second, *qt);
  }
  yield_result.label = node->label() ? node->label()->value() : ir::Label{};

  builder().block_termination_state() =
      node->label() ? ir::Builder::BlockTerminationState::kLabeledYield
                    : ir::Builder::BlockTerminationState::kYield;
  return ir::Results{};
}

ir::Results Compiler::Visit(ast::ScopeLiteral const *node, EmitValueTag) {
  auto *state_type = node->state_type()
                         ? interpretter::EvaluateAs<type::Type const *>(
                               MakeThunk(node->state_type(), type::Type_))
                         : nullptr;

  absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks;
  std::vector<ir::RegOr<ir::Jump *>> inits;
  std::vector<ir::RegOr<ir::Fn>> dones;
  for (auto const *decl : node->decls()) {
    if (decl->id() == "init") {
      inits.push_back(Visit(decl, EmitValueTag{}).get<ir::Jump *>(0));
    } else if (decl->id() == "done") {
      dones.push_back(Visit(decl, EmitValueTag{}).get<ir::Fn>(0));
    } else {
      blocks.emplace(
          decl->id(),
          Visit(decl, EmitValueTag{}).get<ir::BlockDef *>(0).value());
    }
  }

  return ir::Results{builder().MakeScope(data_.add_scope(module(), state_type),
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

  return ir::Results{builder().Struct(node->scope(), fields)};
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
  // ir::CompiledFn *&ir_func = data_.ir_funcs_[node];
  // if (not ir_func) {
  auto work_item_ptr = DeferBody(this, node);

  //   auto const &arg_types =
  //   type_of(node)->as<type::GenericStruct>().deps_;

  //   core::Params<type::Typed<ast::Expression const *>> params;
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

  // return ir::Results{ir::Fn{ir_func}};
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
    auto &[body, match_cond] = node->cases_[i];
    auto *expr_block         = builder().AddBlock();
    auto *match_cond_ptr     = match_cond.get();
    ir::Results match_val = Visit(match_cond_ptr, EmitValueTag{});
    ir::RegOr<bool> cond  = [&] {
      if (node->expr_) {
        ASSERT(expr_type == type_of(match_cond_ptr));
        return builder().Eq(expr_type, match_val, expr_results);
      } else {
        return match_val.get<bool>(0);
      }
    }();

    auto next_block = builder().EarlyExitOn<true>(expr_block, cond);

    builder().CurrentBlock() = expr_block;
    if (body->is<ast::Expression>()) {
      phi_args.emplace(builder().CurrentBlock(),
                       Visit(body.get(), EmitValueTag{}));
      builder().UncondJump(land_block);
    } else {
      // It must be a jump/yield/return, which we've verified in VerifyType.
      Visit(body.get(), EmitValueTag{});

      if (not all_paths_jump) {
        builder().block_termination_state() =
            ir::Builder::BlockTerminationState::kMoreStatements;
      }
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
    if (not all_paths_jump) {
      builder().block_termination_state() =
          ir::Builder::BlockTerminationState::kMoreStatements;
    }
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
      for (auto const &[key, val] : phi_args) {
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
          ir::Load(Visit(node->operand(), EmitValueTag{}).get<ir::Addr>(0), t)};
    }
    case frontend::Operator::Needs: {
      NOT_YET();
    } break;
    case frontend::Operator::Ensure: {
      NOT_YET();
    } break;
    case frontend::Operator::VariadicPack: {
      NOT_YET();
    } break;
    default: UNREACHABLE("Operator is ", static_cast<int>(node->op()));
  }
}

}  // namespace compiler
