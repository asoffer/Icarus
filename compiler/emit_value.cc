#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "base/defer.h"
#include "base/guarded.h"
#include "compiler/dispatch/fn_call_table.h"
#include "compiler/dispatch/scope_table.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/executable_module.h"
#include "diagnostic/consumer/trivial.h"
#include "frontend/parse.h"
#include "interpretter/evaluate.h"
#include "interpretter/execute.h"
#include "ir/builder.h"
#include "ir/jump.h"
#include "ir/struct_field.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
using ::matcher::InheritsFrom;

type::QualType VerifyBody(Compiler *compiler, ast::FunctionLiteral const *node,
                          type::Type const *fn_type = nullptr);

namespace {

absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> MakeJumpInits(
    Compiler *c, ast::OverloadSet const &os) {
  absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> inits;
  DEBUG_LOG("ScopeNode")
  ("Overload set for inits has size ", os.members().size());
  for (ast::Expression const *member : os.members()) {
    DEBUG_LOG("ScopeNode")(member->DebugString());

    auto maybe_def = c->EvaluateAs<ir::ScopeDef *>(member);
    if (not maybe_def) { NOT_YET(); }
    auto *def = *maybe_def;
    DEBUG_LOG("ScopeNode")(def);
    if (def->work_item and *def->work_item) {
      (std::move(*def->work_item))();
      def->work_item = nullptr;
    }
    for (auto *init : def->start_->after_) {
      bool success = inits.emplace(init, def).second;
      static_cast<void>(success);
      ASSERT(success == true);
    }
  }
  return inits;
}

void AddAdl(ast::OverloadSet *overload_set, std::string_view id,
            type::Type const *t) {
  absl::flat_hash_set<CompiledModule *> modules;
  // TODO t->ExtractDefiningModules(&modules);

  for (auto *mod : modules) {
    auto decls = mod->ExportedDeclarations(id);
    // TODO accessing module from another thread is bad news!
    auto &data = mod->data();
    diagnostic::TrivialConsumer consumer;

    for (auto *d : decls) {
      // TODO Wow this is a terrible way to access the type.
      ASSIGN_OR(continue, auto &t,
                Compiler({
                             .builder             = ir::GetBuilder(),
                             .data                = data,
                             .diagnostic_consumer = consumer,
                         })
                    .type_of(d));
      // TODO handle this case. I think it's safe to just discard it.
      for (auto const *expr : overload_set->members()) {
        if (d == expr) { return; }
      }

      // TODO const
      overload_set->insert(d);
    }
  }
}

ast::OverloadSet FindOverloads(
    ast::Scope const *scope, std::string_view token,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  ast::OverloadSet os(scope, token);
  for (type::Typed<ir::Value> const &arg : args) {
    AddAdl(&os, token, arg.type());
  };
  DEBUG_LOG("FindOverloads")
  ("Found ", os.members().size(), " overloads for '", token, "'");
  return os;
}

std::optional<ast::OverloadSet> MakeOverloadSet(
    Compiler *c, ast::Expression const *expr,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  if (auto *id = expr->if_as<ast::Identifier>()) {
    return FindOverloads(expr->scope(), id->token(), args);
  } else if (auto *acc = expr->if_as<ast::Access>()) {
    ASSIGN_OR(return std::nullopt,  //
                     auto result, c->VerifyType(acc->operand()));
    if (result.type() == type::Module) {
      auto maybe_mod = c->EvaluateAs<module::BasicModule *>(acc->operand());
      if (not maybe_mod) { NOT_YET(); }
      auto const *mod         = &(*maybe_mod)->as<CompiledModule>();
      std::string_view member = acc->member_name();
      ast::OverloadSet os(mod->ExportedDeclarations(member));
      for (type::Typed<ir::Value> const &arg : args) {
        AddAdl(&os, member, arg.type());
      };
      return os;
    }
  }

  ast::OverloadSet os;
  os.insert(expr);
  // TODO ADL for node?
  return os;
}

template <typename NodeType>
base::move_func<void()> *DeferBody(Compiler::PersistentResources resources,
                                   NodeType const *node, type::Type const *t) {
  DEBUG_LOG("DeferBody")(node->DebugString());
  auto [iter, success] = resources.data.deferred_work_.lock()->emplace(
      node, [c = Compiler(resources), node, t]() mutable {
        if constexpr (base::meta<NodeType> ==
                      base::meta<ast::FunctionLiteral>) {
          VerifyBody(&c, node, t);
          CompleteBody(&c, node, &t->as<type::Function>());
        } else if constexpr (base::meta<NodeType> ==
                             base::meta<ast::ShortFunctionLiteral>) {
          CompleteBody(&c, node, &t->as<type::Function>());
        } else {
          static_cast<void>(t);
          CompleteBody(&c, node);
        }
      });
  ASSERT(success == true);
  return &iter->second;
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Access const *node) {
  if (type_of(node->operand()) == type::Module) {
    // TODO we already did this evaluation in type verification. Can't we just
    // save and reuse it?
    auto maybe_mod = EvaluateAs<module::BasicModule *>(node->operand());
    if (not maybe_mod) { NOT_YET(); }
    auto const *mod = &(*maybe_mod)->as<CompiledModule>();

    ASSERT(mod != data().module());
    auto decls = mod->ExportedDeclarations(node->member_name());
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1: return mod->ExportedValue(decls[0]);
      default: NOT_YET();
    }
  }

  auto *this_type = type_of(node);
  if (this_type->is<type::Enum>()) {
    ir::EnumVal lit =
        *this_type->as<type::Enum>().EmitLiteral(node->member_name());
    return ir::Value(lit);
  } else if (this_type->is<type::Flags>()) {
    ir::FlagsVal lit =
        *this_type->as<type::Flags>().EmitLiteral(node->member_name());
    return ir::Value(lit);
  } else if (type_of(node->operand()) == type::ByteView) {
    ASSERT(node->member_name() == "length");
    return ir::Value(builder().ByteViewLength(
        EmitValue(node->operand()).get<ir::RegOr<ir::String>>()));
  } else {
    // TODO: Can this be an address?
    return ir::Value(builder().PtrFix(EmitRef(node).reg(), this_type));
  }
}

ir::Value Compiler::EmitValue(ast::ArgumentType const *node) {
  return ir::Value(ASSERT_NOT_NULL(data().arg_type(node->name())));
}

ir::Value Compiler::EmitValue(ast::ArrayLiteral const *node) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = type_of(node);
  auto alloc      = builder().TmpAlloca(this_type);
  if (not node->empty()) {
    auto *data_type = this_type->as<type::Array>().data_type();
    for (size_t i = 0; i < node->size(); ++i) {
      EmitMoveInit(
          type::Typed(EmitValue(node->elem(i)), data_type),
          type::Typed<ir::Reg>(builder().Index(type::Ptr(this_type), alloc,
                                               static_cast<int32_t>(i)),
                               type::Ptr(data_type)));
    }
  }
  return ir::Value(alloc);
}

ir::Value Compiler::EmitValue(ast::ArrayType const *node) {
  auto result =
      EmitValue(node->data_type()).get<ir::RegOr<type::Type const *>>();
  // Size must be at least 1 by construction, so `.size() - 1` will not
  // overflow.
  for (int i = node->lengths().size() - 1; i >= 0; --i) {
    result = builder().Array(
        EmitValue(node->length(i)).get<ir::RegOr<int64_t>>(), result);
  }
  return ir::Value(result);
}

ir::Value Compiler::EmitValue(ast::Assignment const *node) {
  std::vector<type::Typed<ir::RegOr<ir::Addr>>> lhs_refs;
  lhs_refs.reserve(node->lhs().size());

  // TODO understand the precise semantics you care about here and document
  // them. Must references be computed first?
  for (auto const *l : node->lhs()) {
    lhs_refs.push_back(type::Typed<ir::RegOr<ir::Addr>>(
        EmitRef(l), ASSERT_NOT_NULL(data().qual_type(l))->type()));
  }

  auto ref_iter = lhs_refs.begin();
  for (auto const *r : node->rhs()) {
    auto rhs_qt  = *ASSERT_NOT_NULL(data().qual_type(r));
    auto rhs_val = EmitValue(r);
    if (rhs_qt.expansion_size() == 1) {
      type::Typed<ir::RegOr<ir::Addr>> ref = *ref_iter++;
      Visit(ref.type(), *ref, type::Typed{rhs_val, rhs_qt.type()},
            EmitMoveAssignTag{});
    } else {
      auto val_iter = rhs_val.get<ir::MultiValue>().span().begin();
      for (auto *t : rhs_qt.expanded()) {
        type::Typed<ir::RegOr<ir::Addr>> ref = *ref_iter++;
        Visit(ref.type(), *ref, type::Typed{*val_iter++, t},
              EmitMoveAssignTag{});
      }
    }
  }

  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::BinaryOperator const *node) {
  auto *lhs_type = type_of(node->lhs());
  auto *rhs_type = type_of(node->rhs());

  // TODO user-defined types (with a dispatch table).

  switch (node->op()) {
    case frontend::Operator::Xor: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      if (lhs_type == type::Bool) {
        return ir::Value(builder().Ne(lhs_ir.get<ir::RegOr<bool>>(),
                                      rhs_ir.get<ir::RegOr<bool>>()));
      } else if (lhs_type->is<type::Flags>()) {
        return ir::Value(
            builder().XorFlags(lhs_ir.get<ir::RegOr<ir::FlagsVal>>(),
                               rhs_ir.get<ir::RegOr<ir::FlagsVal>>()));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::And: {
      auto lhs_ir = EmitValue(node->lhs());
      if (lhs_type == type::Bool) {
       auto *land_block = builder().AddBlock();

       std::vector<ir::BasicBlock const *> phi_blocks;

       auto *next_block = builder().AddBlock();
       builder().CondJump(lhs_ir.get<ir::RegOr<bool>>(), next_block,
                          land_block);
       phi_blocks.push_back(builder().CurrentBlock());
       builder().CurrentBlock() = next_block;

       auto rhs_ir = EmitValue(node->rhs());
       phi_blocks.push_back(builder().CurrentBlock());
       builder().UncondJump(land_block);

       builder().CurrentBlock() = land_block;

       return ir::Value(builder().Phi<bool>(
           std::move(phi_blocks), {false, rhs_ir.get<ir::RegOr<bool>>()}));

      } else if (lhs_type->is<type::Flags>()) {
        auto rhs_ir = EmitValue(node->rhs());
        return ir::Value(
            builder().AndFlags(lhs_ir.get<ir::RegOr<ir::FlagsVal>>(),
                               rhs_ir.get<ir::RegOr<ir::FlagsVal>>()));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::Add: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Add(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Sub: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Sub(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Mul(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Div(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Mod(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::OrEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = EmitRef(node->lhs());
        builder().Store(
            builder().OrFlags(
                builder().Load<ir::FlagsVal>(lhs_lval),
                EmitValue(node->rhs()).get<ir::RegOr<ir::FlagsVal>>()),
            lhs_lval);
        return ir::Value();
      }
      auto *land_block = builder().AddBlock();
      auto *more_block = builder().AddBlock();

      auto lhs_val       = EmitValue(node->lhs()).get<ir::RegOr<bool>>();
      auto lhs_end_block = builder().CurrentBlock();
      builder().CondJump(lhs_val, land_block, more_block);

      builder().CurrentBlock() = more_block;
      auto rhs_val             = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
      auto rhs_end_block       = builder().CurrentBlock();
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      return ir::Value(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                           {ir::RegOr<bool>(true), rhs_val}));
    } break;
    case frontend::Operator::AndEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = EmitRef(node->lhs());
        builder().Store(
            builder().AndFlags(
                builder().Load<ir::FlagsVal>(lhs_lval),
                EmitValue(node->rhs()).get<ir::RegOr<ir::FlagsVal>>()),
            lhs_lval);
        return ir::Value();
      }

      auto *land_block = builder().AddBlock();
      auto *more_block = builder().AddBlock();

      auto lhs_val       = EmitValue(node->lhs()).get<ir::RegOr<bool>>();
      auto lhs_end_block = builder().CurrentBlock();
      builder().CondJump(lhs_val, more_block, land_block);

      builder().CurrentBlock() = more_block;
      auto rhs_val             = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
      auto rhs_end_block       = builder().CurrentBlock();
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      // TODO this looks like a bug.
      return ir::Value(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                           {rhs_val, ir::RegOr<bool>(false)}));
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Add(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Sub(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Div(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t>(rhs_type, [&](auto tag) {
        using T = typename decltype(tag)::type;
        builder().Store(builder().Div(builder().Load<T>(lhs_lval),
                                      rhs_ir.get<ir::RegOr<T>>()),
                        lhs_lval);
      });
      return ir::Value();
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Mul(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = EmitRef(node->lhs());
        auto rhs_ir   = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
        builder().Store(builder().Ne(builder().Load<bool>(lhs_lval), rhs_ir),
                        lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = EmitRef(node->lhs());
        auto rhs_ir = EmitValue(node->rhs()).get<ir::RegOr<ir::FlagsVal>>();
        builder().Store(
            builder().XorFlags(builder().Load<ir::FlagsVal>(lhs_lval), rhs_ir),
            lhs_lval);
      } else {
        UNREACHABLE(lhs_type);
      }
      return ir::Value();
    } break;
    default: UNREACHABLE(*node);
  }
  UNREACHABLE(*node);
}

ir::Value Compiler::EmitValue(ast::BlockLiteral const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  std::vector<ir::RegOr<ir::Fn>> befores;
  std::vector<ir::RegOr<ir::Jump *>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(EmitValue(decl).get<ir::RegOr<ir::Fn>>());
  }

  for (auto const &decl : node->after()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    afters.push_back(EmitValue(decl).get<ir::RegOr<ir::Jump *>>());
  }

  return ir::Value(builder().MakeBlock(data().add_block(), std::move(befores),
                                       std::move(afters)));
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

Compiler::TransientFunctionState::YieldedArguments Compiler::EmitBlockNode(
    ast::BlockNode const *node) {
  ICARUS_SCOPE(PushVec(&state_.yields)) {
    EmitIrForStatements(this, node->stmts());
    return state_.yields.back();
  }
}

ir::Value Compiler::EmitValue(ast::BlockNode const *node) {
  UNREACHABLE("Should be called via Compiler::EmitBlockNode");
}

ir::Value Compiler::EmitValue(ast::BuiltinFn const *node) {
  return ir::Value(node->value());
}

ir::Value EmitBuiltinCall(Compiler *c, ast::BuiltinFn const *callee,
                          core::FnArgs<ast::Expression const *> const &args) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      auto maybe_name         = c->EvaluateAs<ir::String>(args[0]);
      auto maybe_foreign_type = c->EvaluateAs<type::Type const *>(args[1]);
      if (not maybe_name) { NOT_YET(); }
      if (not maybe_foreign_type) { NOT_YET(); }

      return ir::Value(
          c->builder().LoadSymbol(*maybe_name, *maybe_foreign_type).get());
    } break;

    case ir::BuiltinFn::Which::Opaque:
      return ir::Value(c->builder().OpaqueType(c->data().module()));
    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::BuiltinFn::Bytes().type();
      ir::OutParams outs  = c->builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c->builder().Call(
          ir::Fn{ir::BuiltinFn::Bytes()}, &fn_type,
          {ir::Value(
              c->EmitValue(args[0]).get<ir::RegOr<type::Type const *>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      auto const &fn_type = *ir::BuiltinFn::Alignment().type();
      ir::OutParams outs  = c->builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c->builder().Call(
          ir::Fn{ir::BuiltinFn::Alignment()}, &fn_type,
          {ir::Value(
              c->EmitValue(args[0]).get<ir::RegOr<type::Type const *>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::DebugIr:
      c->builder().DebugIr();
      return ir::Value();
  }
  UNREACHABLE();
}

ir::Value Compiler::EmitValue(ast::Call const *node) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    return EmitBuiltinCall(this, b, node->args());
  }

  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  // TODO: This is a pretty terrible hack.
  if (auto const *gen_struct_type =
          ASSERT_NOT_NULL(data().qual_type(node->callee()))
              ->type()
              ->if_as<type::GenericStruct>()) {
    type::Type const *s = gen_struct_type->concrete(args);
    return ir::Value(s);
  }

  // TODO this shouldn't be able to fail.
  ASSIGN_OR(return ir::Value(),  //
                   auto os, MakeOverloadSet(this, node->callee(), args));
  return FnCallDispatchTable::Emit(this, os, args);
  // TODO node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}

ir::Value Compiler::EmitValue(ast::Cast const *node) {
  // TODO user-defined-types

  auto *to_type = ASSERT_NOT_NULL(type_of(node));
  auto results  = EmitValue(node->expr());
  if (to_type == type::Type_) {
    std::vector<type::Type const *> entries;
    if (auto const *m = results.get_if<ir::MultiValue>()) {
      entries.reserve(m->size());
      for (auto const &v : m->span()) {
        entries.push_back(v.get<type::Type const *>());
      }
    } else {
      entries.push_back(results.get<type::Type const *>());
    }
    return ir::Value(type::Tup(entries));
  }
  auto *from_type = type_of(node->expr());
  if (type::IsNumeric(from_type)) {
    if (type::IsIntegral(from_type)) {
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double,
                              ir::EnumVal, ir::FlagsVal>(
          to_type, [&](auto tag) {
            return ir::Value(builder().CastTo<typename decltype(tag)::type>(
                type::Typed(results, from_type)));
          });
    } else {
      return type::ApplyTypes<float, double>(to_type, [&](auto tag) {
        return ir::Value(builder().CastTo<typename decltype(tag)::type>(
            type::Typed(results, from_type)));
      });
    }
  } else {
    NOT_YET();
  }
}

ir::Value Compiler::EmitValue(ast::Declaration const *node) {
  // TODO: The entirety of constant-caching mechanism here is weird and broken
  // and needs a cleanup.
  DEBUG_LOG("EmitValueDeclaration")(node->id());
  if (node->flags() & ast::Declaration::f_IsConst) {
    if (node->module() != data().module()) {
      // Constant declarations from other modules should already be stored on
      // that module. They must be at the root of the binding tree map,
      // otherwise they would be local to some function/jump/etc. and not be
      // exported.
      return node->module()->as<CompiledModule>().data().Constant(node)->value;
    }

    if (node->flags() & ast::Declaration::f_IsFnParam) {
      auto val = data().LoadConstantParam(node);
      DEBUG_LOG("EmitValueDeclaration")(val);
      return val;
    } else {
      auto *constant_value = data().Constant(node);
      auto const *t        = type_of(node);
      // TODO schedule completion for structs?
      if (constant_value) { return constant_value->value; }

      if (node->IsCustomInitialized()) {
        DEBUG_LOG("EmitValueDeclaration")
        ("Computing slot with ", node->init_val()->DebugString());
        auto maybe_val =
            Evaluate(type::Typed(node->init_val(), t), state_.must_complete);
        if (not maybe_val) {
          // TODO we reserved a slot and haven't cleaned it up. Do we care?
          NOT_YET("Found errors but haven't handled them.",
                  diag().num_consumed());
          return ir::Value();
        }

        DEBUG_LOG("EmitValueDeclaration")("Setting slot = ", *maybe_val);
        data().SetConstant(node, *maybe_val);

        // TODO: This is a struct-speficic hack.
        if (type::Type const **type_val =
                maybe_val->get_if<type::Type const *>()) {
          if (auto const *struct_type = (*type_val)->if_as<type::Struct>()) {
            if (not struct_type->complete_) { return *maybe_val; }
            data().CompleteConstant(node);
          }
        }

        return *maybe_val;

      } else if (node->IsDefaultInitialized()) {
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
      UNREACHABLE(node->DebugString());
    }
  } else {
    if (node->IsUninitialized()) { return ir::Value(); }
    auto *t = type_of(node);
    auto a  = data().addr(node);
    if (node->IsCustomInitialized()) {
      EmitMoveInit(node->init_val(), type::Typed(a, type::Ptr(t)));
    } else {
      if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
        Visit(t, a, EmitDefaultInitTag{});
      }
    }
    return ir::Value(a);
  }
  UNREACHABLE();
}

ir::Value Compiler::EmitValue(ast::DesignatedInitializer const *node) {
  // TODO actual initialization with these field members.
  auto *t    = type_of(node);
  auto alloc = builder().TmpAlloca(t);

  auto &struct_type  = t->as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().token();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    Visit(field.type, builder().Field(alloc, &struct_type, i).get(),
          EmitDefaultInitTag{});
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    NOT_YET();
    // auto *f            = struct_type.field(field);
    // size_t field_index = struct_type.index(f->name);
    // EmitMoveInit(expr.get(), builder().Field(alloc, &struct_type,
    // field_index));
  }
  return ir::Value(alloc);
}

ir::Value Compiler::EmitValue(ast::EnumLiteral const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

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
            EmitValue(decl->init_val()).get<ir::RegOr<enum_t>>());
      }
    }
  }

  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum:
      return ir::Value(
          builder().Enum(data().module(), names, specified_values));
    case ast::EnumLiteral::Kind::Flags:
      return ir::Value(
          builder().Flags(data().module(), names, specified_values));
    default: UNREACHABLE();
  }
}

template <typename NodeType>
ir::NativeFn MakeConcreteFromGeneric(
    Compiler *compiler, NodeType const *node,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  ASSERT(node->is_generic() == true);

  // Note: Cannot use structured bindings because the bindings need to be
  // captured in the lambda.

  // TODO: Rather than recompute this we colud store the `Call` node in the
  // dependent context.
  DependentComputedData temp_data(compiler->data().module());
  Compiler c({
      .builder             = compiler->builder(),
      .data                = temp_data,
      .diagnostic_consumer = compiler->diag(),
  });
  temp_data.parent_ = &compiler->data();
  auto params =
      c.ComputeParamsFromArgs(node, OrderedDependencyNodes(node), args);
  auto find_dependent_result = compiler->data().FindDependent(node, params);
  auto const *fn_type        = find_dependent_result.fn_type;
  auto &data                 = find_dependent_result.data;

  return data.EmplaceNativeFn(node, [&]() {
    ir::NativeFn f(
        &data.fns_, fn_type,
        node->params().Transform([fn_type, i = 0](auto const &d) mutable {
          return type::Typed<ast::Declaration const *>(
              d.get(), fn_type->params()[i++].value.type());
        }));
    f->work_item = DeferBody({.builder             = compiler->builder(),
                              .data                = data,
                              .diagnostic_consumer = compiler->diag()},
                             node, fn_type);
    return f;
  });
}

ir::Value Compiler::EmitValue(ast::ShortFunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::FnArgs<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); });
    return ir::Value(gen_fn);
  }

  ir::NativeFn ir_func = data().EmplaceNativeFn(node, [&] {
    auto *fn_type = &type_of(node)->as<type::Function>();
    auto f        = AddFunc(
        fn_type,
        node->params().Transform([fn_type, i = 0](auto const &d) mutable {
          return type::Typed<ast::Declaration const *>(
              d.get(), fn_type->params()[i++].value.type());
        }));
    f->work_item = DeferBody(resources_, node, fn_type);
    return f;
  });
  return ir::Value(ir::Fn{ir_func});
}

ir::Value Compiler::EmitValue(ast::FunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::FnArgs<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); });
    return ir::Value(gen_fn);
  }

  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  // TODO Use correct constants
  ir::NativeFn ir_func = data().EmplaceNativeFn(node, [&] {
    auto *fn_type = &type_of(node)->as<type::Function>();
    auto f        = AddFunc(
        fn_type,
        node->params().Transform([fn_type, i = 0](auto const &d) mutable {
          return type::Typed<ast::Declaration const *>(
              d.get(), fn_type->params()[i++].value.type());
        }));
    f->work_item = DeferBody(resources_, node, fn_type);
    return f;
  });
  return ir::Value(ir::Fn{ir_func});
}

ir::Value Compiler::EmitValue(ast::FunctionType const *node) {
  std::vector<ir::RegOr<type::Type const *>> param_vals, out_vals;
  param_vals.reserve(node->params().size());
  out_vals.reserve(node->outputs().size());
  for (auto const *p : node->params()) {
    param_vals.push_back(EmitValue(p).get<ir::RegOr<type::Type const *>>());
  }
  for (auto const *o : node->outputs()) {
    out_vals.push_back(EmitValue(o).get<ir::RegOr<type::Type const *>>());
  }

  return ir::Value(builder().Arrow(std::move(param_vals), std::move(out_vals)));
}

ir::Value Compiler::EmitValue(ast::Identifier const *node) {
  auto decl_span = data().decls(node);
  ASSERT(decl_span.size() != 0u);
  if (decl_span[0]->flags() & ast::Declaration::f_IsConst) {
    return EmitValue(decl_span[0]);
  }
  if (decl_span[0]->flags() & ast::Declaration::f_IsFnParam) {
    auto *t     = type_of(node);
    ir::Reg reg = data().addr(decl_span[0]);
    return (decl_span[0]->flags() & ast::Declaration::f_IsOutput) and
                   not t->is_big()
               ? builder().Load(reg, t)
               : ir::Value(reg);
  } else {
    auto *t   = ASSERT_NOT_NULL(type_of(node));
    auto lval = EmitRef(node);
    if (not lval.is_reg()) { NOT_YET(); }
    return ir::Value(builder().PtrFix(lval.reg(), t));
  }
}

ir::Value Compiler::EmitValue(ast::Import const *node) {
  // Note: Even though this must return a more specific type (LibraryModule
  // instead of BasicModule), we use Type to ensure that if this gets routed
  // into an ir::Value, it will be tagged correctly.
  //
  // TODO compiler doesn't know about inheritence here.
  return ir::Value(reinterpret_cast<module::BasicModule *>(
      ASSERT_NOT_NULL(data().imported_module(node))));
}

ir::Value Compiler::EmitValue(ast::Index const *node) {
  if (type_of(node->lhs()) == type::ByteView) {
    auto data = builder().ByteViewData(
        EmitValue(node->lhs()).get<ir::RegOr<ir::String>>());
    auto addr = builder().PtrIncr(
        data, EmitValue(node->rhs()).get<ir::RegOr<int64_t>>(),
        type::Ptr(type::Nat8));
    return builder().Load(addr, type::Nat8);
  }
  return ir::Value(builder().PtrFix(EmitRef(node).reg(), type_of(node)));
}

void EmitJump(Compiler *c, absl::Span<ast::JumpOption const> options) {
  std::vector<std::string_view> names;
  names.reserve(options.size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(options.size());

  std::vector<core::FnArgs<type::Typed<ir::Value>>> args;
  args.reserve(options.size());

  auto current_block = c->builder().CurrentBlock();

  for (auto const &opt : options) {
    ir::BasicBlock *block = c->builder().AddBlock();
    blocks.push_back(block);
    names.push_back(opt.block());

    c->builder().CurrentBlock() = block;

    args.push_back(opt.args().Transform([c](auto const &expr) {
      return type::Typed(c->EmitValue(expr.get()), c->type_of(expr.get()));
    }));
  }

  c->builder().CurrentBlock() = current_block;
  c->builder().ChooseJump(std::move(names), std::move(blocks), std::move(args));
}

ir::Value Compiler::EmitValue(ast::ConditionalGoto const *node) {
  auto condition    = EmitValue(node->condition());
  auto *true_block  = builder().AddBlock();
  auto *false_block = builder().AddBlock();
  builder().CondJump(condition.get<ir::RegOr<bool>>(), true_block, false_block);

  builder().CurrentBlock() = true_block;
  EmitJump(this, node->true_options());

  builder().CurrentBlock() = false_block;
  EmitJump(this, node->false_options());

  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::UnconditionalGoto const *node) {
  EmitJump(this, node->options());
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::Label const *node) {
  return ir::Value(/* TODO node->value() */);
}

ir::Value Compiler::EmitValue(ast::Jump const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  return ir::Value(data().add_jump(node, [this, node] {
    auto *jmp_type     = &type_of(node)->as<type::Jump>();
    auto work_item_ptr = DeferBody(resources_, node, jmp_type);

    size_t i    = 0;
    auto params = node->params().Transform([&](auto const &decl) {
      return type::Typed<ast::Declaration const *>(
          decl.get(), jmp_type->params()[i++].value);
    });

    DEBUG_LOG("Jump")("Jump type = ", jmp_type->to_string());
    ir::Jump jmp(jmp_type, std::move(params));
    if (work_item_ptr) { jmp.work_item = work_item_ptr; }
    return jmp;
  }));
}

static std::vector<std::pair<ast::Expression const *, ir::Value>>
EmitValueWithExpand(Compiler *c, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, ir::Value>> vals;
  for (auto *expr : exprs) { vals.emplace_back(expr, c->EmitValue(expr)); }
  return vals;
}

ir::Value Compiler::EmitValue(ast::ReturnStmt const *node) {
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
      EmitMoveInit(type::Typed(arg_vals[i].second, ret_type),
                   type::Typed<ir::Reg>(builder().GetRet(i, ret_type),
                                        type::Ptr(ret_type)));

    } else {
      builder().SetRet(i, type::Typed{arg_vals[i].second, ret_type});
    }
  }

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  auto *scope = node->scope();
  while (auto *exec = scope->if_as<ast::ExecScope>()) {
    MakeAllDestructions(this, exec);
    if (not exec->parent or exec->is<ast::FnScope>()) { break; }
    scope = exec->parent;
  }

  builder().ReturnJump();
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::YieldStmt const *node) {
  auto arg_vals = EmitValueWithExpand(this, node->exprs());
  // TODO store this as an exec_scope.
  MakeAllDestructions(this, &node->scope()->as<ast::ExecScope>());
  // TODO pretty sure this is all wrong.

  // Can't return these because we need to pass them up at least through the
  // containing statements this and maybe further if we allow labelling
  // scopes to be yielded to.
  auto &yielded_args = state_.yields.back();

  // TODO one problem with this setup is that we look things up in a context
  // after returning, so the `after` method has access to a different
  // (smaller) collection of bound constants. This can change the meaning of
  // things or at least make them not compile if the `after` function takes
  // a compile-time constant argument.
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    auto qt = qual_type_of(node->exprs()[i]);
    ASSERT(qt.has_value() == true);
    yielded_args.vals.pos_emplace(arg_vals[i].second, *qt);
  }
  yielded_args.label = node->label() ? node->label()->value() : ir::Label{};

  builder().block_termination_state() =
      node->label() ? ir::Builder::BlockTerminationState::kLabeledYield
                    : ir::Builder::BlockTerminationState::kYield;
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::ScopeLiteral const *node) {
  DEBUG_LOG("ScopeLiteral")(node->state_type());
  type::Type const *state_type = nullptr;
  if (node->state_type()) {
    auto maybe_type = EvaluateAs<type::Type const *>(node->state_type());
    if (not maybe_type) { NOT_YET(); }
    state_type = *maybe_type;
  }

  absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks;
  std::vector<ir::RegOr<ir::Jump *>> inits;
  std::vector<ir::RegOr<ir::Fn>> dones;
  for (auto const &decl : node->decls()) {
    if (decl.id() == "init") {
      inits.push_back(EmitValue(&decl).get<ir::RegOr<ir::Jump *>>());
    } else if (decl.id() == "done") {
      dones.push_back(EmitValue(&decl).get<ir::RegOr<ir::Fn>>());
    } else {
      blocks.emplace(decl.id(),
                     EmitValue(&decl).get<ir::RegOr<ir::BlockDef *>>().value());
    }
  }

  return ir::Value(builder().MakeScope(
      data().add_scope(data().module(), state_type), std::move(inits),
      std::move(dones), std::move(blocks)));
}

ir::Value Compiler::EmitValue(ast::ScopeNode const *node) {
  DEBUG_LOG("ScopeNode")("Emitting IR for ScopeNode");

  // Jump to a new block in case some scope ends up with `goto start()` in order
  // to re-evealuate arguments.
  auto *args_block = builder().AddBlock();
  builder().UncondJump(args_block);
  builder().CurrentBlock() = args_block;

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  ASSIGN_OR(return ir::Value(),  //
                   auto os, MakeOverloadSet(this, node->name(), args));

  auto inits = MakeJumpInits(this, os);
  DEBUG_LOG("ScopeNode")(inits);

  ASSIGN_OR(
      return ir::Value(),  //
             auto table,
             ScopeDispatchTable::Verify(this, node, std::move(inits), args));

  return table.EmitCall(this, args);
}

void Compiler::CompleteStruct(ast::StructLiteral const *node) {
  DEBUG_LOG("struct")
  ("Completing struct-literal emission: ", node,
   " must-complete = ", state_.must_complete);

  type::Struct *s = data().get_struct(node);
  if (s->complete_) {
    DEBUG_LOG("struct")("Already complete, exiting: ", node);
    return;
  }

  ir::CompiledFn fn(type::Func({}, {}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(&fn, &builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    builder().CurrentBlock() = fn.entry();

    std::vector<ir::StructField> fields;
    fields.reserve(node->fields().size());

    std::optional<ir::Fn> dtor;
    for (auto const &field : node->fields()) {
      // TODO hashtags.
      if (field.id() == "destroy") {
        // TODO handle potential errors here.
        ASSIGN_OR(
            continue,  //
            auto dtor_value,
            Evaluate(type::Typed(
                field.init_val(),
                type::Func({core::AnonymousParam(
                               type::QualType::NonConstant(type::Ptr(s)))},
                           {}))));
        ASSIGN_OR(continue, dtor, dtor_value.get_if<ir::Fn>());
      } else {
        if (auto *init_val = field.init_val()) {
          // TODO init_val type may not be the same.
          auto *t = ASSERT_NOT_NULL(qual_type_of(init_val)->type());
          fields.emplace_back(field.id(), t, EmitValue(init_val));
        } else {
          fields.emplace_back(
              field.id(),
              EmitValue(field.type_expr()).get<type::Type const *>());
        }
      }
    }
    builder().Struct(data().module(), s, std::move(fields), dtor);
    builder().ReturnJump();
  }

  // TODO: What if execution fails.
  fn.WriteByteCode();
  interpretter::Execute(std::move(fn));
  DEBUG_LOG("struct")
  ("Completed ", node->DebugString(), " which is a struct ", *s, " with ",
   s->fields().size(), " field(s).");
}

ir::Value Compiler::EmitValue(ast::StructLiteral const *node) {
  DEBUG_LOG("struct")
  ("Starting struct-literal emission: ", node,
   state_.must_complete ? " (must complete)" : " (need not complete)");

  if (type::Struct *s = data().get_struct(node)) {
    return ir::Value(static_cast<type::Type const *>(s));
  }

  type::Struct *s = type::Allocate<type::Struct>(
      data().module(), type::Struct::Options{
                           .is_copyable = not node->contains_hashtag(
                               ast::Hashtag(ast::Hashtag::Builtin::Uncopyable)),
                           .is_movable = not node->contains_hashtag(
                               ast::Hashtag(ast::Hashtag::Builtin::Immovable)),
                       });

  DEBUG_LOG("struct")("Allocating a new struct ", s, " for ", node);
  data().set_struct(node, s);

  // Note: VerifyBody may end up triggering EmitValue calls for member types
  // that depend on this incomplete type. For this reason it is important that
  // we have already allocated the struct so we do not get a double-allocation.
  //
  // The process, as you can see above is to
  // 1. Check if it has already been allocated. Return if it has been.
  // 2. Allocated ourselves.
  // 3. Start body verification.
  // 4. Schedule completion.
  //
  // Notably, steps 1 and 2 must not be separated. Moreover, because body
  // verification could end up calling this function again, we must "set up
  // guards" (i.e., steps 1 and 2) before step 3 runs.
  //
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  if (state_.must_complete) {
    state_.work_queue.emplace(node,
                              TransientFunctionState::WorkType::CompleteStruct);
  }
  return ir::Value(static_cast<type::Type const *>(s));
}

ir::Value Compiler::EmitValue(ast::ParameterizedStructLiteral const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }
  // NOT_YET();
  // TODO: At least right now with the hacky version we don't look at this.

  return ir::Value(data().qual_type(node)->type());
}

ir::Value Compiler::EmitValue(ast::StructType const *node) { NOT_YET(); }

ir::Value Compiler::EmitValue(ast::Switch const *node) {
  auto *land_block = builder().AddBlock();
  auto *t          = type_of(node);
  // TODO this is not precisely accurate if you have regular void.
  bool all_paths_jump = (t == type::Void());

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. this is very wrong.

  // TODO handle switching on tuples/multiple values?
  ir::Value expr_results;
  type::Type const *expr_type = nullptr;
  if (node->expr()) {
    expr_results = EmitValue(node->expr());
    expr_type    = type_of(node->expr());
  }

  absl::flat_hash_map<ir::BasicBlock *, ir::Value> phi_args;
  for (size_t i = 0; i + 1 < node->cases().size(); ++i) {
    auto &[body, match_cond] = node->cases()[i];
    auto *expr_block         = builder().AddBlock();
    auto *match_cond_ptr     = match_cond.get();
    ir::Value match_val      = EmitValue(match_cond_ptr);
    ir::RegOr<bool> cond     = [&] {
      if (node->expr()) {
        ASSERT(expr_type == type_of(match_cond_ptr));
        return builder().Eq(expr_type, match_val, expr_results);
      } else {
        return match_val.get<ir::RegOr<bool>>();
      }
    }();

    auto next_block = builder().EarlyExitOn<true>(expr_block, cond);

    builder().CurrentBlock() = expr_block;
    if (body->is<ast::Expression>()) {
      phi_args.emplace(builder().CurrentBlock(), EmitValue(body.get()));
      builder().UncondJump(land_block);
    } else {
      // It must be a jump/yield/return, which we've verified in VerifyType.
      EmitValue(body.get());

      if (not all_paths_jump) {
        builder().block_termination_state() =
            ir::Builder::BlockTerminationState::kMoreStatements;
      }
    }

    builder().CurrentBlock() = next_block;
  }

  if (node->cases().back().first->is<ast::Expression>()) {
    phi_args.emplace(builder().CurrentBlock(),
                     EmitValue(node->cases().back().first.get()));
    builder().UncondJump(land_block);
  } else {
    // It must be a jump/yield/return, which we've verified in VerifyType.
    EmitValue(node->cases().back().first.get());
    if (not all_paths_jump) {
      builder().block_termination_state() =
          ir::Builder::BlockTerminationState::kMoreStatements;
    }
  }

  builder().CurrentBlock() = land_block;
  if (t == type::Void()) {
    return ir::Value();
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
        vals.push_back(val.template get<ir::RegOr<T>>());
      }
      return ir::Value(builder().Phi<T>(blocks, vals));
    });
    return r;
  }
}

ir::Value Compiler::EmitValue(ast::Terminal const *node) {
  return node->value();
}

ir::Value Compiler::EmitValue(ast::UnaryOperator const *node) {
  auto *operand_type = type_of(node->operand());
  // TODO user-defined-types

  switch (node->op()) {
    case frontend::Operator::Copy: {
      auto reg = builder().TmpAlloca(operand_type);
      EmitCopyInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, operand_type));
      return ir::Value(reg);
    } break;
    case frontend::Operator::Move: {
      auto reg = builder().TmpAlloca(operand_type);
      EmitMoveInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, operand_type));
      return ir::Value(reg);
    } break;
    case frontend::Operator::BufPtr:
      return ir::Value(builder().BufPtr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));
    case frontend::Operator::Not: {
      auto *t = type_of(node->operand());
      if (t == type::Bool) {
        return ir::Value(
            builder().Not(EmitValue(node->operand()).get<ir::RegOr<bool>>()));
      } else if (t->is<type::Flags>()) {
        return ir::Value(builder().XorFlags(
            EmitValue(node->operand()).get<ir::RegOr<ir::FlagsVal>>(),
            ir::FlagsVal{t->as<type::Flags>().All}));

      } else {
        NOT_YET();
      }
    } break;
    case frontend::Operator::Sub: {
      auto operand_ir = EmitValue(node->operand());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          type_of(node->operand()), [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Neg(operand_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::TypeOf: return ir::Value(type_of(node->operand()));
    case frontend::Operator::Which:
      return ir::Value(builder().Load<type::Type const *>(
          builder().VariantType(EmitValue(node->operand()).get<ir::Reg>())));
    case frontend::Operator::And: return ir::Value(EmitRef(node->operand()));
    case frontend::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      auto maybe_val =
          Evaluate(type::Typed(node->operand(), type_of(node->operand())));
      if (not maybe_val) { NOT_YET(); }
      return *maybe_val;
    }
    case frontend::Operator::Mul: {
      state_.must_complete = false;

      ir::Value value(builder().Ptr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));

      state_.must_complete = true;

      return value;
    } break;
    case frontend::Operator::At: {
      auto *t = type_of(node);
      return builder().Load(
          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(), t);
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
