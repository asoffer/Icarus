#include "absl/algorithm/container.h"

#include <iostream>
#include <optional>
#include <string_view>

#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/extract_jumps.h"
#include "compiler/library_module.h"
#include "core/call.h"
#include "diagnostic/errors.h"
#include "frontend/lex/operators.h"
#include "interpretter/evaluate.h"
#include "ir/compiled_fn.h"
#include "ir/value/generic_fn.h"
#include "type/cast.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/overload_set.h"
#include "type/parameter_pack.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace compiler {
namespace {

template <typename Fn>
void AddAdl(std::string_view id, type::Type const *t, Fn fn) {
  absl::flat_hash_set<CompiledModule *> modules;
  // TODO t->ExtractDefiningModules(&modules);

  for (auto *mod : modules) {
    auto decls = mod->ExportedDeclarations(id);

    auto const &data = mod->data();
    for (auto *d : decls) {
      ASSIGN_OR(continue, auto qt, data.qual_type(d));
      ASSIGN_OR(continue, auto &t, qt.type());
      if (not fn(d)) { return; }
    }
  }
}

type::QualType VerifyUnaryOverload(Compiler *c, char const *symbol,
                                   ast::Expression const *node,
                                   type::Type const *operand_type) {
  type::Quals quals = type::Quals::All();
  absl::flat_hash_set<type::Callable const *> member_types;

  auto extract_callable_type = [&](ast::Expression const *expr) {
    ASSIGN_OR(return false, auto qt, c->qual_type_of(expr));
    if (auto *c = qt.type()->if_as<type::Callable>()) {
      quals &= qt.quals();
      auto [iter, inserted] = member_types.insert(c);
      // TODO currently because of ADL, it's possible to have overload
      // sets that want to have the same type appear more than once. I
      // don't yet know how I want to deal with this.
      if (not inserted) { NOT_YET(); }
      member_types.insert(c);
    } else {
      NOT_YET();
    }
    return true;
  };

  module::ForEachDeclTowardsRoot(node->scope(), symbol, extract_callable_type);
  AddAdl(symbol, operand_type, extract_callable_type);

  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(ir::Value(), operand_type);

  return c->data().set_qual_type(
      node,
      type::QualType(type::MakeOverloadSet(std::move(member_types))
                         ->return_types(core::FnArgs<type::Typed<ir::Value>>(
                             std::move(pos_args), {})),
                     quals));
}

type::QualType VerifyBinaryOverload(Compiler *c, char const *symbol,
                                    ast::Expression const *node,
                                    type::Type const *lhs_type,
                                    type::Type const *rhs_type) {
  type::Quals quals = type::Quals::All();
  absl::flat_hash_set<type::Callable const *> member_types;

  auto extract_callable_type = [&](ast::Expression const *expr) {
    ASSIGN_OR(return false, auto qt, c->qual_type_of(expr));
    if (auto *c = qt.type()->if_as<type::Callable>()) {
      quals &= qt.quals();
      auto [iter, inserted] = member_types.insert(c);
      // TODO currently because of ADL, it's possible to have overload
      // sets that want to have the same type appear more than once. I
      // don't yet know how I want to deal with this.
      if (not inserted) { NOT_YET(); }
      member_types.insert(c);
    } else {
      NOT_YET();
    }
    return true;
  };

  module::ForEachDeclTowardsRoot(node->scope(), symbol, extract_callable_type);
  AddAdl(symbol, lhs_type, extract_callable_type);
  AddAdl(symbol, rhs_type, extract_callable_type);

  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(ir::Value(), lhs_type);
  pos_args.emplace_back(ir::Value(), rhs_type);

  return c->data().set_qual_type(
      node,
      type::QualType(type::MakeOverloadSet(std::move(member_types))
                         ->return_types(core::FnArgs<type::Typed<ir::Value>>(
                             std::move(pos_args), {})),
                     quals));
}

// NOTE: the order of these enumerators is meaningful and relied upon! They are
// ordered from strongest relation to weakest.
enum class Cmp { Order, Equality, None };

Cmp Comparator(type::Type const *t);

template <typename TypeContainer>
Cmp MinCmp(TypeContainer const &c) {
  using cmp_t = std::underlying_type_t<Cmp>;
  return static_cast<Cmp>(absl::c_accumulate(
      c, static_cast<cmp_t>(Cmp::Equality), [](cmp_t val, type::Type const *t) {
        return std::min(val, static_cast<cmp_t>(Comparator(t)));
      }));
}

Cmp Comparator(type::Type const *t) {
  using cmp_t = std::underlying_type_t<Cmp>;
  if (auto *v = t->if_as<type::Variant>()) { return MinCmp(v->variants_); }
  if (auto *tup = t->if_as<type::Tuple>()) { return MinCmp(tup->entries_); }
  if (auto *a = t->if_as<type::Array>()) {
    return static_cast<Cmp>(
        std::min(static_cast<cmp_t>(Comparator(a->data_type())),
                 static_cast<cmp_t>(Cmp::Equality)));
  } else if (auto *p = t->if_as<type::Primitive>()) {
    return type::IsNumeric(p) ? Cmp::Order : Cmp::Equality;
  } else if (t->is<type::Flags>() or t->is<type::BufferPointer>()) {
    return Cmp::Order;
  } else if (t->is<type::Enum>() or t->is<type::Pointer>()) {
    return Cmp::Equality;
  } else {
    return Cmp::None;
  }
}

}  // namespace

// TODO there's not that much shared between the inferred and uninferred cases,
// so probably break them out.
type::QualType VerifyBody(Compiler *c, ast::FunctionLiteral const *node,
                          type::Type const *t = nullptr) {
  if (not t) { t = ASSERT_NOT_NULL(c->type_of(node)); }
  for (auto const *stmt : node->stmts()) { c->VerifyType(stmt); }
  // TODO propogate cyclic dependencies.

  // TODO we can have yields and returns, or yields and jumps, but not jumps and
  // returns. Check this.
  absl::flat_hash_set<type::Type const *> types;
  absl::flat_hash_map<ast::ReturnStmt const *, type::Type const *>
      saved_ret_types;
  for (auto const *n : c->data().extraction_map_[node]) {
    if (auto const *ret_node = n->if_as<ast::ReturnStmt>()) {
      std::vector<type::Type const *> ret_types;
      for (auto const *expr : ret_node->exprs()) {
        ret_types.push_back(ASSERT_NOT_NULL(c->type_of(expr)));
      }
      auto *t = type::Tup(std::move(ret_types));
      types.emplace(t);

      saved_ret_types.emplace(ret_node, t);
    } else {
      UNREACHABLE();  // TODO
    }
  }

  auto type_params = node->params().Transform([&](auto const &param) {
    auto maybe_qt = c->qual_type_of(param.get());
    ASSERT(maybe_qt != std::nullopt);
    return *maybe_qt;
  });

  if (not node->outputs()) {
    std::vector<type::Type const *> output_type_vec(
        std::make_move_iterator(types.begin()),
        std::make_move_iterator(types.end()));

    if (types.size() > 1) { NOT_YET("log an error"); }
    auto f = type::Func(std::move(type_params), std::move(output_type_vec));
    return c->data().set_qual_type(node, type::QualType::Constant(f));

  } else {
    auto *node_type = t;
    auto outs       = ASSERT_NOT_NULL(node_type)->as<type::Function>().output();
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *n : c->data().extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            if (not ret_node->exprs().empty()) {
              c->diag().Consume(diagnostic::NoReturnTypes{
                  .range = ret_node->range(),
              });
              err = true;
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? type::QualType::Error()
                   : type::QualType::Constant(node_type);
      } break;
      case 1: {
        bool err = false;
        for (auto *n : c->data().extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *t = ASSERT_NOT_NULL(saved_ret_types[ret_node]);
            if (t == outs[0]) { continue; }
            c->diag().Consume(diagnostic::ReturnTypeMismatch{
                .actual   = t,
                .expected = outs[0],
                .range    = ret_node->range(),
            });
            err = true;
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? type::QualType::Error()
                   : type::QualType::Constant(node_type);
      } break;
      default: {
        for (auto *n : c->data().extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *expr_type = ASSERT_NOT_NULL(saved_ret_types[ret_node]);
            if (expr_type->is<type::Tuple>()) {
              auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
              if (tup_entries.size() != outs.size()) {
                c->diag().Consume(diagnostic::ReturningWrongNumber{
                    .actual   = (expr_type->is<type::Tuple>()
                                   ? expr_type->as<type::Tuple>().size()
                                   : 1),
                    .expected = outs.size(),
                    .range    = ret_node->range(),
                });
                return type::QualType::Error();
              } else {
                bool err = false;
                for (size_t i = 0; i < tup_entries.size(); ++i) {
                  if (tup_entries[i] != outs[i]) {
                    // TODO if this is a commalist we can point to it more
                    // carefully but if we're just passing on multiple return
                    // values it's harder.
                    //
                    // TODO point the span to the correct entry which may be
                    // hard if it's splatted.
                    c->diag().Consume(diagnostic::IndexedReturnTypeMismatch{
                        .index    = i,
                        .actual   = outs[i],
                        .expected = tup_entries[i],
                        .range    = ret_node->range(),
                    });
                    err = true;
                  }
                }
                if (err) { return type::QualType::Error(); }
              }
            } else {
              c->diag().Consume(diagnostic::ReturningWrongNumber{
                  .actual   = (expr_type->is<type::Tuple>()
                                 ? expr_type->as<type::Tuple>().size()
                                 : 1),
                  .expected = outs.size(),
                  .range    = ret_node->range(),
              });
              return type::QualType::Error();
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return type::QualType::Constant(node_type);
      } break;
    }
  }
}

std::optional<core::Params<type::QualType>> VerifyParams(
    Compiler *c,
    core::Params<std::unique_ptr<ast::Declaration>> const &params) {
  // Parameter types cannot be dependent in concrete implementations so it is
  // safe to verify each of them separately (to generate more errors that are
  // likely correct).
  core::Params<type::QualType> type_params;
  type_params.reserve(params.size());
  bool err = false;
  for (auto &d : params) {
    ASSIGN_OR(
        {
          err = true;
          continue;
        },
        auto result, c->VerifyType(d.value.get()));
    type_params.append(d.name, result, d.flags);
  }
  if (err) { return std::nullopt; }
  return type_params;
}

type::QualType Compiler::VerifyConcreteFnLit(ast::FunctionLiteral const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, VerifyParams(this, node->params()));

  std::vector<type::Type const *> output_type_vec;
  bool error   = false;
  auto outputs = node->outputs();
  if (outputs) {
    output_type_vec.reserve(outputs->size());
    for (auto *output : *outputs) {
      auto result = VerifyType(output);
      output_type_vec.push_back(result.type());
      if (result.type() != nullptr and not result.constant()) {
        // TODO this feels wrong because output could be a decl. And that decl
        // being a const decl isn't what I care about.
        NOT_YET("log an error");
        error = true;
      }
    }
  }

  if (error or absl::c_any_of(output_type_vec, [](type::Type const *t) {
        return t == nullptr;
      })) {
    return type::QualType::Error();
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (diag().num_consumed() > 0) { return type::QualType::Error(); }

  if (outputs) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = (*outputs)[i]->if_as<ast::Declaration>()) {
        output_type_vec[i] = type_of(decl);
      } else {
        ASSERT(output_type_vec[i] == type::Type_);
        auto maybe_type = EvaluateAs<type::Type const *>((*outputs)[i]);
        if (not maybe_type) { NOT_YET(); }
        output_type_vec[i] = *maybe_type;
      }
    }

    return data().set_qual_type(
        node, type::QualType::Constant(
                  type::Func(std::move(params), std::move(output_type_vec))));
  } else {
    return data().set_qual_type(node, VerifyBody(this, node));
  }
}

static std::optional<type::Quals> VerifyAndGetQuals(
    Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  bool err          = false;
  type::Quals quals = type::Quals::All();
  for (auto *expr : exprs) {
    auto r = v->VerifyType(expr);
    err |= not r.ok();
    if (not err) { quals &= r.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

type::QualType Compiler::VerifyType(ast::ArgumentType const *node) {
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::VerifyType(ast::Binop const *node) {
  auto lhs_qual_type = VerifyType(node->lhs());
  auto rhs_qual_type = VerifyType(node->rhs());
  if (not lhs_qual_type.ok() or not rhs_qual_type.ok()) {
    return type::QualType::Error();
  }

  using frontend::Operator;
  switch (node->op()) {
    case Operator::XorEq:
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return data().set_qual_type(node, lhs_qual_type);
      } else {
        diag().Consume(diagnostic::XorEqNeedsBoolOrFlags{
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    case Operator::AndEq:
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return data().set_qual_type(node, lhs_qual_type);
      } else {
        diag().Consume(diagnostic::AndEqNeedsBoolOrFlags{
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    case Operator::OrEq:
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return data().set_qual_type(node, lhs_qual_type);
      } else {
        diag().Consume(diagnostic::OrEqNeedsBoolOrFlags{
            .range = node->range(),
        });
        return type::QualType::Error();
      }

#define CASE(symbol, return_type)                                              \
  do {                                                                         \
    (lhs_qual_type.quals() & rhs_qual_type.quals());                           \
    auto quals =                                                               \
        type::Quals::Const() & lhs_qual_type.quals() & rhs_qual_type.quals();  \
    if (type::IsNumeric(lhs_qual_type.type()) and                              \
        type::IsNumeric(rhs_qual_type.type())) {                               \
      if (lhs_qual_type.type() == rhs_qual_type.type()) {                      \
        return data().set_qual_type(node,                                      \
                                    type::QualType((return_type), quals));     \
      } else {                                                                 \
        diag().Consume(diagnostic::ArithmeticBinaryOperatorTypeMismatch{       \
            .lhs_type = lhs_qual_type.type(),                                  \
            .rhs_type = rhs_qual_type.type(),                                  \
            .range    = node->range(),                                         \
        });                                                                    \
        return type::QualType::Error();                                        \
      }                                                                        \
    } else {                                                                   \
      /* TODO Support calling with constants */                                \
      return VerifyBinaryOverload(this, symbol, node, lhs_qual_type.type(),    \
                                  rhs_qual_type.type());                       \
    }                                                                          \
  } while (false)

    case Operator::Sub: {
      CASE("-", lhs_qual_type.type());
    } break;
    case Operator::Mul: {
      CASE("*", lhs_qual_type.type());
    } break;
    case Operator::Div: {
      CASE("/", lhs_qual_type.type());
    } break;
    case Operator::Mod: {
      CASE("%", lhs_qual_type.type());
    } break;
    case Operator::SubEq: {
      CASE("-=", type::Void());
    } break;
    case Operator::MulEq: {
      CASE("*=", type::Void());
    } break;
    case Operator::DivEq: {
      CASE("/=", type::Void());
    } break;
    case Operator::ModEq: {
      CASE("%=", type::Void());
    } break;
#undef CASE
    case Operator::Add: {
      auto quals =
          type::Quals::Const() & lhs_qual_type.quals() & rhs_qual_type.quals();
      if (type::IsNumeric(lhs_qual_type.type()) and
          type::IsNumeric(rhs_qual_type.type())) {
        if (lhs_qual_type.type() == rhs_qual_type.type()) {
          return data().set_qual_type(
              node, type::QualType(lhs_qual_type.type(), quals));
        } else {
          diag().Consume(diagnostic::ArithmeticBinaryOperatorTypeMismatch{
              .lhs_type = lhs_qual_type.type(),
              .rhs_type = rhs_qual_type.type(),
              .range    = node->range()});
          return type::QualType::Error();
        }
      } else {
        // TODO support calling with constants.
        return VerifyBinaryOverload(this, "+", node, lhs_qual_type.type(),
                                    rhs_qual_type.type());
      }
    } break;
    case Operator::AddEq: {
      auto quals =
          type::Quals::Const() & lhs_qual_type.quals() & rhs_qual_type.quals();
      if (type::IsNumeric(lhs_qual_type.type()) and
          type::IsNumeric(rhs_qual_type.type())) {
        if (lhs_qual_type.type() == rhs_qual_type.type()) {
          return data().set_qual_type(node,
                                      type::QualType(type::Void(), quals));
        } else {
          diag().Consume(diagnostic::ArithmeticBinaryOperatorTypeMismatch{
              .lhs_type = lhs_qual_type.type(),
              .rhs_type = rhs_qual_type.type(),
              .range    = node->range()});
          return type::QualType::Error();
        }
      } else {
        // TODO support calling with constants.
        return VerifyBinaryOverload(this, "+=", node, lhs_qual_type.type(),
                                    rhs_qual_type.type());
      }
    } break;
    default: UNREACHABLE();
  }
  UNREACHABLE(stringify(node->op()));
}

type::QualType Compiler::VerifyType(ast::BlockLiteral const *node) {
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { VerifyType(b); }
  for (auto *a : node->after()) { VerifyType(a); }

  return data().set_qual_type(node, type::QualType::Constant(type::Block));
}

type::QualType Compiler::VerifyType(ast::BlockNode const *node) {
  for (auto &param : node->params()) { VerifyType(param.value.get()); }
  for (auto *stmt : node->stmts()) { VerifyType(stmt); }
  return data().set_qual_type(node, type::QualType::Constant(type::Block));
}

type::QualType Compiler::VerifyType(ast::BuiltinFn const *node) {
  return data().set_qual_type(node,
                              type::QualType::Constant(node->value().type()));
}

template <typename EPtr, typename StrType>
static type::QualType VerifyCall(
    Compiler *c, ast::BuiltinFn const *b,
    core::FnArgs<EPtr, StrType> const &args,
    core::FnArgs<type::Typed<ir::Value>> const &arg_vals) {
  // TODO for builtin's consider moving all the messages into an enum.
  switch (b->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      bool err = false;
      if (not arg_vals.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `foreign` cannot be called with "
                       "named arguments.",
        });
        err = true;
      }

      size_t size = arg_vals.size();
      if (size != 2u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = absl::StrCat("Built-in function `foreign` takes exactly "
                                    "two arguments (You provided ",
                                    size, ")."),
        });
        err = true;
      }

      if (not err) {
        if (arg_vals[0].type() != type::ByteView) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->range(),
              .message = absl::StrCat("First argument to `foreign` must be a "
                                      "byte-view (You provided a(n) ",
                                      arg_vals[0].type()->to_string(), ")."),
          });
        }
        if (arg_vals[0]->empty()) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->range(),
              .message = "First argument to `foreign` must be a constant."});
        }
        if (arg_vals[1].type() != type::Type_) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range = b->range(),
              .message =
                  absl::StrCat("Second argument to `foreign` must be a type "
                               "(You provided a(n) ",
                               arg_vals[0].type()->to_string(), ").")});
        }
        if (arg_vals[1]->empty()) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->range(),
              .message = "Second argument to `foreign` must be a constant."});
        }
      }

      auto maybe_type = c->EvaluateAs<type::Type const *>(args[1]);
      if (not maybe_type) { NOT_YET(); }
      auto const *foreign_type = *maybe_type;
      if (not foreign_type->template is<type::Function>() and
          not foreign_type->template is<type::Pointer>()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Builtin `foreign` may only be called when the second "
                       "argument is a pointer or a function type.",
        });
      }
      return type::QualType::Constant(foreign_type);
    } break;
    case ir::BuiltinFn::Which::Opaque:
      if (not arg_vals.empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `opaque` takes no arguments."});
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Opaque().type()->output()[0]);

    case ir::BuiltinFn::Which::Bytes: {
      size_t size = arg_vals.size();
      if (not arg_vals.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `bytes` cannot be called with named "
                       "arguments."});
      } else if (size != 1u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = absl::StrCat(
                "Built-in function `bytes` takes exactly one argument "
                "(You provided ",
                size, ")."),
        });
      } else if (arg_vals[0].type() != type::Type_) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range = b->range(),
            .message =
                absl::StrCat("Built-in function `bytes` must take a single "
                             "argument of type `type` (You provided a(n) ",
                             arg_vals[0].type()->to_string(), ").")});
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Bytes().type()->output()[0]);
    }
    case ir::BuiltinFn::Which::Alignment: {
      size_t size = arg_vals.size();
      if (not arg_vals.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `alignment` cannot be called with "
                       "named arguments."});
      }
      if (size != 1u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = absl::StrCat("Built-in function `alignment` takes "
                                    "exactly one argument (You provided ",
                                    size, ")."),
        });

      } else if (arg_vals[0].type() != type::Type_) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range = b->range(),
            .message =
                absl::StrCat("Built-in function `alignment` must take a single "
                             "argument of type `type` (you provided a(n) ",
                             arg_vals[0].type()->to_string(), ")"),
        });
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Alignment().type()->output()[0]);
    }
    case ir::BuiltinFn::Which::DebugIr:
      // This is for debugging the compiler only, so there's no need to write
      // decent errors here.
      ASSERT(arg_vals.size() == 0u);
      return type::QualType::Constant(type::Void());
  }
  UNREACHABLE();
}

static type::Typed<ir::Value> EvaluateIfConstant(Compiler *c,
                                                 ast::Expression const *expr,
                                                 type::QualType qt) {
  if (qt.constant()) {
    DEBUG_LOG("EvaluateIfConstant")
    ("Evaluating constant: ", expr->DebugString());
    auto maybe_val = c->Evaluate(type::Typed(expr, qt.type()));
    if (not maybe_val) { NOT_YET(); }
    return type::Typed<ir::Value>(*maybe_val, qt.type());
  } else {
    return type::Typed<ir::Value>(ir::Value(), qt.type());
  }
}

static std::optional<core::FnArgs<type::Typed<ir::Value>, std::string_view>>
VerifyFnArgs(
    Compiler *c,
    core::FnArgs<ast::Expression const *, std::string_view> const &args) {
  bool err      = false;
  auto arg_vals = args.Transform([&](ast::Expression const *expr) {
    auto expr_qual_type = c->VerifyType(expr);
    err |= not expr_qual_type.ok();
    if (err) {
      DEBUG_LOG("VerifyFnArgs")("Error with: ", expr->DebugString());
      return type::Typed<ir::Value>(ir::Value(), nullptr);
    }
    DEBUG_LOG("VerifyFnArgs")("constant: ", expr->DebugString());
    return EvaluateIfConstant(c, expr, expr_qual_type);
  });

  if (err) { return std::nullopt; }
  return arg_vals;
}

type::QualType Compiler::VerifyType(ast::Call const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto arg_vals, VerifyFnArgs(this, node->args()));
  // TODO handle cyclic dependencies in call arguments.

  // Note: Currently `foreign` being generic means that we can't easily make
  // builtins overloadable, not that it ever makes sense to do so (because
  // they're globally available).
  //
  // TODO Once type::OverloadSet becomes more robust, we can make generics more
  // robust, then have `foreign` use generics and make this part of the overload
  // set code too.
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    // TODO: Should we allow these to be overloaded?
    ASSIGN_OR(return type::QualType::Error(), auto result,
                     VerifyCall(this, b, node->args(), arg_vals));
    return data().set_qual_type(node, result);
  }

  ASSIGN_OR(return type::QualType::Error(),  //
                   auto callee_qt, VerifyType(node->callee()));
  if (auto *c = callee_qt.type()->if_as<type::Callable>()) {
    DEBUG_LOG("Call.VerifyType")
    ("Callee's (", node->callee()->DebugString(), ") qual-type: ", callee_qt);
    auto ret_types = c->return_types(arg_vals);
    DEBUG_LOG("Call.VerifyType")
    ("Return types for this instantiation: ",
     type::Tup(ret_types)->to_string());
    // TODO under what circumstances can we prove that the implementation
    // doesn't need to be run at runtime?
    return data().set_qual_type(
        node, type::QualType(ret_types, type::Quals::Unqualified()));
  } else {
    diag().Consume(diagnostic::UncallableExpression{
        .range = node->callee()->range(),
    });
    return data().set_qual_type(node, type::QualType::Error());
  }
}

type::QualType Compiler::VerifyType(ast::Cast const *node) {
  auto expr_qual_type = VerifyType(node->expr());
  auto type_result    = VerifyType(node->type());
  if (not expr_qual_type.ok() or not type_result.ok()) {
    return type::QualType::Error();
  }

  if (type_result.type() != type::Type_) {
    diag().Consume(diagnostic::CastToNonType{
        .range = node->range(),
    });
    return type::QualType::Error();
  }
  if (not type_result.constant()) {
    diag().Consume(diagnostic::CastToNonConstantType{
        .range = node->range(),
    });
    return type::QualType::Error();
  }

  auto maybe_type = EvaluateAs<type::Type const *>(node->type());
  if (not maybe_type) { NOT_YET(); }
  auto const *t = *maybe_type;
  if (t->is<type::Struct>()) {
    // TODO do you ever want to support overlaods that accepts constants?
    return VerifyUnaryOverload(this, "as", node, expr_qual_type.type());
  } else {
    if (not type::CanCast(expr_qual_type.type(), t)) {
      diag().Consume(diagnostic::InvalidCast{
          .from  = expr_qual_type.type(),
          .to    = t,
          .range = node->range(),
      });
      NOT_YET("log an error", expr_qual_type.type(), t);
    }

    return data().set_qual_type(node,
                                type::QualType(t, expr_qual_type.quals()));
  }
}

type::QualType Compiler::VerifyType(ast::ChainOp const *node) {
  std::vector<type::QualType> results;
  results.reserve(node->exprs().size());
  for (auto *expr : node->exprs()) { results.push_back(VerifyType(expr)); }
  if (absl::c_any_of(results,
                     [](type::QualType const &v) { return not v.ok(); })) {
    return type::QualType::Error();
  }

  if (node->ops()[0] == frontend::Operator::Or) {
    bool found_err = false;
    for (size_t i = 0; i < results.size() - 1; ++i) {
      if (results[i].type() == type::Block) {
        if (not results[i].constant()) {
          NOT_YET("log an error: non const block");
        }
      } else {
        goto not_blocks;
      }
    }
    if (found_err) { return type::QualType::Error(); }
    auto &last = results.back();
    if (last.type() != type::Block) {
      goto not_blocks;
    } else if (not results.back().constant()) {
      NOT_YET("log an error: non const block");
    } else {
      return data().set_qual_type(node, type::QualType::Constant(last.type()));
    }
  }
not_blocks:

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all
  // have the same precedence, and ^, &, and | uniquely hold a given
  // precedence.
  switch (node->ops()[0]) {
    case frontend::Operator::Or:
    case frontend::Operator::And:
    case frontend::Operator::Xor: {
      bool failed                       = false;
      type::Quals quals                 = type::Quals::Const();
      type::Type const *first_expr_type = results[0].type();

      for (auto &result : results) {
        // TODO node collection of error messages could be greatly improved.
        if (result.type() != first_expr_type) {
          auto op_str = [node] {
            switch (node->ops()[0]) {
              case frontend::Operator::Or: return "|";
              case frontend::Operator::And: return "&";
              case frontend::Operator::Xor: return "^";
              default: UNREACHABLE();
            }
          }();

          NOT_YET("Log an error");
          quals &= result.quals();
          failed = true;
        }
      }

      if (failed) { return type::QualType::Error(); }
      return data().set_qual_type(node, type::QualType(first_expr_type, quals));
    } break;
    default: {
      auto quals = type::Quals::Const() & results[0].quals();
      ASSERT(node->exprs().size() >= 2u);
      for (size_t i = 0; i + 1 < node->exprs().size(); ++i) {
        type::QualType const &lhs_qual_type = results[i];
        type::QualType const &rhs_qual_type = results[i + 1];
        quals &= rhs_qual_type.quals();

        // TODO struct is wrong. generally user-defined (could be array of
        // struct too, or perhaps a variant containing a struct?) need to
        // figure out the details here.
        const char *token = nullptr;
        switch (node->ops()[i]) {
          case frontend::Operator::Lt: token = "<"; break;
          case frontend::Operator::Le: token = "<="; break;
          case frontend::Operator::Eq: token = "=="; break;
          case frontend::Operator::Ne: token = "!="; break;
          case frontend::Operator::Ge: token = ">="; break;
          case frontend::Operator::Gt: token = ">"; break;
          default: UNREACHABLE();
        }

        if (lhs_qual_type.type()->is<type::Struct>() or
            lhs_qual_type.type()->is<type::Struct>()) {
          // TODO overwriting type a bunch of times?
          // TODO support calling with constants.
          return VerifyBinaryOverload(this, token, node, lhs_qual_type.type(),
                                      rhs_qual_type.type());
        }

        if (lhs_qual_type.type() != rhs_qual_type.type() and
            not(lhs_qual_type.type()->is<type::Pointer>() and
                rhs_qual_type.type() == type::NullPtr) and
            not(rhs_qual_type.type()->is<type::Pointer>() and
                lhs_qual_type.type() == type::NullPtr)) {
          NOT_YET("Log an error", lhs_qual_type.type()->to_string(),
                  rhs_qual_type.type()->to_string(), node);

        } else {
          auto cmp = Comparator(lhs_qual_type.type());

          switch (node->ops()[i]) {
            case frontend::Operator::Eq:
            case frontend::Operator::Ne: {
              switch (cmp) {
                case Cmp::Order:
                case Cmp::Equality: continue;
                case Cmp::None:
                  diag().Consume(diagnostic::ComparingIncomparables{
                      .lhs   = lhs_qual_type.type(),
                      .rhs   = rhs_qual_type.type(),
                      .range = frontend::SourceRange(
                          node->exprs()[i]->range().begin(),
                          node->exprs()[i + 1]->range().end()),
                  });
                  return type::QualType::Error();
              }
            } break;
            case frontend::Operator::Lt:
            case frontend::Operator::Le:
            case frontend::Operator::Ge:
            case frontend::Operator::Gt: {
              switch (cmp) {
                case Cmp::Order: continue;
                case Cmp::Equality:
                case Cmp::None:
                  diag().Consume(diagnostic::ComparingIncomparables{
                      .lhs   = lhs_qual_type.type(),
                      .rhs   = rhs_qual_type.type(),
                      .range = frontend::SourceRange(
                          node->exprs()[i]->range().begin(),
                          node->exprs()[i + 1]->range().end()),
                  });
                  return type::QualType::Error();
              }
            } break;
            default: UNREACHABLE("Expecting a ChainOp operator type.");
          }
        }
      }

      return data().set_qual_type(node, type::QualType(type::Bool, quals));
    }
  }
}

type::QualType Compiler::VerifyType(ast::EnumLiteral const *node) {
  for (auto const &elem : node->elems()) {
    if (auto *decl = elem->if_as<ast::Declaration>()) {
      auto *t = VerifyType(decl->init_val()).type();
      ASSERT(type::IsIntegral(t) == true);
      // TODO determine what is allowed here and how to generate errors.
    }
  }

  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const *node) {
  absl::flat_hash_set<core::DependencyNode<ast::Declaration>> deps;
  for (auto const &p : node->params()) {
    deps.insert(
        core::DependencyNode<ast::Declaration>::MakeArgType(p.value.get()));
    deps.insert(
        core::DependencyNode<ast::Declaration>::MakeType(p.value.get()));
    if (p.value->flags() & ast::Declaration::f_IsConst) {
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeValue(p.value.get()));
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeArgType(p.value.get()));
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeArgValue(p.value.get()));
    }
  }

  std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
      ordered_nodes;
  node->parameter_dependency_graph().topologically([&](auto dep_node) {
    if (not deps.contains(dep_node)) { return; }
    DEBUG_LOG("generic-fn")
    ("adding ", ToString(dep_node.kind()), "`", dep_node.node()->id(), "`");
    ordered_nodes.emplace_back(0, dep_node);
  });

  // Compute and set the index or `ordered_nodes` so that each node knows the
  // ordering in source code. This allows us to match parameters to arguments
  // efficiently.
  absl::flat_hash_map<ast::Declaration const *, int> param_index;
  int index = 0;
  for (auto const &param : node->params()) {
    param_index.emplace(param.value.get(), index++);
  }

  for (auto &[index, node] : ordered_nodes) {
    index = param_index.find(node.node())->second;
  }

  return ordered_nodes;
}

// TODO: There's something strange about this: We want to work on a temporary
// data/compiler, but using `this` makes it feel more permanent.
std::pair<core::Params<type::QualType>, ConstantBinding>
Compiler::ComputeParamsFromArgs(
    ast::ParameterizedExpression const *node,
    absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
        ordered_nodes,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  ConstantBinding constants;
  DEBUG_LOG("generic-fn")
  ("Creating a concrete implementation with ",
   args.Transform([](auto const &a) { return a.type()->to_string(); }));

  core::Params<type::QualType> param_types(node->params().size());

  // TODO use the proper ordering.
  for (auto [index, dep_node] : ordered_nodes) {
    DEBUG_LOG("generic-fn")
    ("Handling dep-node ", ToString(dep_node.kind()), "`",
     dep_node.node()->id(), "`");
    switch (dep_node.kind()) {
      case core::DependencyNodeKind::ArgValue: {
        ir::Value val;
        if (index < args.pos().size()) {
          val = *args[index];
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          val = **a;
        } else {
          auto const *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          type::Type const *t =
              ASSERT_NOT_NULL(data().arg_type(dep_node.node()->id()));
          auto maybe_val = Evaluate(type::Typed(init_val, t));
          if (not maybe_val) { NOT_YET(); }
          val = *maybe_val;
        }

        // Erase values not known at compile-time.
        if (val.get_if<ir::Reg>()) { val = ir::Value(); }

        auto tostr = [](ir::Value v) {
          if (auto **t = v.get_if<type::Type const *>()) {
            return (*t)->to_string();
          } else {
            std::stringstream ss;
            ss << v;
            return ss.str();
          }
        };
        DEBUG_LOG("generic-fn")("... ", tostr(val));
        data().set_arg_value(dep_node.node()->id(), val);
      } break;
      case core::DependencyNodeKind::ArgType: {
        type::Type const *arg_type = nullptr;
        if (index < args.pos().size()) {
          arg_type = args[index].type();
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          arg_type = a->type();
        } else {
          auto *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          arg_type       = VerifyType(init_val).type();
        }
        DEBUG_LOG("generic-fn")("... ", *arg_type);
        data().set_arg_type(dep_node.node()->id(), arg_type);
      } break;
      case core::DependencyNodeKind::ParamType: {
        type::Type const *t = nullptr;
        if (auto const *type_expr = dep_node.node()->type_expr()) {
          auto type_expr_type = VerifyType(type_expr).type();
          if (type_expr_type != type::Type_) {
            NOT_YET("log an error: ", type_expr->DebugString(), ": ", type_expr_type);
          }

          auto maybe_type = EvaluateAs<type::Type const *>(type_expr);
          if (not maybe_type) { NOT_YET(); }
          t = ASSERT_NOT_NULL(*maybe_type);
        } else {
          t = VerifyType(dep_node.node()->init_val()).type();
        }

        auto qt = (dep_node.node()->flags() & ast::Declaration::f_IsConst)
                      ? type::QualType::Constant(t)
                      : type::QualType::NonConstant(t);

        // TODO: Once a parameter type has been computed, we know it's
        // argument type has already been computed so we can verify that the
        // implicit casts are allowed.
        DEBUG_LOG("generic-fn")("... ", qt);
        size_t i =
            *ASSERT_NOT_NULL(node->params().at_or_null(dep_node.node()->id()));
        param_types.set(
            i, core::Param<type::QualType>(dep_node.node()->id(), qt,
                                               node->params()[i].flags));
      } break;
      case core::DependencyNodeKind::ParamValue: {
        // Find the argument associated with this parameter.
        // TODO, if the type is wrong but there is an implicit cast, deal with
        // that.
        type::Typed<ir::Value> arg;
        if (index < args.pos().size()) {
          arg = args[index];
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          arg = *a;
        } else {
          auto const *t  = ASSERT_NOT_NULL(type_of(dep_node.node()));
          auto maybe_val = Evaluate(
              type::Typed(ASSERT_NOT_NULL(dep_node.node()->init_val()), t));
          if (not maybe_val) { NOT_YET(); }
          arg = type::Typed<ir::Value>(*maybe_val, t);
          DEBUG_LOG("generic-fn")(dep_node.node()->DebugString());
        }

        data().constants_.reserve_slot(dep_node.node(), arg.type());
        if (data().constants_.get_constant(dep_node.node()).empty()) {
          data().constants_.set_slot(dep_node.node(), *arg);
        }
        constants.reserve_slot(dep_node.node(), arg.type());
        constants.set_slot(dep_node.node(), *arg);

      } break;
    }
  }
  return std::pair<core::Params<type::QualType>, ConstantBinding>(
      std::move(param_types), std::move(constants));
}

std::tuple<core::Params<type::QualType>, std::vector<type::Type const *> *,
           DependentComputedData *>
MakeConcrete(
    ast::ParameterizedExpression const *node, CompiledModule *mod,
    absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
        ordered_nodes,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    DependentComputedData &compiler_data,
    diagnostic::DiagnosticConsumer &diag) {
  DependentComputedData temp_data(mod);
  Compiler c({
      .builder             = ir::GetBuilder(),
      .data                = temp_data,
      .diagnostic_consumer = diag,
  });
  temp_data.parent_ = &compiler_data;

  auto [parameters, constants] =
      c.ComputeParamsFromArgs(node, ordered_nodes, args);

  auto [params, rets, data, inserted] =
      compiler_data.InsertDependent(node, parameters, std::move(constants));
  if (inserted) {
    if (auto const *fn_node = node->if_as<ast::FunctionLiteral>()) {
      if (auto outputs = fn_node->outputs(); outputs and not outputs->empty()) {
        for (auto const *o : *outputs) {
          auto qt = c.VerifyType(o);
          ASSERT(qt == type::QualType::Constant(type::Type_));
          auto maybe_type = c.EvaluateAs<type::Type const *>(o);
          if (not maybe_type) { NOT_YET(); }
          rets.push_back(ASSERT_NOT_NULL(*maybe_type));
        }
      }
    }
  }

  return std::tuple(params, &rets, &data);
}

type::QualType Compiler::VerifyType(ast::FunctionLiteral const *node) {
  if (not node->is_generic()) { return VerifyConcreteFnLit(node); }

  auto ordered_nodes = OrderedDependencyNodes(node);

  auto *diag_consumer = &diag();
  auto gen            = [node, compiler_data = &data(), diag_consumer,
              ordered_nodes(std::move(ordered_nodes))](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    auto [params, rets, data] =
        MakeConcrete(node, compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);
    type::Function const *ft = type::Func(params, *rets);
    data->set_qual_type(node, type::QualType::Constant(ft));
    return ft;
  };

  return data().set_qual_type(
      node,
      type::QualType::Constant(new type::GenericFunction(std::move(gen))));
}

type::QualType Compiler::VerifyType(ast::FunctionType const *node) {
  type::Type const *t = type::Type_;
  type::Quals quals   = type::Quals::Const();

  for (auto const *p : node->params()) {
    auto qt = VerifyType(p);
    quals &= qt.quals();
    if (qt.type() != type::Type_) {
      t = nullptr;
      diag().Consume(diagnostic::NonTypeFunctionInput{
          .range = p->range(),
      });
    }
  }

  for (auto const *p : node->outputs()) {
    auto qt = VerifyType(p);
    quals &= qt.quals();
    if (qt.type() != type::Type_) {
      t = nullptr;
      diag().Consume(diagnostic::NonTypeFunctionInput{
          .range = p->range(),
      });
    }
  }

  if (t == nullptr) { return type::QualType::Error(); }
  return data().set_qual_type(node, type::QualType(type::Type_, quals));
}

type::QualType Compiler::VerifyType(ast::ShortFunctionLiteral const *node) {
  if (not node->is_generic()) {
    ASSIGN_OR(return type::QualType::Error(),  //
                     auto params, VerifyParams(this, node->params()));
    ASSIGN_OR(return _, auto body_qt, VerifyType(node->body()));
    return data().set_qual_type(
        node, type::QualType::Constant(
                  type::Func(std::move(params), {body_qt.type()})));
  }

  auto ordered_nodes = OrderedDependencyNodes(node);

  auto *diag_consumer = &diag();
  auto gen            = [node, compiler_data = &data(), diag_consumer,
              ordered_nodes(std::move(ordered_nodes))](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    // TODO handle compilation failures.
    auto [params, rets, data] =
        MakeConcrete(node, compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);

    Compiler c({
        .builder             = ir::GetBuilder(),
        .data                = *data,
        .diagnostic_consumer = *diag_consumer,
    });
    auto body_qt = c.VerifyType(node->body());
    *rets        = {body_qt.type()};
    return type::Func(std::move(params), *rets);
  };

  return data().set_qual_type(
      node,
      type::QualType::Constant(new type::GenericFunction(std::move(gen))));
}

type::QualType Compiler::VerifyType(ast::Import const *node) {
  DEBUG_LOG("Import")(node->DebugString());
  ASSIGN_OR(return _, auto result, VerifyType(node->operand()));
  bool err = false;
  if (result.type() != type::ByteView) {
    // TODO allow (import) overload
    diag().Consume(diagnostic::InvalidImport{
        .range = node->operand()->range(),
    });
    err = true;
  }

  if (not result.constant()) {
    diag().Consume(diagnostic::NonConstantImport{
        .range = node->operand()->range(),
    });
    err = true;
  }

  if (err) { return type::QualType::Error(); }

  auto maybe_src = EvaluateAs<ir::String>(node->operand());
  if (not maybe_src) { NOT_YET(); }

  auto canonical_file_name =
      frontend::CanonicalFileName::Make(frontend::FileName(maybe_src->get()));
  if (auto *mod = ImportLibraryModule(canonical_file_name)) {
    data().set_imported_module(node, mod);
    return data().set_qual_type(node, type::QualType::Constant(type::Module));
  } else {
    return type::QualType::Error();
  }
}

type::QualType Compiler::VerifyType(ast::Goto const *node) {
  for (auto const &option : node->options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }
  return type::QualType::Constant(type::Void());
}

type::QualType Compiler::VerifyType(ast::Label const *node) {
  return data().set_qual_type(node, type::QualType::Constant(type::Label));
}

type::QualType Compiler::VerifyType(ast::Jump const *node) {
  DEBUG_LOG("Jump")(node->DebugString());

  bool err                = false;
  type::Type const *state = nullptr;

  if (node->state()) {
    auto state_qual_type = VerifyType(node->state());
    err                  = not state_qual_type.ok();
    if (not err) { state = state_qual_type.type(); }
  }

  core::Params<type::Type const *> param_types =
      node->params().Transform([&](auto const &param) {
        auto v = VerifyType(param.get());
        err |= not v.ok();
        return v.type();
      });

  for (auto const *stmt : node->stmts()) { VerifyType(stmt); }

  return data().set_qual_type(
      node, err ? type::QualType::Error()
                : type::QualType::Constant(type::Jmp(state, param_types)));
}

type::QualType Compiler::VerifyType(ast::ReturnStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::YieldStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::ScopeLiteral const *node) {
  auto verify_result =
      data().set_qual_type(node, type::QualType::Constant(type::Scope));
  bool error = false;
  if (node->state_type()) {
    type::QualType state_qual_type = VerifyType(node->state_type());
    if (state_qual_type != type::QualType(type::Type_, type::Quals::Const())) {
      // TODO check for non-const vs. not a type.
      diag().Consume(diagnostic::NonTypeScopeState{
          .type  = state_qual_type.type(),
          .range = node->state_type()->range(),
      });
      error = true;
    }
  }

  absl::flat_hash_map<ast::Declaration const *, type::Type const *> types;
  for (auto const *decl : node->decls()) {
    auto qual_type = VerifyType(decl);
    if (not qual_type.constant()) {
      error = true;
      NOT_YET("log an error");
    }
    types.emplace(decl, qual_type.type());
  }
  // TODO verify that it has at least one entry and exit point each.
  if (error) { return type::QualType::Error(); }

  if (not node->state_type()) { return verify_result; }
  auto maybe_type = EvaluateAs<type::Type const *>(node->state_type());
  if (not maybe_type) { NOT_YET(); }
  auto const *state_type_ptr = type::Ptr(ASSERT_NOT_NULL(*maybe_type));
  for (auto const [decl, decl_type] : types) {
    if (decl->id() == "init") {
      auto *jump_type = decl_type->if_as<type::Jump>();
      if (not jump_type) { NOT_YET(); }
      if (state_type_ptr != jump_type->state()) {
        NOT_YET(state_type_ptr ? state_type_ptr->to_string() : "NULL", " vs ",
                (jump_type and jump_type->state())
                    ? jump_type->state()->to_string()
                    : "NULL");
      }
    } else if (decl->id() == "done") {
      auto *fn_type = decl_type->if_as<type::Function>();
      if (not fn_type) { NOT_YET(); }
    } else {
      // TODO
    }
  }
  return verify_result;
}

type::QualType Compiler::VerifyType(ast::ScopeNode const *node) {
  DEBUG_LOG("ScopeNode")(node->DebugString());
  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyFnArgs(this, node->args()));
  // TODO handle cyclic dependencies in call arguments.

  for (auto const &block : node->blocks()) { VerifyType(&block); }

  // TODO hack. Set this for real.
  return data().set_qual_type(node, type::QualType::NonConstant(type::Void()));
}

type::QualType Compiler::VerifyType(ast::StructLiteral const *node) {
  bool err = false;
  for (auto const &field : node->fields()) {
    type::QualType type_expr_qt;
    if (field.type_expr()) { type_expr_qt = VerifyType(field.type_expr()); }

    type::QualType init_val_qt;
    if (field.init_val()) { init_val_qt = VerifyType(field.init_val()); }

    if ((field.type_expr() and not type_expr_qt) or
        (field.init_val() and not init_val_qt)) {
      err = true;
      continue;
    }

    if (field.type_expr() and not type_expr_qt.constant()) {
      err = true;
      NOT_YET("Log an error, type must be constant");
    }

    if (field.init_val() and not init_val_qt.constant()) {
      err = true;
      NOT_YET("Log an error, initial value must be constant");
    }

    if (field.init_val()) {
      auto maybe_type = EvaluateAs<type::Type const *>(field.type_expr());
      if (not maybe_type) { NOT_YET(); }
      auto const *t = ASSERT_NOT_NULL(*maybe_type);
      if (t != init_val_qt.type()) {
        err = true;
        NOT_YET("log an error, type mismatch", init_val_qt, t);
      }
    }

    // TODO set field results?
  }

  if (err) { return data().set_qual_type(node, type::QualType::Error()); }
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  std::vector<type::Type const *> ts;
  ts.reserve(node->params().size());
  for (auto const &a : node->params()) { ts.push_back(VerifyType(&a).type()); }
  if (absl::c_any_of(ts, [](type::Type const *t) { return t == nullptr; })) {
    return type::QualType::Error();
  }

  return data().set_qual_type(node, type::QualType::Constant(type::GenStruct(
                                        node->scope(), std::move(ts))));
}

type::QualType Compiler::VerifyType(ast::StructType const *node) {
  for (auto &arg : node->args_) { VerifyType(arg.get()); }
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::VerifyType(ast::Switch const *node) {
  // Don't allow switch to return references.
  type::Quals quals           = type::Quals::Const();
  type::Type const *expr_type = nullptr;
  if (node->expr()) {
    ASSIGN_OR(return _, auto result, VerifyType(node->expr()));
    quals &= result.quals();
    expr_type = result.type();
  }

  absl::flat_hash_set<type::Type const *> types;
  bool err = false;
  for (auto &[body, cond] : node->cases()) {
    auto cond_result = VerifyType(cond.get());
    auto body_result = VerifyType(body.get());
    err |= not cond_result or not body_result;
    if (err) {
      NOT_YET();
      continue;
    }

    quals &= cond_result.quals() & body_result.quals();
    if (node->expr()) {
      static_cast<void>(expr_type);
      // TODO dispatch table
    } else {
      if (cond_result.type() != type::Bool) {
        diag().Consume(diagnostic::SwitchConditionNeedsBool{
            .type  = cond_result.type(),
            .range = node->range(),
        });
      }
    }
    // TODO if there's an error, an unordereded_set is not helpful for giving
    // good error messages.
    if (body->is<ast::Expression>()) {
      // TODO check that it's actually a jump
      types.insert(body_result.type());
    }
  }
  if (err) { return type::QualType::Error(); }

  // TODO check to ensure that the type is either exhaustable or has a
  // default.

  if (types.empty()) {
    return data().set_qual_type(node, type::QualType(type::Void(), quals));
  }
  auto some_type = *types.begin();
  if (absl::c_all_of(types,
                     [&](type::Type const *t) { return t == some_type; })) {
    // TODO node might be a constant.
    return data().set_qual_type(node, type::QualType(some_type, quals));
  } else {
    NOT_YET("handle type error");
    return type::QualType::Error();
  }
}

type::QualType Compiler::VerifyType(ast::Terminal const *node) {
  return data().set_qual_type(
      node, type::QualType::Constant(type::Prim(node->basic_type())));
}

type::QualType Compiler::VerifyType(ast::Unop const *node) {
  ASSIGN_OR(return type::QualType::Error(), auto result,
                   VerifyType(node->operand()));
  auto *operand_type = result.type();

  switch (node->op()) {
    case frontend::Operator::Copy:
      if (not operand_type->IsCopyable()) {
        NOT_YET("log an error. not copyable");
      }
      // TODO Are copies always consts?
      return data().set_qual_type(node,
                                  type::QualType(operand_type, result.quals()));
    case frontend::Operator::Move:
      if (not operand_type->IsMovable()) {
        NOT_YET("log an error. not movable");
      }
      // TODO Are copies always consts?
      return data().set_qual_type(node,
                                  type::QualType(operand_type, result.quals()));
    case frontend::Operator::BufPtr:
      return data().set_qual_type(node,
                                  type::QualType(operand_type, result.quals()));
    case frontend::Operator::TypeOf:
      return data().set_qual_type(node,
                                  type::QualType(operand_type, result.quals()));
    case frontend::Operator::Eval:
      if (not result.constant()) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do node probably worth a
        // full pass over all verification code.
        diag().Consume(diagnostic::NonConstantEvaluation{
            .range = node->operand()->range(),
        });
        return type::QualType::Error();
      } else {
        return data().set_qual_type(
            node, type::QualType(operand_type, result.quals()));
      }
    case frontend::Operator::Which:
      if (not operand_type->is<type::Variant>()) {
        diag().Consume(diagnostic::WhichNonVariant{
            .type  = operand_type,
            .range = node->range(),
        });
      }
      return data().set_qual_type(node,
                                  type::QualType(type::Type_, result.quals()));
    case frontend::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        return data().set_qual_type(
            node, type::QualType(operand_type->as<type::Pointer>().pointee(),
                                 result.quals()));
      } else {
        diag().Consume(diagnostic::DereferencingNonPointer{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    case frontend::Operator::And:
      if ((result.quals() & type::Quals::Ref()) == type::Quals::Ref()) {
        result = type::QualType(type::Ptr(operand_type), result.quals());
      } else {
        diag().Consume(diagnostic::NonAddressableExpression{
            .range = node->range(),
        });
        result = type::QualType::Error();
      }
      return data().set_qual_type(node, result);
    case frontend::Operator::Mul:
      if (operand_type == type::Type_) {
        return data().set_qual_type(
            node, type::QualType(type::Type_, type::Quals::Const()));
      } else {
        NOT_YET("log an error, ", operand_type->to_string(),
                node->DebugString());
        return type::QualType::Error();
      }
    case frontend::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        return data().set_qual_type(
            node, type::QualType(operand_type,
                                 result.quals() & type::Quals::Const()));
      } else if (operand_type->is<type::Struct>()) {
        // TODO do you ever want to support overlaods that accepts constants?
        return VerifyUnaryOverload(this, "-", node, result.type());
      }
      NOT_YET();
      return type::QualType::Error();
    case frontend::Operator::Not:
      if (operand_type == type::Bool or operand_type->is<type::Enum>() or
          operand_type->is<type::Flags>()) {
        return data().set_qual_type(
            node, type::QualType(operand_type,
                                 result.quals() & type::Quals::Const()));
      }
      if (operand_type->is<type::Struct>()) {
        // TODO do you ever want to support overlaods that accepts constants?
        return VerifyUnaryOverload(this, "-", node, result.type());
      } else {
        NOT_YET("log an error");
        return type::QualType::Error();
      }
    case frontend::Operator::Needs:
      if (operand_type != type::Bool) {
        diag().Consume(diagnostic::PreconditionNeedsBool{
            .type  = operand_type,
            .range = node->operand()->range(),
        });
      }
      if (not result.constant()) { NOT_YET(); }
      return data().set_qual_type(node, type::QualType::Constant(type::Void()));
    case frontend::Operator::Ensure:
      if (operand_type != type::Bool) {
        diag().Consume(diagnostic::PostconditionNeedsBool{
            .type  = operand_type,
            .range = node->operand()->range(),
        });
      }
      if (not result.constant()) { NOT_YET(); }
      return data().set_qual_type(node, type::QualType::Constant(type::Void()));
    case frontend::Operator::VariadicPack: {
      if (not result.constant()) { NOT_YET("Log an error"); }
      // TODO could be a type, or a function returning a ty
      if (result.type() == type::Type_) {
        return data().set_qual_type(node,
                                    type::QualType::Constant(type::Type_));
      } else if (result.type()->is<type::Function>()) {
        NOT_YET();
      }
      NOT_YET(*node);
    }
    default: UNREACHABLE(*node);
  }
}

}  // namespace compiler
