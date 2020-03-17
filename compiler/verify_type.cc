#include "absl/algorithm/container.h"

#include <iostream>
#include <optional>
#include <string_view>

#include "ast/ast.h"
#include "ast/overload_set.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/scope_table.h"
#include "compiler/extract_jumps.h"
#include "compiler/library_module.h"
#include "compiler/verify_assignment_and_initialization.h"
#include "diagnostic/consumer/trivial.h"
#include "diagnostic/errors.h"
#include "frontend/lex/operators.h"
#include "interpretter/evaluate.h"
#include "ir/compiled_fn.h"
#include "type/cast.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/parameter_pack.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace compiler {
namespace {

void AddAdl(ast::OverloadSet *overload_set, std::string_view id,
            type::Type const *t) {
  absl::flat_hash_set<CompiledModule *> modules;
  // TODO t->ExtractDefiningModules(&modules);

  for (auto *mod : modules) {
    auto decls = mod->declarations(id);
    diagnostic::TrivialConsumer consumer;

    for (auto *d : decls) {
      // TODO Wow this is a terrible way to access the type.
      ASSIGN_OR(continue, auto &t, Compiler(mod, consumer).type_of(d));
      // TODO handle this case. I think it's safe to just discard it.
      for (auto const *expr : overload_set->members()) {
        if (d == expr) { return; }
      }

      // TODO const
      overload_set->insert(d);
    }
  }
}

type::QualType VerifyUnaryOverload(Compiler *c, char const *symbol,
                                   ast::Expression const *node,
                                   type::Typed<ir::Results> operand) {
  ast::OverloadSet os(node->scope(), symbol);
  AddAdl(&os, symbol, operand.type());
  std::vector<type::Typed<ir::Results>> pos_args;
  pos_args.push_back(std::move(operand));
  ASSIGN_OR(
      return type::QualType::Error(), auto table,
             FnCallDispatchTable::Verify(c, os,
                                         core::FnArgs<type::Typed<ir::Results>>(
                                             std::move(pos_args), {})));
  c->data_.set_dispatch_table(node, std::move(table));
  return c->set_result(node, table.result_qual_type());
}

type::QualType VerifyBinaryOverload(Compiler *c, char const *symbol,
                                    ast::Expression const *node,
                                    type::Typed<ir::Results> lhs,
                                    type::Typed<ir::Results> rhs) {
  ast::OverloadSet os(node->scope(), symbol);
  AddAdl(&os, symbol, lhs.type());
  AddAdl(&os, symbol, rhs.type());

  std::vector<type::Typed<ir::Results>> pos_args;
  pos_args.push_back(std::move(lhs));
  pos_args.push_back(std::move(rhs));
  ASSIGN_OR(
      return type::QualType::Error(), auto table,
             FnCallDispatchTable::Verify(c, os,
                                         core::FnArgs<type::Typed<ir::Results>>(
                                             std::move(pos_args), {})));
  c->data_.set_dispatch_table(node, std::move(table));
  return c->set_result(node, table.result_qual_type());
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
        std::min(static_cast<cmp_t>(Comparator(a->data_type)),
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

std::pair<type::Type const *, int> DereferenceAll(type::Type const *t) {
  int num_derefs = 0;
  while (auto *p = t->if_as<type::Pointer>()) {
    t = p->pointee;
    ++num_derefs;
  }
  return std::pair(t, num_derefs);
}

}  // namespace

static std::optional<std::vector<type::QualType>> VerifyWithoutSetting(
    Compiler *visitor, base::PtrSpan<ast::Expression const> exprs) {
  std::vector<type::QualType> results;
  results.reserve(exprs.size());
  for (auto const &expr : exprs) {
    auto r = visitor->Visit(expr, VerifyTypeTag{});
    results.push_back(r);
  }
  if (absl::c_any_of(results,
                     [](type::QualType const &r) { return not r.ok(); })) {
    return std::nullopt;
  }
  return results;
}

static type::QualType VerifySpecialFunctions(Compiler *visitor,
                                             ast::Declaration const *decl,
                                             type::Type const *decl_type) {
  bool error = false;
  if (decl->id() == "copy") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (not f->output().empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->params().size() != 2 or
          f->params().at(0).value != f->params().at(1).value or
          not f->params().at(0).value->is<type::Pointer>() or
          not f->params()
                  .at(0)
                  .value->as<type::Pointer>()
                  .pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect params type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s = f->params()
                            .at(0)
                            .value->as<type::Pointer>()
                            .pointee->as<type::Struct>();

        if (decl->scope() != s.scope_) {
          error = true;
          NOT_YET(
              "(copy) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(
                ast::Hashtag(ast::Hashtag::Builtin::Uncopyable))) {
          NOT_YET("defined (copy) on a non-copyable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (copy) must be a function.");
    }
  } else if (decl->id() == "move") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (not f->output().empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->params().size() != 2 or
          f->params().at(0).value != f->params().at(1).value or
          not f->params().at(0).value->is<type::Pointer>() or
          not f->params()
                  .at(0)
                  .value->as<type::Pointer>()
                  .pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect params type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s = f->params()
                            .at(0)
                            .value->as<type::Pointer>()
                            .pointee->as<type::Struct>();

        if (decl->scope() != s.scope_) {
          error = true;
          NOT_YET(
              "(move) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(
                ast::Hashtag(ast::Hashtag::Builtin::Immovable))) {
          error = true;
          NOT_YET("defined (move) for an immovable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (move) must be a function.");
    }
  }
  if (error) { visitor->set_result(decl, type::QualType::Error()); }

  return visitor->set_result(
      decl,
      type::QualType(decl_type, (decl->flags() & ast::Declaration::f_IsConst)
                                    ? type::Quals::Const()
                                    : type::Quals::Unqualified()));
}

// TODO what about shadowing of symbols across module boundaries imported with
// -- ::= ?
// Or when you import two modules verifying that symbols don't conflict.
bool Shadow(Compiler *compiler, type::Typed<ast::Declaration const *> decl1,
            type::Typed<ast::Declaration const *> decl2) {
  // TODO Don't worry about generic shadowing? It'll be checked later?
  if (decl1.type()->is<type::GenericFunction>() or
      decl2.type()->is<type::GenericFunction>()) {
    return false;
  }

  return core::AmbiguouslyCallable(
      ExtractParamTypes(compiler, *decl1).Transform([](auto const &typed_decl) {
        return typed_decl.type();
      }),
      ExtractParamTypes(compiler, *decl2).Transform([](auto const &typed_decl) {
        return typed_decl.type();
      }),
      [](type::Type const *lhs, type::Type const *rhs) {
        return type::Meet(lhs, rhs) != nullptr;
      });
}

static diagnostic::UninferrableType::Reason Inferrable(type::Type const *t) {
  if (t == type::NullPtr) {
    return diagnostic::UninferrableType::Reason::kNullPtr;
  }
  if (t == type::EmptyArray) {
    return diagnostic::UninferrableType::Reason::kEmptyArray;
  }
  if (auto *a = t->if_as<type::Array>()) { return Inferrable(a->data_type); }
  if (auto *p = t->if_as<type::Pointer>()) { return Inferrable(p->pointee); }
  if (auto *v = t->if_as<type::Variant>()) {
    // TODO only returning the first failure here and not even givving a good
    // explanation of precisely what the problem is. Fix here and below.
    for (auto const *var : v->variants_) {
      auto reason = Inferrable(var);
      if (reason != diagnostic::UninferrableType::Reason::kInferrable) {
        return reason;
      }
    }
  } else if (auto *tup = t->if_as<type::Tuple>()) {
    for (auto const *entry : tup->entries_) {
      auto reason = Inferrable(entry);
      if (reason != diagnostic::UninferrableType::Reason::kInferrable) {
        return reason;
      }
    }
  } else if (auto *f = t->if_as<type::Function>()) {
    for (auto const &param : f->params()) {
      auto reason = Inferrable(param.value);
      if (reason != diagnostic::UninferrableType::Reason::kInferrable) {
        return reason;
      }
    }
    for (auto const *t : f->output()) {
      auto reason = Inferrable(t);
      if (reason != diagnostic::UninferrableType::Reason::kInferrable) {
        return reason;
      }
    }
  }
  // TODO higher order types?
  return diagnostic::UninferrableType::Reason::kInferrable;
}

// TODO there's not that much shared between the inferred and uninferred cases,
// so probably break them out.
type::QualType VerifyBody(Compiler *c, ast::FunctionLiteral const *node) {
  for (auto const *stmt : node->stmts()) { c->Visit(stmt, VerifyTypeTag{}); }
  // TODO propogate cyclic dependencies.

  // TODO we can have yields and returns, or yields and jumps, but not jumps and
  // returns. Check this.
  absl::flat_hash_set<type::Type const *> types;
  absl::flat_hash_map<ast::ReturnStmt const *, type::Type const *>
      saved_ret_types;
  for (auto const *n : c->data_.extraction_map_[node]) {
    if (auto const *ret_node = n->if_as<ast::ReturnStmt>()) {
      std::vector<type::Type const *> ret_types;
      for (auto const *expr : ret_node->exprs()) {
        ret_types.push_back(c->type_of(expr));
      }
      auto *t = Tup(std::move(ret_types));
      types.emplace(t);
      saved_ret_types.emplace(ret_node, t);
    } else {
      UNREACHABLE();  // TODO
    }
  }

  auto type_params = node->params().Transform([&](auto const &param) {
    return ASSERT_NOT_NULL(c->type_of(param.get()));
  });

  if (not node->outputs()) {
    std::vector<type::Type const *> output_type_vec(
        std::make_move_iterator(types.begin()),
        std::make_move_iterator(types.end()));

    if (types.size() > 1) { NOT_YET("log an error"); }
    auto f = type::Func(std::move(type_params), std::move(output_type_vec));
    return c->set_result(node, type::QualType::Constant(f));

  } else {
    auto *node_type = c->type_of(node);
    auto outs       = ASSERT_NOT_NULL(node_type)->as<type::Function>().output();
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *n : c->data_.extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            if (not ret_node->exprs().empty()) {
              c->diag().Consume(diagnostic::NoReturnTypes{
                  .range = ret_node->span,
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
        for (auto *n : c->data_.extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *t = ASSERT_NOT_NULL(saved_ret_types.at(ret_node));
            if (t == outs[0]) { continue; }
            c->diag().Consume(diagnostic::ReturnTypeMismatch{
                .actual   = t,
                .expected = outs[0],
                .range    = ret_node->span,
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
        for (auto *n : c->data_.extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *expr_type = ASSERT_NOT_NULL(saved_ret_types.at(ret_node));
            if (expr_type->is<type::Tuple>()) {
              auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
              if (tup_entries.size() != outs.size()) {
                c->diag().Consume(diagnostic::ReturningWrongNumber{
                    .actual   = (expr_type->is<type::Tuple>()
                                   ? expr_type->as<type::Tuple>().size()
                                   : 1),
                    .expected = outs.size(),
                    .range    = ret_node->span,
                });
                return type::QualType::Error();
              } else {
                bool err = false;
                for (size_t i = 0; i < tup_entries.size(); ++i) {
                  if (tup_entries.at(i) != outs.at(i)) {
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
                        .range    = ret_node->span,
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
                  .range    = ret_node->span,
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

type::QualType Compiler::VerifyConcreteFnLit(ast::FunctionLiteral const *node) {
  // Parameter types can be dependent, so we in general need to bail out after
  // any error.
  //
  // TODO we can actually continue so long as we don't use a dependency of a
  // failure.
  core::Params<type::Type const *> input_type_params;
  input_type_params.reserve(node->params().size());
  for (auto &d : node->params()) {
    ASSIGN_OR(return _, auto result, Visit(d.value.get(), VerifyTypeTag{}));
    input_type_params.append(d.name, result.type(), d.flags);
  }

  std::vector<type::Type const *> output_type_vec;
  bool error   = false;
  auto outputs = node->outputs();
  if (outputs) {
    output_type_vec.reserve(outputs->size());
    for (auto *output : *outputs) {
      auto result = Visit(output, VerifyTypeTag{});
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
        output_type_vec.at(i) = type_of(decl);
      } else {
        ASSERT(output_type_vec.at(i) == type::Type_);
        output_type_vec.at(i) = interpretter::EvaluateAs<type::Type const *>(
            MakeThunk((*outputs)[i], type::Type_));
      }
    }

    return set_result(
        node, type::QualType::Constant(type::Func(std::move(input_type_params),
                                                  std::move(output_type_vec))));
  } else {
    return set_result(node, VerifyBody(this, node));
  }
}

static std::optional<type::Quals> VerifyAndGetQuals(
    Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  bool err          = false;
  type::Quals quals = type::Quals::All();
  for (auto *expr : exprs) {
    auto r = v->Visit(expr, VerifyTypeTag{});
    err |= not r.ok();
    if (not err) { quals &= r.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

static type::QualType AccessTypeMember(Compiler *c, ast::Access const *node,
                                       type::QualType operand_result) {
  if (not operand_result.constant()) {
    c->diag().Consume(
        diagnostic::NonConstantTypeMemberAccess{.range = node->span});
    return type::QualType::Error();
  }
  // TODO We may not be allowed to evaluate node:
  //    f ::= (T: type) => T.key
  // We need to know that T is const
  auto *evaled_type = interpretter::EvaluateAs<type::Type const *>(
      c->MakeThunk(node->operand(), operand_result.type()));

  // For enums and flags, regardless of whether we can get the value, it's
  // clear that node is supposed to be a member so we should emit an error but
  // carry on assuming that node is an element of that enum type.
  if (auto *e = evaled_type->if_as<type::Enum>()) {
    if (not e->Get(node->member_name()).has_value()) {
      c->diag().Consume(diagnostic::MissingMember{
          .range  = node->span,
          .member = std::string{node->member_name()},
          .type   = evaled_type,
      });
    }
    return c->set_result(node, type::QualType::Constant(evaled_type));
  } else if (auto *f = evaled_type->if_as<type::Flags>()) {
    if (not f->Get(node->member_name()).has_value()) {
      c->diag().Consume(diagnostic::MissingMember{
          .range  = node->span,
          .member = std::string{node->member_name()},
          .type   = evaled_type,
      });
    }
    return c->set_result(node, type::QualType::Constant(evaled_type));
  } else {
    // TODO what about structs? Can structs have constant members we're
    // allowed to access?
    c->diag().Consume(diagnostic::TypeHasNoMembers{
        .range = node->span,
    });
    return type::QualType::Error();
  }
}

static type::QualType AccessStructMember(Compiler *c, ast::Access const *node,
                                         type::QualType operand_result) {
  auto const &s      = operand_result.type()->as<type::Struct>();
  auto const *member = s.field(node->member_name());
  if (member == nullptr) {
    c->diag().Consume(diagnostic::MissingMember{
        .range  = node->span,
        .member = std::string{node->member_name()},
        .type   = &s,
    });
    return type::QualType::Error();
  }
  if (c->module() != s.defining_module() and
      not member->contains_hashtag(
          ast::Hashtag(ast::Hashtag::Builtin::Export))) {
    c->diag().Consume(diagnostic::NonExportedMember{
        .member = std::string{node->member_name()},
        .type   = &s,
        .range  = node->span,
    });
  }

  return c->set_result(
      node, type::QualType(member->type, operand_result.constant()
                                             ? type::Quals::Const()
                                             : type::Quals::Unqualified()));
}

static type::QualType AccessModuleMember(Compiler *c, ast::Access const *node,
                                         type::QualType operand_result) {
  DEBUG_LOG("AccessModuleMember")(node->DebugString());
  if (not operand_result.constant()) {
    c->diag().Consume(diagnostic::NonConstantModuleMemberAccess{
        .range = node->span,
    });
    return type::QualType::Error();
  }

  DEBUG_LOG("AccessModuleMember")(node->DebugString());
  // TODO this is a common pattern for dealing with imported modules. Extract
  // it.
  auto *mod = interpretter::EvaluateAs<CompiledModule const *>(
      c->MakeThunk(node->operand(), type::Module));
  auto decls = mod->declarations(node->member_name());
  switch (decls.size()) {
    case 0: {
      NOT_YET("Log an error, no such symbol in module.");
    } break;
    case 1: {
      type::Type const *t = mod->type_of(decls[0]);
      if (t == nullptr) {
        c->diag().Consume(diagnostic::NoExportedSymbol{
            .range = node->span,
        });
        return type::QualType::Error();
      }
      DEBUG_LOG("AccessModuleMember")
      ("Setting type of ", node, " to ", type::QualType::Constant(t), " on ",
       c);
      return c->set_result(node, type::QualType::Constant(t));

    } break;
    default: {
      NOT_YET("Ambiguous, multiple possible symbols exported by module.");
    } break;
  }
  UNREACHABLE();
}

type::QualType Compiler::Visit(ast::Access const *node, VerifyTypeTag) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto operand_result,
                   Visit(node->operand(), VerifyTypeTag{}));

  auto [base_type, num_derefs] = DereferenceAll(operand_result.type());
  if (base_type == type::Type_) {
    return AccessTypeMember(this, node, operand_result);
  } else if (base_type == type::Module) {
    return AccessModuleMember(this, node, operand_result);
  } else {
    auto quals = operand_result.quals();
    if (num_derefs > 0) { quals |= type::Quals::Ref(); }

    if (base_type == type::ByteView) {
      return set_result(node, type::QualType(type::Int64, quals));
    } else if (auto *s = base_type->if_as<type::Struct>()) {
      return AccessStructMember(this, node, type::QualType(base_type, quals));
    } else {
      diag().Consume(diagnostic::MissingMember{
          .range  = node->span,
          .member = std::string{node->member_name()},
          .type   = base_type,
      });
      return type::QualType::Error();
    }
  }
}

type::QualType Compiler::Visit(ast::ArrayLiteral const *node, VerifyTypeTag) {
  if (node->empty()) {
    return set_result(node, type::QualType::Constant(type::EmptyArray));
  }

  ASSIGN_OR(return type::QualType::Error(), auto expr_results,
                   VerifyWithoutSetting(this, node->elems()));
  auto *t           = expr_results.front().type();
  type::Quals quals = type::Quals::Const();
  for (auto expr_result : expr_results) {
    quals &= expr_result.quals();
    if (expr_result.type() != t) {
      diag().Consume(diagnostic::InconsistentArrayType{
          .range = node->span,
      });
      return type::QualType::Error();
    }
  }
  return set_result(node,
                    type::QualType(type::Arr(expr_results.size(), t), quals));
}

type::QualType Compiler::Visit(ast::ArrayType const *node, VerifyTypeTag) {
  std::vector<type::QualType> length_results;
  length_results.reserve(node->lengths().size());
  auto quals = type::Quals::Const();
  for (auto const &len : node->lengths()) {
    auto result = Visit(len, VerifyTypeTag{});
    quals &= result.quals();
    length_results.push_back(result);
    if (result.type() != type::Int64) {
      diag().Consume(diagnostic::NonIntegralArrayLength{
          .range = node->span,
      });
    }
  }

  auto data_qual_type = Visit(node->data_type(), VerifyTypeTag{});
  quals &= data_qual_type.quals();
  if (data_qual_type.type() != type::Type_) {
    diag().Consume(diagnostic::ArrayDataTypeNotAType{
        .range = node->data_type()->span,
    });
  }

  return set_result(node, type::QualType(type::Type_, quals));
}

static bool IsTypeOrTupleOfTypes(type::Type const *t) {
  return t == type::Type_ or t->is<type::Tuple>();
}

type::QualType Compiler::Visit(ast::Binop const *node, VerifyTypeTag) {
  auto lhs_qual_type = Visit(node->lhs(), VerifyTypeTag{});
  auto rhs_qual_type = Visit(node->rhs(), VerifyTypeTag{});
  if (not lhs_qual_type.ok() or not rhs_qual_type.ok()) {
    return type::QualType::Error();
  }

  using frontend::Operator;
  switch (node->op()) {
    case Operator::Assign: {
      // TODO if lhs is reserved?
      if (not VerifyAssignment(diag(), node->span, lhs_qual_type,
                               rhs_qual_type)) {
        return type::QualType::Error();
      }
      return type::QualType::NonConstant(type::Void());
    } break;
    case Operator::XorEq:
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return set_result(node, lhs_qual_type);
      } else {
        diag().Consume(diagnostic::XorEqNeedsBoolOrFlags{
            .range = node->span,
        });
        return type::QualType::Error();
      }
    case Operator::AndEq:
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return set_result(node, lhs_qual_type);
      } else {
        diag().Consume(diagnostic::AndEqNeedsBoolOrFlags{
            .range = node->span,
        });
        return type::QualType::Error();
      }
    case Operator::OrEq:
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return set_result(node, lhs_qual_type);
      } else {
        diag().Consume(diagnostic::OrEqNeedsBoolOrFlags{
            .range = node->span,
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
        return set_result(node, type::QualType((return_type), quals));         \
      } else {                                                                 \
        diag().Consume(diagnostic::ArithmeticBinaryOperatorTypeMismatch{       \
            .lhs_type = lhs_qual_type.type(),                                  \
            .rhs_type = rhs_qual_type.type(),                                  \
            .range    = node->span,                                            \
        });                                                                    \
        return type::QualType::Error();                                        \
      }                                                                        \
    } else {                                                                   \
      /* TODO Support calling with constants */                                \
      return VerifyBinaryOverload(                                             \
          this, symbol, node,                                                  \
          type::Typed(ir::Results{}, lhs_qual_type.type()),                    \
          type::Typed(ir::Results{}, rhs_qual_type.type()));                   \
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
          return set_result(node, type::QualType(lhs_qual_type.type(), quals));
        } else {
          diag().Consume(diagnostic::ArithmeticBinaryOperatorTypeMismatch{
              .lhs_type = lhs_qual_type.type(),
              .rhs_type = rhs_qual_type.type(),
              .range    = node->span});
          return type::QualType::Error();
        }
      } else {
        // TODO support calling with constants.
        return VerifyBinaryOverload(
            this, "+", node, type::Typed(ir::Results{}, lhs_qual_type.type()),
            type::Typed(ir::Results{}, rhs_qual_type.type()));
      }
    } break;
    case Operator::AddEq: {
      auto quals =
          type::Quals::Const() & lhs_qual_type.quals() & rhs_qual_type.quals();
      if (type::IsNumeric(lhs_qual_type.type()) and
          type::IsNumeric(rhs_qual_type.type())) {
        if (lhs_qual_type.type() == rhs_qual_type.type()) {
          return set_result(node, type::QualType(type::Void(), quals));
        } else {
          diag().Consume(diagnostic::ArithmeticBinaryOperatorTypeMismatch{
              .lhs_type = lhs_qual_type.type(),
              .rhs_type = rhs_qual_type.type(),
              .range    = node->span});
          return type::QualType::Error();
        }
      } else {
        // TODO support calling with constants.
        return VerifyBinaryOverload(
            this, "+=", node, type::Typed(ir::Results{}, lhs_qual_type.type()),
            type::Typed(ir::Results{}, rhs_qual_type.type()));
      }
    } break;
    case Operator::Arrow: {
      type::Type const *t = type::Type_;
      if (not IsTypeOrTupleOfTypes(lhs_qual_type.type())) {
        t = nullptr;
        diag().Consume(diagnostic::NonTypeFunctionInput{
            .range = node->span,
        });
      }

      if (not IsTypeOrTupleOfTypes(rhs_qual_type.type())) {
        t = nullptr;
        diag().Consume(diagnostic::NonTypeFunctionOutput{
            .range = node->span,
        });
      }

      if (t == nullptr) { return type::QualType::Error(); }

      return set_result(
          node, type::QualType(type::Type_,
                               lhs_qual_type.quals() & rhs_qual_type.quals()));
    }
    default: UNREACHABLE();
  }
  UNREACHABLE(stringify(node->op()));
}

type::QualType Compiler::Visit(ast::BlockLiteral const *node, VerifyTypeTag) {
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { Visit(b, VerifyTypeTag{}); }
  for (auto *a : node->after()) { Visit(a, VerifyTypeTag{}); }

  return set_result(node, type::QualType::Constant(type::Block));
}

std::vector<core::FnArgs<type::QualType>> Compiler::VerifyBlockNode(
    ast::BlockNode const *node) {
  for (auto &param : node->params()) {
    Visit(param.value.get(), VerifyTypeTag{});
  }
  for (auto *stmt : node->stmts()) { Visit(stmt, VerifyTypeTag{}); }
  set_result(node, type::QualType::Constant(type::Block));

  auto const &yields = data_.extraction_map_[node];
  // TODO this setup is definitely wrong because it doesn't account for
  // multiple yields correctly. For example,
  //
  // ```
  //  result: int32 | bool = if (cond) then {
  //    << 3
  //  } else if (other_cond) then {
  //    << 4
  //  } else {
  //    << true
  //  }
  //  ```
  std::vector<core::FnArgs<type::QualType>> result;
  for (auto *yield : yields) {
    auto &back = result.emplace_back();
    // TODO actually fill a fnargs
    for (auto *yield_expr : yields[0]->as<ast::YieldStmt>().exprs()) {
      auto q = qual_type_of(yield_expr);
      ASSERT(q.has_value() == true);
      back.pos_emplace(*q);
    }
  }

  return result;
}

type::QualType Compiler::Visit(ast::BlockNode const *node, VerifyTypeTag) {
  UNREACHABLE("Should be called via Compiler::VerifyBlockNode");
}

type::QualType Compiler::Visit(ast::BuiltinFn const *node, VerifyTypeTag) {
  return set_result(node, type::QualType::Constant(node->value().type()));
}

static ast::OverloadSet FindOverloads(
    ast::Scope const *scope, std::string_view token,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  ast::OverloadSet os(scope, token);
  for (type::Typed<ir::Results> const &result : args) {
    AddAdl(&os, token, result.type());
  };
  DEBUG_LOG("FindOverloads")
  ("Found ", os.members().size(), " overloads for '", token, "'");
  return os;
}

std::optional<ast::OverloadSet> MakeOverloadSet(
    Compiler *c, ast::Expression const *expr,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  if (auto *id = expr->if_as<ast::Identifier>()) {
    return FindOverloads(expr->scope(), id->token(), args);
  } else if (auto *acc = expr->if_as<ast::Access>()) {
    ASSIGN_OR(return std::nullopt,  //
                     auto result, c->Visit(acc->operand(), VerifyTypeTag{}));
    if (result.type() == type::Module) {
      // TODO this is a common pattern for dealing with imported modules.
      // Extract it.
      auto *mod = interpretter::EvaluateAs<CompiledModule const *>(
          c->MakeThunk(acc->operand(), type::Module));
      return FindOverloads(mod->scope(), acc->member_name(), args);
    }
  } else {
    ASSIGN_OR(return std::nullopt,  //
                     std::ignore, c->Visit(expr, VerifyTypeTag{}));
  }

  ast::OverloadSet os;
  os.insert(expr);
  // TODO ADL for node?
  return os;
}

template <typename EPtr, typename StrType>
static type::QualType VerifyCall(
    Compiler *c, ast::BuiltinFn const *b,
    core::FnArgs<EPtr, StrType> const &args,
    core::FnArgs<type::Typed<ir::Results>> const &arg_results) {
  // TODO for builtin's consider moving all the messages into an enum.
  switch (b->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      bool err = false;
      if (not arg_results.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = "Built-in function `foreign` cannot be called with "
                       "named arguments.",
        });
        err = true;
      }

      size_t size = arg_results.size();
      if (size != 2u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = absl::StrCat("Built-in function `foreign` takes exactly "
                                    "two arguments (You provided ",
                                    size, ")."),
        });
        err = true;
      }

      if (not err) {
        if (arg_results.at(0).type() != type::ByteView) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range = b->span,
              .message =
                  absl::StrCat("First argument to `foreign` must be a "
                               "byte-view (You provided a(n) ",
                               arg_results.at(0).type()->to_string(), ")."),
          });
        }
        if (arg_results.at(0)->empty()) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->span,
              .message = "First argument to `foreign` must be a constant."});
        }
        if (arg_results.at(1).type() != type::Type_) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range = b->span,
              .message =
                  absl::StrCat("Second argument to `foreign` must be a type "
                               "(You provided a(n) ",
                               arg_results.at(0).type()->to_string(), ").")});
        }
        if (arg_results.at(1)->empty()) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->span,
              .message = "Second argument to `foreign` must be a constant."});
        }
      }
      auto *foreign_type = interpretter::EvaluateAs<type::Type const *>(
          c->MakeThunk(args.at(1), type::Type_));
      if (not foreign_type->template is<type::Function>() and
          not foreign_type->template is<type::Pointer>()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = "Builtin `foreign` may only be called when the second "
                       "argument is a pointer or a function type.",
        });
      }
      return type::QualType::Constant(foreign_type);
    } break;
    case ir::BuiltinFn::Which::Opaque:
      if (not arg_results.empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = "Built-in function `opaque` takes no arguments."});
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Opaque().type()->output()[0]);

    case ir::BuiltinFn::Which::Bytes: {
      size_t size = arg_results.size();
      if (not arg_results.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = "Built-in function `bytes` cannot be called with named "
                       "arguments."});
      } else if (size != 1u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = absl::StrCat(
                "Built-in function `bytes` takes exactly one argument "
                "(You provided ",
                size, ")."),
        });
      } else if (arg_results.at(0).type() != type::Type_) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range = b->span,
            .message =
                absl::StrCat("Built-in function `bytes` must take a single "
                             "argument of type `type` (You provided a(n) ",
                             arg_results.at(0).type()->to_string(), ").")});
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Bytes().type()->output()[0]);
    }
    case ir::BuiltinFn::Which::Alignment: {
      size_t size = arg_results.size();
      if (not arg_results.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = "Built-in function `alignment` cannot be called with "
                       "named arguments."});
      }
      if (size != 1u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->span,
            .message = absl::StrCat("Built-in function `alignment` takes "
                                    "exactly one argument (You provided ",
                                    size, ")."),
        });

      } else if (arg_results.at(0).type() != type::Type_) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range = b->span,
            absl::StrCat("Built-in function `alignment` must take a single "
                         "argument of "
                         "type `type` (you provided a(n) ",
                         arg_results.at(0).type()->to_string(), ")"),
        });
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Alignment().type()->output()[0]);
    }
    case ir::BuiltinFn::Which::DebugIr:
      // This is for debugging the compiler only, so there's no need to write
      // decent errors here.
      ASSERT(arg_results.size() == 0u);
      return type::QualType::Constant(type::Void());
  }
  UNREACHABLE();
}

static type::Typed<ir::Results> EvaluateIfConstant(Compiler *c,
                                                   ast::Expression const *expr,
                                                   type::QualType qt) {
  if (qt.constant()) {
    DEBUG_LOG("EvaluateIfConstant")
    ("Evaluating constant: ", expr->DebugString());
    return type::Typed(interpretter::Evaluate(c->MakeThunk(expr, qt.type())),
                       qt.type());
  } else {
    return type::Typed(ir::Results{}, qt.type());
  }
}

static std::optional<core::FnArgs<type::Typed<ir::Results>, std::string_view>>
VerifyFnArgs(
    Compiler *c,
    core::FnArgs<ast::Expression const *, std::string_view> const &args) {
  bool err         = false;
  auto arg_results = args.Transform([&](ast::Expression const *expr) {
    auto expr_qual_type = c->Visit(expr, VerifyTypeTag{});
    err |= not expr_qual_type.ok();
    if (err) {
      DEBUG_LOG("VerifyFnArgs")("Error with: ", expr->DebugString());
      return type::Typed(ir::Results{},
                         static_cast<type::Type const *>(nullptr));
    }
    return EvaluateIfConstant(c, expr, expr_qual_type);
  });

  if (err) { return std::nullopt; }
  return arg_results;
}

type::QualType Compiler::Visit(ast::Call const *node, VerifyTypeTag) {
  ASSIGN_OR(
      return type::QualType::Error(),  //
             auto arg_results,
             VerifyFnArgs(this, node->args()));
  // TODO handle cyclic dependencies in call arguments.

  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    // TODO: Should we allow these to be overloaded?
    ASSIGN_OR(return type::QualType::Error(), auto result,
                     VerifyCall(this, b, node->args(), arg_results));
    return set_result(node, type::QualType(result.type(), result.quals()));
  }

  ASSIGN_OR(return type::QualType::Error(),  //
                   auto os, MakeOverloadSet(this, node->callee(), arg_results));
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto table,
                   FnCallDispatchTable::Verify(this, os, arg_results));
  auto result = table.result_qual_type();
  data_.set_dispatch_table(node, std::move(table));
  DEBUG_LOG("dispatch-verify")("Resulting type of dispatch is ", result);
  return set_result(node, result);
}

type::QualType Compiler::Visit(ast::Cast const *node, VerifyTypeTag) {
  auto expr_qual_type = Visit(node->expr(), VerifyTypeTag{});
  auto type_result = Visit(node->type(), VerifyTypeTag{});
  if (not expr_qual_type.ok() or not type_result.ok()) {
    return type::QualType::Error();
  }

  if (type_result.type() != type::Type_) {
    diag().Consume(diagnostic::CastToNonType{
        .range = node->span,
    });
    return type::QualType::Error();
  }
  if (not type_result.constant()) {
    diag().Consume(diagnostic::CastToNonConstantType{
        .range = node->span,
    });
    return type::QualType::Error();
  }
  auto *t = ASSERT_NOT_NULL(interpretter::EvaluateAs<type::Type const *>(
      MakeThunk(node->type(), type::Type_)));
  if (t->is<type::Struct>()) {
    // TODO do you ever want to support overlaods that accepts constants?
    return VerifyUnaryOverload(
        this, "as", node, type::Typed(ir::Results{}, expr_qual_type.type()));
  } else {
    if (not type::CanCast(expr_qual_type.type(), t)) {
      diag().Consume(diagnostic::InvalidCast{
          .from  = expr_qual_type.type(),
          .to    = t,
          .range = node->span,
      });
      NOT_YET("log an error", expr_qual_type.type(), t);
    }

    return set_result(node, type::QualType(t, expr_qual_type.quals()));
  }
}

type::QualType Compiler::Visit(ast::ChainOp const *node, VerifyTypeTag) {
  std::vector<type::QualType> results;
  results.reserve(node->exprs().size());
  for (auto *expr : node->exprs()) {
    results.push_back(Visit(expr, VerifyTypeTag{}));
  }
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
      return set_result(node, type::QualType::Constant(last.type()));
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
      return set_result(node, type::QualType(first_expr_type, quals));
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
          return VerifyBinaryOverload(
              this, token, node,
              type::Typed(ir::Results{}, lhs_qual_type.type()),
              type::Typed(ir::Results{}, rhs_qual_type.type()));
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
                          node->exprs()[i]->span.begin(),
                          node->exprs()[i + 1]->span.end()),
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
                          node->exprs()[i]->span.begin(),
                          node->exprs()[i + 1]->span.end()),
                  });
                  return type::QualType::Error();
              }
            } break;
            default: UNREACHABLE("Expecting a ChainOp operator type.");
          }
        }
      }

      return set_result(node, type::QualType(type::Bool, quals));
    }
  }
}

type::QualType Compiler::Visit(ast::CommaList const *node, VerifyTypeTag) {
  ASSIGN_OR(
      return type::QualType::Error(), auto results,
             VerifyWithoutSetting(
                 this, base::PtrSpan<ast::Expression const>(node->exprs_)));
  std::vector<type::Type const *> ts;
  ts.reserve(results.size());
  type::Quals quals = type::Quals::Const();
  for (auto const &r : results) {
    ts.push_back(r.type());
    quals &= r.quals();
  }
  return set_result(node, type::QualType(std::move(ts), quals));
}

// TODO set qualifiers correctly here.
type::QualType Compiler::Visit(ast::Declaration const *node, VerifyTypeTag) {
  // Declarations may have already been computed. Essentially the first time
  // we see an identifier (either a real identifier node, or a declaration, we
  // need to verify the type, but we only want to do node once.
  if (auto q = qual_type_of(node)) { return *q; }
  type::QualType node_qual_type;
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      auto type_expr_result = Visit(node->type_expr(), VerifyTypeTag{});
      if (not type_expr_result) {
        return set_result(node, type::QualType::Error());
      } else if (not type_expr_result.constant()) {
        // Hmm, not necessarily an error. Example (not necessarily minimal):
        //
        //   S ::= (x: any`T) => struct {
        //     _val: T = x
        //   }
        //
        NOT_YET("log an error", node->DebugString());
        return set_result(node, type::QualType::Error());
      }
      auto *type_expr_type = type_expr_result.type();
      if (type_expr_type == type::Type_) {
        node_qual_type = set_result(
            node,
            type::QualType(
                ASSERT_NOT_NULL(interpretter::EvaluateAs<type::Type const *>(
                    MakeThunk(node->type_expr(), type_expr_type))),
                (node->flags() & ast::Declaration::f_IsConst)
                    ? type::Quals::Const()
                    : type::Quals::Unqualified()));

        if (not(node->flags() & ast::Declaration::f_IsFnParam) and
            not node_qual_type.type()->IsDefaultInitializable()) {
          diag().Consume(diagnostic::NoDefaultValue{
              .type  = node_qual_type.type(),
              .range = node->span,
          });
        }

      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->type_expr()->span,
            .type  = type_expr_type,
        });
        return set_result(node, type::QualType::Error());
      }
    } break;
    case ast::Declaration::kInferred: {
      DEBUG_LOG("Declaration")("Verifying, ", node->id());

      ASSIGN_OR(return set_result(node, type::QualType::Error()),
                       auto init_val_result,
                       Visit(node->init_val(), VerifyTypeTag{}));

      auto reason = Inferrable(init_val_result.type());
      if (reason != diagnostic::UninferrableType::Reason::kInferrable) {
        diag().Consume(diagnostic::UninferrableType{
            .reason = reason,
            .range  = node->init_val()->span,
        });
        return set_result(node, type::QualType::Error());
      }

      if (not VerifyInitialization(diag(), node->span, init_val_result,
                                   init_val_result)) {
        return set_result(node, type::QualType::Error());
      }

      node_qual_type = set_result(
          node, type::QualType(init_val_result.type(),
                               (node->flags() & ast::Declaration::f_IsConst)
                                   ? type::Quals::Const()
                                   : type::Quals::Unqualified()));
      DEBUG_LOG("Declaration")
      ("Verified, ", node->id(), ": ", node_qual_type.type()->to_string());
    } break;
    case ast::Declaration::kInferredAndUninitialized: {
      diag().Consume(diagnostic::UninferrableType{
          .reason = diagnostic::UninferrableType::Reason::kHole,
          .range  = node->init_val()->span,
      });
      if (node->flags() & ast::Declaration::f_IsConst) {
        diag().Consume(diagnostic::UninitializedConstant{
            .range = node->span,
        });
      }
      return set_result(node, type::QualType::Error());
    } break;
    case ast::Declaration::kCustomInit: {
      auto init_val_qual_type = Visit(node->init_val(), VerifyTypeTag{});
      bool error              = not init_val_qual_type.ok();
      auto type_expr_result   = Visit(node->type_expr(), VerifyTypeTag{});
      auto *type_expr_type    = type_expr_result.type();

      if (type_expr_type == nullptr) {
        error = true;
      } else if (type_expr_type == type::Type_) {
        if (not type_expr_result.constant()) {
          NOT_YET("log an error");
          error = true;
        } else {
          node_qual_type = set_result(
              node, type::QualType::Constant(
                        interpretter::EvaluateAs<type::Type const *>(
                            MakeThunk(node->type_expr(), type::Type_))));
        }

        if (node_qual_type and init_val_qual_type) {
          error |= not VerifyInitialization(diag(), node->span, node_qual_type,
                                            init_val_qual_type);
        }
      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->type_expr()->span,
            type_expr_type,
        });
        error = true;
      }

      if (error) { return set_result(node, type::QualType::Error()); }
    } break;
    case ast::Declaration::kUninitialized: {
      ASSIGN_OR(return set_result(node, type::QualType::Error()),
                       auto type_expr_result,
                       Visit(node->type_expr(), VerifyTypeTag{}));
      auto *type_expr_type = type_expr_result.type();
      if (type_expr_type == type::Type_) {
        if (not type_expr_result.constant()) {
          NOT_YET("log an error");
          return set_result(node, type::QualType::Error());
        }
        node_qual_type = set_result(
            node, type::QualType::Constant(
                      interpretter::EvaluateAs<type::Type const *>(
                          MakeThunk(node->type_expr(), type::Type_))));
      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->type_expr()->span,
            type_expr_type,
        });
        return set_result(node, type::QualType::Error());
      }

      if (node->flags() & ast::Declaration::f_IsConst) {
        diag().Consume(diagnostic::UninitializedConstant{
            .range = node->span,
        });
        return set_result(node, type::QualType::Error());
      }

    } break;
    default: UNREACHABLE(node->DebugString());
  }

  if (node->id().empty()) {
    if (node_qual_type.type() == type::Module) {
      // TODO check shadowing against other modules?
      // TODO what if no init val is provded? what if not constant?
      node->scope()->embedded_modules_.insert(
          interpretter::EvaluateAs<module::BasicModule const *>(
              MakeThunk(node->init_val(), type::Module)));
      return set_result(node, type::QualType::Constant(type::Module));
    } else {
      NOT_YET(node_qual_type, node->DebugString());
    }
  }

  ASSERT(node_qual_type != type::QualType::Error()) << node->DebugString();

  // Gather all declarations with the same identifer that are visible in this
  // scope or that are in a scope which for which this declaration would be
  // visible. In other words, look both up and down the scope tree for
  // declarations of this identifier.
  //
  // It's tempting to assume we only need to look in one direction because we
  // would catch any ambiguity at a later time. However this is not correct.
  // For instance, consider this example:
  //
  // ```
  // if (cond) then {
  //   a := 4
  // }
  // a := 3  // Error: Redeclaration of `a`.
  // ```
  //
  // There is a redeclaration of `a` that needs to be caught. However, If we
  // only look towards the root of the scope tree, we will first see `a := 4`
  // which is not ambiguous. Later we will find `a := 3` which should have
  // been found but wasn't due to the fact that we saw the declaration that
  // was further from the root first while processing.
  //
  // The problem can be described mathematically as follows:
  //
  // Define *scope tree order* to be the partial order defined by D1 <= D2 iff
  // D1's path to the scope tree root is a prefix of D2's path to the scope
  // tree root. Define *processing order* to be the order in which nodes have
  // their types verified.
  //
  // The problem is that scope tree order does not have processing order as a
  // linear extension.
  //
  // To fix this particular problem, we need to make sure we check all
  // declarations that may be ambiguous regardless of whether they are above
  // or below `node` on the scope tree. However, we only want to look at the
  // ones which have been previously processed. This can be checked by looking
  // to see if we have saved the result of this declaration. We can also skip
  // out if the result was an error.
  //
  // TODO Skipping out on errors *might* reduce the fidelity of future
  // compilation work by not finding ambiguities that we should have.
  bool failed_shadowing = false;
  type::Typed<ast::Declaration const *> typed_node_decl(node,
                                                        node_qual_type.type());
  for (auto const *decl :
       module::AllAccessibleDecls(node->scope(), node->id())) {
    if (decl == node) { continue; }
    auto q = qual_type_of(decl);
    if (not q) { continue; }
    auto *t = q->type();
    if (not t) { continue; }

    type::Typed<ast::Declaration const *> typed_decl(decl, t);
    if (Shadow(this, typed_node_decl, typed_decl)) {
      failed_shadowing = true;
      diag().Consume(diagnostic::ShadowingDeclaration{
          .range1 = node->span,
          .range2 = (*typed_decl)->span,
      });
    }
  }

  if (failed_shadowing) {
    // TODO node may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else
    // on a different branch could find it unambiguously. It's also just a
    // hack from the get-go so maybe we should just do it the right way.
    return set_result(node, type::QualType::Error());
  }

  return VerifySpecialFunctions(this, node, node_qual_type.type());
}

type::QualType Compiler::Visit(ast::DesignatedInitializer const *node,
                               VerifyTypeTag) {
  // TODO include fields.
  // TODO constant only when all fields are constant.
  auto type_type = Visit(node->type(), VerifyTypeTag{});
  if (type_type != type::QualType::Constant(type::Type_)) {
    NOT_YET("log an error", type_type, " vs ",
            type::QualType::Constant(type::Type_));
  }

  type::Type const *expr_type = interpretter::EvaluateAs<type::Type const *>(
      MakeThunk(node->type(), type::Type_));

  auto *struct_type = expr_type->if_as<type::Struct>();
  if (not struct_type) { NOT_YET("log an error"); }

  type::Quals quals = type::Quals::Const();
  for (auto &[field, expr] : node->assignments()) {
    type::QualType initializer_qual_type = Visit(expr.get(), VerifyTypeTag{});
    if (not initializer_qual_type) {
      // If there was an error we still want to verify all other initializers
      // and we still want to claim this expression has the same type, but
      // we'll just give up on it being a constant.
      quals = type::Quals::Unqualified();
      continue;
    }
    if (auto *struct_field = struct_type->field(field)) {
      if (not type::CanCast(initializer_qual_type.type(), struct_field->type)) {
        NOT_YET("log an error: ", initializer_qual_type.type()->to_string(),
                struct_field->type->to_string());
      }
      quals &= initializer_qual_type.quals();
    } else {
      NOT_YET("log an error");
      quals = type::Quals::Unqualified();
    }
  }

  return set_result(node, type::QualType(struct_type, quals));
}

type::QualType Compiler::Visit(ast::EnumLiteral const *node, VerifyTypeTag) {
  for (auto const &elem : node->elems()) {
    if (auto *decl = elem->if_as<ast::Declaration>()) {
      auto *t = Visit(decl->init_val(), VerifyTypeTag{}).type();
      ASSERT(type::IsIntegral(t) == true);
      // TODO determine what is allowed here and how to generate errors.
    }
  }

  return set_result(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::Visit(ast::FunctionLiteral const *node,
                               VerifyTypeTag) {
  for (auto const &p : node->params()) {
    if (p.value->flags() & ast::Declaration::f_IsConst) { goto generic; }

    // TODO There are other ways this could be generic. For example
    // (x: $x) -> () { .. }
  }
  return VerifyConcreteFnLit(node);

generic:
  absl::flat_hash_set<ast::DependencyNode> deps;
  for (auto const &p : node->params()) {
    deps.insert(ast::DependencyNode::MakeType(p.value.get()));
    if (p.value->flags() & ast::Declaration::f_IsConst) {
      deps.insert(ast::DependencyNode::MakeValue(p.value.get()));
    }
  }

  std::vector<ast::DependencyNode> ordered_nodes;
  node->parameter_dependency_graph().topologically([&](auto dep_node) {
    if (not deps.contains(dep_node)) { return; }
    ordered_nodes.push_back(dep_node);
  });

  // TODO: Capturing `this` compiler is dangerous and definitely wrong. We'll
  // have use-after-free bugs for anything cross-module.
  auto gen = [this, ordered_nodes(std::move(ordered_nodes))](
                 core::FnArgs<type::Typed<ir::Results>> const &args)
      -> type::Function const * {
    // TODO Add a new constant binding node for this.

    // TODO use the proper ordering.
    core::Params<type::Type const *> params;
    for (auto dep_node : ordered_nodes) {
      switch (dep_node.kind()) {
        case ast::DependencyNode::Kind::ArgValue: NOT_YET();
        case ast::DependencyNode::Kind::ArgType: NOT_YET();
        case ast::DependencyNode::Kind::ParamType: {
          // TODO check that it is indeed a type?
          // TODO What if there's no type_expr?
          // TODO always constant
          Visit(dep_node.decl()->type_expr(), VerifyTypeTag{});
          set_result(dep_node.decl()->type_expr(), type::QualType::Constant(type::Type_));
          auto const *t = interpretter::EvaluateAs<type::Type const *>(
              MakeThunk(dep_node.decl()->type_expr(), type::Type_));
          set_result(dep_node.decl(), type::QualType::Constant(t));
          params.append(dep_node.decl()->id(), t);
        } break;
        case ast::DependencyNode::Kind::ParamValue: {
          // TODO argument get the argument associated to this parameter.
          current_constants_->binding().set_slot(
              dep_node.decl(), ir::Results(*args.pos()[0]).extract_buffer());
        } break;
      }
    }
    return type::Func(std::move(params), {});
  };

  return set_result(node, type::QualType::Constant(
                              new type::GenericFunction(std::move(gen))));
}

type::QualType Compiler::Visit(ast::Identifier const *node, VerifyTypeTag) {
  DEBUG_LOG("Identifier")(node->DebugString());
  for (auto iter = data_.cyc_deps_.begin(); iter != data_.cyc_deps_.end();
       ++iter) {
    if (*iter == node) {
      diagnostic::CyclicDependency cyclic_dep;
      cyclic_dep.cycle.reserve(std::distance(iter, data_.cyc_deps_.end()));
      for (; iter != data_.cyc_deps_.end(); ++iter) {
        cyclic_dep.cycle.emplace_back((*iter)->span, (*iter)->token());
      }
      diag().Consume(std::move(cyclic_dep));
      return type::QualType::Error();
    }
  }
  data_.cyc_deps_.push_back(node);
  base::defer d([&] { data_.cyc_deps_.pop_back(); });

  // `node->decl()` is not necessarily null. Because we may call VerifyType
  // many times in multiple contexts, it is null the first time, but not on
  // future iterations.
  //
  // TODO that means we should probably resolve identifiers ahead of
  // type verification, but I think we rely on type information to figure it
  // out for now so you'll have to undo that first.
  if (node->decl() == nullptr) {
    auto potential_decls =
        module::AllDeclsTowardsRoot(node->scope(), node->token());
    DEBUG_LOG("Identifier")(node->DebugString(), ": ", potential_decls);
    switch (potential_decls.size()) {
      case 1: {
        // TODO could it be that evn though there is only one declaration,
        // there's a bound constant of the same name? If so, we need to deal
        // with node case.
        const_cast<ast::Identifier *>(node)->set_decl(potential_decls[0]);
        if (node->decl() == nullptr) { return type::QualType::Error(); }
      } break;
      case 0:
        diag().Consume(diagnostic::UndeclaredIdentifier{
            .id    = node->token(),
            .range = node->span,
        });
        return type::QualType::Error();
      default:
        // TODO Should we allow the overload?
        diag().Consume(diagnostic::UnspecifiedOverload{
            .range = node->span,
        });
        return type::QualType::Error();
    }

    if (not(node->decl()->flags() & ast::Declaration::f_IsConst) and
        node->span.begin() < node->decl()->span.begin()) {
      diag().Consume(diagnostic::DeclOutOfOrder{
          .id         = node->token(),
          .decl_range = node->decl()->span,
          .use_range  = node->span,
      });
    }
  }

  // TODO node is because we may have determined the declartaion previously
  // with a different generic setup but not bound the type for node context.
  // But node is wrong in the sense that the declaration bound is possibly
  // dependent on the context.
  type::Type const *t = type_of(node->decl());

  if (t == nullptr) { return type::QualType::Error(); }
  return set_result(node, type::QualType(t, (node->decl()->flags() &
                                             ast::Declaration::f_IsConst)
                                                ? type::Quals::Const()
                                                : type::Quals::Ref()));
}

type::QualType Compiler::Visit(ast::Import const *node, VerifyTypeTag) {
  DEBUG_LOG("Import")(node->DebugString());
  ASSIGN_OR(return _, auto result, Visit(node->operand(), VerifyTypeTag{}));
  bool err = false;
  if (result.type() != type::ByteView) {
    // TODO allow (import) overload
    diag().Consume(diagnostic::InvalidImport{
        .range = node->operand()->span,
    });
    err = true;
  }

  if (not result.constant()) {
    diag().Consume(diagnostic::NonConstantImport{
        .range = node->operand()->span,
    });
    err = true;
  }

  if (err) { return type::QualType::Error(); }
  // TODO storing node might not be safe.
  auto src = interpretter::EvaluateAs<ir::String>(
      MakeThunk(node->operand(), type::ByteView));
  // TODO source name?

  frontend::FileName file_name{src.get()};
  ASSIGN_OR(diag().Consume(diagnostic::MissingModule{
      .source    = src.get(),
      .requestor = "TODO source",
  });
            return type::QualType::Error(),  //
                   auto pending_mod,
                   module::ImportModule<LibraryModule>(file_name));

  if (not pending_mod.valid()) { return type::QualType::Error(); }
  set_pending_module(node, pending_mod);
  return set_result(node, type::QualType::Constant(type::Module));
}

type::QualType Compiler::Visit(ast::Index const *node, VerifyTypeTag) {
  auto lhs_qual_type = Visit(node->lhs(), VerifyTypeTag{});
  auto rhs_qual_type = Visit(node->rhs(), VerifyTypeTag{});
  if (not lhs_qual_type.ok() or not rhs_qual_type.ok()) {
    return type::QualType::Error();
  }

  auto *index_type = rhs_qual_type.type()->if_as<type::Primitive>();
  if (not index_type or not index_type->is_integral()) {
    diag().Consume(diagnostic::InvalidIndexType{
        .range      = node->span,
        .type       = lhs_qual_type.type(),
        .index_type = lhs_qual_type.type(),
    });
  }

  auto quals = lhs_qual_type.quals() | type::Quals::Ref();
  if (not rhs_qual_type.constant()) { quals &= ~type::Quals::Const(); }
  if (lhs_qual_type.type() == type::ByteView) {
    return set_result(node, type::QualType(type::Nat8, quals));
  } else if (auto *lhs_array_type =
                 lhs_qual_type.type()->if_as<type::Array>()) {
    return set_result(node, type::QualType(lhs_array_type->data_type, quals));
  } else if (auto *lhs_buf_type =
                 lhs_qual_type.type()->if_as<type::BufferPointer>()) {
    return set_result(node, type::QualType(lhs_buf_type->pointee, quals));
  } else if (auto *tup = lhs_qual_type.type()->if_as<type::Tuple>()) {
    if (not rhs_qual_type.constant()) {
      diag().Consume(diagnostic::NonConstantTupleIndex{
          .range = node->span,
      });
      return type::QualType::Error();
    }

    int64_t index = [&]() -> int64_t {
      auto results = interpretter::Evaluate(MakeThunk(node->rhs(), index_type));
      if (index_type == type::Int8) { return results.get<int8_t>(0).value(); }
      if (index_type == type::Int16) { return results.get<int16_t>(0).value(); }
      if (index_type == type::Int32) { return results.get<int32_t>(0).value(); }
      if (index_type == type::Int64) { return results.get<int64_t>(0).value(); }
      if (index_type == type::Nat8) { return results.get<uint8_t>(0).value(); }
      if (index_type == type::Nat16) {
        return results.get<uint16_t>(0).value();
      }
      if (index_type == type::Nat32) {
        return results.get<uint32_t>(0).value();
      }
      if (index_type == type::Nat64) {
        return results.get<uint64_t>(0).value();
      }
      UNREACHABLE();
    }();

    if (index < 0 or index >= static_cast<int64_t>(tup->size())) {
      diag().Consume(diagnostic::IndexingTupleOutOfBounds{
          .range = node->span,
          .tuple = tup,
          .index = index,
      });
      return type::QualType::Error();
    }

    return set_result(node, type::QualType(tup->entries_[index], quals));

  } else {
    diag().Consume(diagnostic::InvalidIndexing{
        .range = node->span,
        .type  = lhs_qual_type.type(),
    });
    return type::QualType::Error();
  }
}

type::QualType Compiler::Visit(ast::Goto const *node, VerifyTypeTag) {
  for (auto const &option : node->options()) {
    for (auto const &expr : option.args()) {
      Visit(expr.get(), VerifyTypeTag{});
    }
  }
  return type::QualType::Constant(type::Void());
}

type::QualType Compiler::Visit(ast::Label const *node, VerifyTypeTag) {
  return set_result(node, type::QualType::Constant(type::Label));
}

type::QualType Compiler::Visit(ast::Jump const *node, VerifyTypeTag) {
  DEBUG_LOG("Jump")(node->DebugString());

  bool err                = false;
  type::Type const *state = nullptr;

  if (node->state()) {
    auto state_qual_type = Visit(node->state(), VerifyTypeTag{});
    err                  = not state_qual_type.ok();
    if (not err) { state = state_qual_type.type(); }
  }

  core::Params<type::Type const *> param_types =
      node->params().Transform([&](auto const &param) {
        auto v = Visit(param.get(), VerifyTypeTag{});
        err |= not v.ok();
        return v.type();
      });

  for (auto const *stmt : node->stmts()) { Visit(stmt, VerifyTypeTag{}); }

  return set_result(
      node, err ? type::QualType::Error()
                : type::QualType::Constant(type::Jmp(state, param_types)));
}

type::QualType Compiler::Visit(ast::ReturnStmt const *node, VerifyTypeTag) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::Visit(ast::YieldStmt const *node, VerifyTypeTag) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::Visit(ast::ScopeLiteral const *node, VerifyTypeTag) {
  auto verify_result = set_result(node, type::QualType::Constant(type::Scope));
  bool error         = false;
  if (node->state_type()) {
    type::QualType state_qual_type = Visit(node->state_type(), VerifyTypeTag{});
    if (state_qual_type != type::QualType(type::Type_, type::Quals::Const())) {
      // TODO check for non-const vs. not a type.
      diag().Consume(diagnostic::NonTypeScopeState{
          .type  = state_qual_type.type(),
          .range = node->state_type()->span,
      });
      error = true;
    }
  }

  absl::flat_hash_map<ast::Declaration const *, type::Type const *> types;
  for (auto const *decl : node->decls()) {
    auto qual_type = Visit(decl, VerifyTypeTag{});
    if (not qual_type.constant()) {
      error = true;
      NOT_YET("log an error");
    }
    types.emplace(decl, qual_type.type());
  }
  // TODO verify that it has at least one entry and exit point each.
  if (error) { return type::QualType::Error(); }

  if (not node->state_type()) { return verify_result; }
  auto *state_type_ptr = type::Ptr(interpretter::EvaluateAs<type::Type const *>(
      MakeThunk(node->state_type(), type::Type_)));
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

static absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> MakeJumpInits(
    Compiler *c, ast::OverloadSet const &os) {
  absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> inits;
  DEBUG_LOG("ScopeNode")
  ("Overload set for inits has size ", os.members().size());
  for (ast::Expression const *member : os.members()) {
    DEBUG_LOG("ScopeNode")(member->DebugString());
    auto *def = interpretter::EvaluateAs<ir::ScopeDef *>(
        c->MakeThunk(member, type::Scope));
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

type::QualType Compiler::Visit(ast::ScopeNode const *node, VerifyTypeTag) {
  DEBUG_LOG("ScopeNode")(node->DebugString());
  ASSIGN_OR(
      return type::QualType::Error(),  //
             auto arg_results,
             VerifyFnArgs(this, node->args()));
  // TODO handle cyclic dependencies in call arguments.

  ASSIGN_OR(return type::QualType::Error(),  //
                   auto os, MakeOverloadSet(this, node->name(), arg_results));
  auto inits = MakeJumpInits(this, os);

  DEBUG_LOG("ScopeNode")(inits);

  ASSIGN_OR(return type::QualType::Error(),  //
                   auto table,
                   ScopeDispatchTable::Verify(this, node, std::move(inits),
                                              arg_results));
  return data_.set_scope_dispatch_table(node, std::move(table));
}

type::QualType Compiler::Visit(ast::StructLiteral const *node, VerifyTypeTag) {
  bool err = false;
  for (auto const &field : node->fields()) {
    type::QualType type_expr_result;
    if (field.type_expr()) {
      type_expr_result = Visit(field.type_expr(), VerifyTypeTag{});
    }

    type::QualType init_val_result;
    if (field.init_val()) {
      init_val_result = Visit(field.init_val(), VerifyTypeTag{});
    }

    if ((field.type_expr() and not type_expr_result) or
        (field.init_val() and not init_val_result)) {
      err = true;
      continue;
    }

    if (field.type_expr() and not type_expr_result.constant()) {
      err = true;
      NOT_YET("Log an error, type must be constant");
    }

    if (field.init_val() and not init_val_result.constant()) {
      err = true;
      NOT_YET("Log an error, type must be constant");
    }

    if (field.init_val() and init_val_result != type_expr_result) {
      err = true;
      NOT_YET("log an error, type mismatch");
    }
  }

  if (err) { return set_result(node, type::QualType::Error()); }
  return set_result(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::Visit(ast::ParameterizedStructLiteral const *node,
                               VerifyTypeTag) {
  std::vector<type::Type const *> ts;
  ts.reserve(node->params().size());
  for (auto const &a : node->params()) {
    ts.push_back(Visit(&a, VerifyTypeTag{}).type());
  }
  if (absl::c_any_of(ts, [](type::Type const *t) { return t == nullptr; })) {
    return type::QualType::Error();
  }

  return set_result(node, type::QualType::Constant(
                              type::GenStruct(node->scope(), std::move(ts))));
}

type::QualType Compiler::Visit(ast::StructType const *node, VerifyTypeTag) {
  for (auto &arg : node->args_) { Visit(arg.get(), VerifyTypeTag{}); }
  return set_result(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::Visit(ast::Switch const *node, VerifyTypeTag) {
  // Don't allow switch to return references.
  type::Quals quals           = type::Quals::Const();
  type::Type const *expr_type = nullptr;
  if (node->expr()) {
    ASSIGN_OR(return _, auto result, Visit(node->expr(), VerifyTypeTag{}));
    quals &= result.quals();
    expr_type = result.type();
  }

  absl::flat_hash_set<type::Type const *> types;
  bool err = false;
  for (auto &[body, cond] : node->cases()) {
    auto cond_result = Visit(cond.get(), VerifyTypeTag{});
    auto body_result = Visit(body.get(), VerifyTypeTag{});
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
            .range = node->span,
        });
      }
    }
    // TODO if there's an error, an unorderded_set is not helpful for giving
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
    return set_result(node, type::QualType(type::Void(), quals));
  }
  auto some_type = *types.begin();
  if (absl::c_all_of(types,
                     [&](type::Type const *t) { return t == some_type; })) {
    // TODO node might be a constant.
    return set_result(node, type::QualType(some_type, quals));
  } else {
    NOT_YET("handle type error");
    return type::QualType::Error();
  }
}

type::QualType Compiler::Visit(ast::Terminal const *node, VerifyTypeTag) {
  return set_result(node,
                    type::QualType::Constant(type::Prim(node->basic_type())));
}

type::QualType Compiler::Visit(ast::Unop const *node, VerifyTypeTag) {
  ASSIGN_OR(return type::QualType::Error(), auto result,
                   Visit(node->operand(), VerifyTypeTag{}));
  auto *operand_type = result.type();

  switch (node->op()) {
    case frontend::Operator::Copy:
      if (not operand_type->IsCopyable()) {
        NOT_YET("log an error. not copyable");
      }
      // TODO Are copies always consts?
      return set_result(node, type::QualType(operand_type, result.quals()));
    case frontend::Operator::Move:
      if (not operand_type->IsMovable()) {
        NOT_YET("log an error. not movable");
      }
      // TODO Are copies always consts?
      return set_result(node, type::QualType(operand_type, result.quals()));
    case frontend::Operator::BufPtr:
      return set_result(node, type::QualType(operand_type, result.quals()));
    case frontend::Operator::TypeOf:
      return set_result(node, type::QualType(operand_type, result.quals()));
    case frontend::Operator::Eval:
      if (not result.constant()) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do node probably worth a
        // full pass over all verification code.
        diag().Consume(diagnostic::NonConstantEvaluation{
            .range = node->operand()->span,
        });
        return type::QualType::Error();
      } else {
        return set_result(node, type::QualType(operand_type, result.quals()));
      }
    case frontend::Operator::Which:
      if (not operand_type->is<type::Variant>()) {
        diag().Consume(diagnostic::WhichNonVariant{
            .type  = operand_type,
            .range = node->span,
        });
      }
      return set_result(node, type::QualType(type::Type_, result.quals()));
    case frontend::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        return set_result(
            node, type::QualType(operand_type->as<type::Pointer>().pointee,
                                 result.quals()));
      } else {
        diag().Consume(diagnostic::DereferencingNonPointer{
            .type  = operand_type,
            .range = node->span,
        });
        return type::QualType::Error();
      }
    case frontend::Operator::And:
      if ((result.quals() & type::Quals::Ref()) == type::Quals::Ref()) {
        result = type::QualType(type::Ptr(operand_type), result.quals());
      } else {
        diag().Consume(diagnostic::NonAddressableExpression{
            .range = node->span,
        });
        result = type::QualType::Error();
      }
      return set_result(node, result);
    case frontend::Operator::Mul:
      if (operand_type == type::Type_) {
        return set_result(node,
                          type::QualType(type::Type_, type::Quals::Const()));
      } else {
        NOT_YET("log an error, ", operand_type->to_string(),
                node->DebugString());
        return type::QualType::Error();
      }
    case frontend::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        return set_result(
            node, type::QualType(operand_type,
                                 result.quals() & type::Quals::Const()));
      } else if (operand_type->is<type::Struct>()) {
        // TODO do you ever want to support overlaods that accepts constants?
        return VerifyUnaryOverload(this, "-", node,
                                   type::Typed(ir::Results{}, result.type()));
      }
      NOT_YET();
      return type::QualType::Error();
    case frontend::Operator::Not:
      if (operand_type == type::Bool or operand_type->is<type::Enum>() or
          operand_type->is<type::Flags>()) {
        return set_result(
            node, type::QualType(operand_type,
                                 result.quals() & type::Quals::Const()));
      }
      if (operand_type->is<type::Struct>()) {
        // TODO do you ever want to support overlaods that accepts constants?
        return VerifyUnaryOverload(this, "-", node,
                                   type::Typed(ir::Results{}, result.type()));
      } else {
        NOT_YET("log an error");
        return type::QualType::Error();
      }
    case frontend::Operator::Needs:
      if (operand_type != type::Bool) {
        diag().Consume(diagnostic::PreconditionNeedsBool{
            .type  = operand_type,
            .range = node->operand()->span,
        });
      }
      if (not result.constant()) { NOT_YET(); }
      return set_result(node, type::QualType::Constant(type::Void()));
    case frontend::Operator::Ensure:
      if (operand_type != type::Bool) {
        diag().Consume(diagnostic::PostconditionNeedsBool{
            .type  = operand_type,
            .range = node->operand()->span,
        });
      }
      if (not result.constant()) { NOT_YET(); }
      return set_result(node, type::QualType::Constant(type::Void()));
    case frontend::Operator::VariadicPack: {
      if (not result.constant()) { NOT_YET("Log an error"); }
      // TODO could be a type, or a function returning a ty
      if (result.type() == type::Type_) {
        return set_result(node, type::QualType::Constant(type::Type_));
      } else if (result.type()->is<type::Function>()) {
        NOT_YET();
      }
      NOT_YET(*node);
    }
    default: UNREACHABLE(*node);
  }
}

}  // namespace compiler
