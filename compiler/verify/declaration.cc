#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify_assignment_and_initialization.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {
diagnostic::UninferrableType::Reason Inferrable(type::Type const *t) {
  if (t == type::NullPtr) {
    return diagnostic::UninferrableType::Reason::kNullPtr;
  }
  if (t == type::EmptyArray) {
    return diagnostic::UninferrableType::Reason::kEmptyArray;
  }
  if (auto *a = t->if_as<type::Array>()) { return Inferrable(a->data_type()); }
  if (auto *p = t->if_as<type::Pointer>()) { return Inferrable(p->pointee()); }
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
      auto reason = Inferrable(param.value.type());
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
      compiler->type_of(*decl1)->as<type::Function>().params(),
      compiler->type_of(*decl2)->as<type::Function>().params(),
      [](type::QualType lhs, type::QualType rhs) {
        return type::Meet(lhs.type(), rhs.type()) != nullptr;
      });
}

type::QualType VerifySpecialFunctions(Compiler *visitor,
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
          f->params()[0].value != f->params()[1].value or
          not f->params()[0].value.type()->is<type::Pointer>() or
          not f->params()[0]
                  .value.type()
                  ->as<type::Pointer>()
                  .pointee()
                  ->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect params type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s = f->params()[0]
                            .value.type()
                            ->as<type::Pointer>()
                            .pointee()
                            ->as<type::Struct>();

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
          f->params()[0].value != f->params()[1].value or
          not f->params()[0].value.type()->is<type::Pointer>() or
          not f->params()[0]
                  .value.type()
                  ->as<type::Pointer>()
                  .pointee()
                  ->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect params type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s = f->params()[0]
                            .value.type()
                            ->as<type::Pointer>()
                            .pointee()
                            ->as<type::Struct>();

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
  if (error) { visitor->data().set_qual_type(decl, type::QualType::Error()); }

  return visitor->data().set_qual_type(
      decl,
      type::QualType(decl_type, (decl->flags() & ast::Declaration::f_IsConst)
                                    ? type::Quals::Const()
                                    : type::Quals::Unqualified()));
}

}  // namespace

// TODO set qualifiers correctly here.
type::QualType Compiler::VerifyType(ast::Declaration const *node) {
  type::QualType node_qual_type;
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      auto type_expr_result = VerifyType(node->type_expr());
      if (not type_expr_result) {
        return data().set_qual_type(node, type::QualType::Error());
      }
      auto *type_expr_type = type_expr_result.type();

      if (type_expr_type == type::Type_) {
        auto maybe_type = EvaluateAs<type::Type const *>(node->type_expr());
        if (not maybe_type) { NOT_YET(); }
        auto const *t = ASSERT_NOT_NULL(*maybe_type);

        node_qual_type = data().set_qual_type(
            node,
            type::QualType(t, (node->flags() & ast::Declaration::f_IsConst)
                                  ? type::Quals::Const()
                                  : type::Quals::Unqualified()));

        if (not(node->flags() & ast::Declaration::f_IsFnParam) and
            not node_qual_type.type()->IsDefaultInitializable()) {
          diag().Consume(diagnostic::NoDefaultValue{
              .type  = node_qual_type.type(),
              .range = node->range(),
          });
        }

      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->type_expr()->range(),
            .type  = type_expr_type,
        });
        return data().set_qual_type(node, type::QualType::Error());
      }
    } break;
    case ast::Declaration::kInferred: {
      DEBUG_LOG("Declaration")("Verifying, ", node->id());

      ASSIGN_OR(return data().set_qual_type(node, type::QualType::Error()),
                       auto init_val_result, VerifyType(node->init_val()));

      auto reason = Inferrable(init_val_result.type());
      if (reason != diagnostic::UninferrableType::Reason::kInferrable) {
        diag().Consume(diagnostic::UninferrableType{
            .reason = reason,
            .range  = node->init_val()->range(),
        });
        return data().set_qual_type(node, type::QualType::Error());
      }

      if (not VerifyInitialization(diag(), node->range(), init_val_result,
                                   init_val_result)) {
        return data().set_qual_type(node, type::QualType::Error());
      }

      node_qual_type = data().set_qual_type(
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
          .range  = node->init_val()->range(),
      });
      if (node->flags() & ast::Declaration::f_IsConst) {
        diag().Consume(diagnostic::UninitializedConstant{
            .range = node->range(),
        });
      }
      return data().set_qual_type(node, type::QualType::Error());
    } break;
    case ast::Declaration::kCustomInit: {
      auto init_val_qual_type = VerifyType(node->init_val());
      bool error              = not init_val_qual_type.ok();
      auto type_expr_result   = VerifyType(node->type_expr());
      auto *type_expr_type    = type_expr_result.type();

      if (type_expr_type == nullptr) {
        error = true;
      } else if (type_expr_type == type::Type_) {
        if (not type_expr_result.constant()) {
          NOT_YET("log an error");
          error = true;
        } else {
          auto maybe_type = EvaluateAs<type::Type const *>(node->type_expr());
          if (not maybe_type) { NOT_YET(); }
          auto const *t = ASSERT_NOT_NULL(*maybe_type);

          node_qual_type =
              data().set_qual_type(node, type::QualType::Constant(t));
        }

        if (node_qual_type and init_val_qual_type) {
          error |= not VerifyInitialization(diag(), node->range(),
                                            node_qual_type, init_val_qual_type);
        }
      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->type_expr()->range(),
            .type  = type_expr_type,
        });
        error = true;
      }

      if (error) { return data().set_qual_type(node, type::QualType::Error()); }
    } break;
    case ast::Declaration::kUninitialized: {
      ASSIGN_OR(return data().set_qual_type(node, type::QualType::Error()),
                       auto type_expr_result, VerifyType(node->type_expr()));
      auto *type_expr_type = type_expr_result.type();
      if (type_expr_type == type::Type_) {
        if (not type_expr_result.constant()) {
          NOT_YET("log an error");
          return data().set_qual_type(node, type::QualType::Error());
        }
        auto maybe_type = EvaluateAs<type::Type const *>(node->type_expr());
        if (not maybe_type) { NOT_YET(); }
        auto const *t = ASSERT_NOT_NULL(*maybe_type);

        node_qual_type =
            data().set_qual_type(node, type::QualType::Constant(t));
      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->type_expr()->range(),
            .type  = type_expr_type,
        });
        return data().set_qual_type(node, type::QualType::Error());
      }

      if (node->flags() & ast::Declaration::f_IsConst) {
        diag().Consume(diagnostic::UninitializedConstant{
            .range = node->range(),
        });
        return data().set_qual_type(node, type::QualType::Error());
      }

    } break;
    default: UNREACHABLE(node->DebugString());
  }

  if (node->id().empty()) {
    if (node_qual_type.type() == type::Module) {
      // TODO check shadowing against other modules?
      // TODO what if no init val is provded? what if not constant?
      auto maybe_mod = EvaluateAs<module::BasicModule *>(node->init_val());
      if (not maybe_mod) { NOT_YET(); }
      node->scope()->embedded_modules_.insert(*maybe_mod);
      return data().set_qual_type(node, type::QualType::Constant(type::Module));
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
          .range1 = node->range(),
          .range2 = (*typed_decl)->range(),
      });
    }
  }

  if (failed_shadowing) {
    // TODO node may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else
    // on a different branch could find it unambiguously. It's also just a
    // hack from the get-go so maybe we should just do it the right way.
    return data().set_qual_type(node, type::QualType::Error());
  }

  return VerifySpecialFunctions(this, node, node_qual_type.type());
}

}  // namespace compiler
