#include "compiler/dispatch/extract_params.h"

#include "ast/ast.h"
#include "backend/eval.h"
#include "ir/any_func.h"
#include "ir/compiled_fn.h"
#include "type/function.h"

namespace compiler {
namespace {
core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, type::Typed<ast::Declaration const *> decl) {
  if (decl.get()->flags() & ast::Declaration::f_IsConst) {
    if (auto const *fn_type = decl.type()->if_as<type::Function>()) {
      auto f = backend::EvaluateAs<ir::AnyFunc>(decl, compiler);
      return f.is_fn() ? f.func()->params() : fn_type->AnonymousFnParams();
    } else {
      NOT_YET();
    }
  } else {
    if (auto const *fn_type = decl.type()->if_as<type::Function>()) {
      return fn_type->AnonymousFnParams();
    } else {
      NOT_YET();
    }
  }
}

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, type::Typed<ast::FunctionLiteral const *> fn_lit) {
  return fn_lit.get()->params().Transform([compiler](auto const &expr) {
    return type::Typed<ast::Declaration const *>(expr.get(),
                                                 compiler->type_of(expr.get()));
  });
}
}  // namespace

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, type::Typed<ast::Expression const *> expr) {
  if (auto const *decl = expr.get()->if_as<ast::Declaration>()) {
    return ExtractParams(
        compiler, type::Typed<ast::Declaration const *>(decl, expr.type()));
  } else if (auto const *fn_lit = expr.get()->if_as<ast::FunctionLiteral>()) {
    return ExtractParams(compiler, type::Typed<ast::FunctionLiteral const *>(
                                       fn_lit, expr.type()));
  } else {
    NOT_YET();
  }
}

}  // namespace compiler
