#include "compiler/dispatch/extract_params.h"

#include "ast/ast.h"
#include "backend/eval.h"
#include "ir/any_func.h"
#include "ir/compiled_fn.h"
#include "type/function.h"

namespace compiler {
namespace {
core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, ast::Declaration const *decl) {
  auto *decl_type = compiler->type_of(decl);
  if (decl->flags() & ast::Declaration::f_IsConst) {
    if (auto const *fn_type = decl_type->if_as<type::Function>()) {
      auto f = backend::EvaluateAs<ir::AnyFunc>(
          compiler->MakeThunk(decl, decl_type));
      return f.is_fn() ? f.func()->params() : fn_type->AnonymousFnParams();
    } else {
      NOT_YET();
    }
  } else {
    if (auto const *fn_type = decl_type->if_as<type::Function>()) {
      return fn_type->AnonymousFnParams();
    } else {
      NOT_YET();
    }
  }
}

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, ast::FunctionLiteral const * fn_lit) {
  return fn_lit->params().Transform([compiler](auto const &expr) {
    return type::Typed<ast::Declaration const *>(expr.get(),
                                                 compiler->type_of(expr.get()));
  });
}
}  // namespace

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, ast::Expression const *expr) {
  if (auto const *decl = expr->if_as<ast::Declaration>()) {
    return ExtractParams(compiler, decl);
  } else if (auto const *fn_lit = expr->if_as<ast::FunctionLiteral>()) {
    return ExtractParams(compiler, fn_lit);
  } else {
    NOT_YET();
  }
}

}  // namespace compiler
