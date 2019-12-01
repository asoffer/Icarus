#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "backend/eval.h"
#include "ir/any_func.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "type/function.h"

namespace compiler {
namespace {
core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, ast::Declaration const *decl) {
  auto *decl_type = ASSERT_NOT_NULL(compiler->type_of(decl));
  if (decl->flags() & ast::Declaration::f_IsConst) {
    if (auto const *fn_type = decl_type->if_as<type::Function>()) {
      auto f = backend::EvaluateAs<ir::AnyFunc>(
          compiler->MakeThunk(decl, decl_type));
      return f.is_fn() ? f.func()->params() : fn_type->AnonymousFnParams();
    } else {
      NOT_YET(decl->DebugString());
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
    Compiler *compiler, ast::FunctionLiteral const *fn_lit) {
  return fn_lit->params().Transform([compiler](auto const &expr) {
    return type::Typed<ast::Declaration const *>(expr.get(),
                                                 compiler->type_of(expr.get()));
  });
}

template <typename IndexT>
void AddType(IndexT &&index, type::Type const *t,
             std::vector<core::FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    std::vector<core::FnArgs<type::Type const *>> new_args;
    for (auto *v : vt->variants_) {
      for (auto fnargs : *args) {
        if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
          fnargs.pos_emplace(v);
        } else {
          fnargs.named_emplace(index, v);
        }
        new_args.push_back(std::move(fnargs));
      }
    }
    *args = std::move(new_args);
  } else {
    std::for_each(
        args->begin(), args->end(),
        [&](core::FnArgs<type::Type const *> &fnargs) {
          if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
            fnargs.pos_emplace(t);
          } else {
            fnargs.named_emplace(index, t);
          }
        });
  }
}

}  // namespace

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<VerifyResult> const &fnargs) {
  std::vector<core::FnArgs<type::Type const *>> all_expanded_options(1);
  fnargs.ApplyWithIndex([&](auto &&index, compiler::VerifyResult r) {
    // TODO also maybe need the expression this came from to see if it needs
    // to be expanded.
    AddType(index, r.type(), &all_expanded_options);
  });

  return all_expanded_options;
}

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

ir::Results PrepareArg(ir::Builder &bldr, type::Typed<ir::Results> const &arg,
                       type::Type const *param_type) {
  auto *arg_var   = arg.type()->if_as<type::Variant>();
  auto *param_var = param_type->if_as<type::Variant>();
  if (arg_var and param_var) {
    NOT_YET();
  } else if (arg_var) {
    return ir::Results{ir::PtrFix(
        bldr.VariantValue(nullptr, arg->get<ir::Addr>(0)), param_type)};
  } else if (param_var) {
    auto tmp = bldr.TmpAlloca(param_var);
    // TODO type::ApplyTypes<>
    // ir::Store(arg_var , tmp);
    NOT_YET(tmp);
  } else {
    // TODO other implicit conversions?
    return arg.get();
  }
}

}  // namespace compiler
