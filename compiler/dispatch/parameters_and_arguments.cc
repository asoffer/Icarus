#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "interpretter/evaluate.h"
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
      auto f = interpretter::EvaluateAs<ir::AnyFunc>(
          compiler->MakeThunk(decl, decl_type));
      return f.is_fn() ? f.func()->params() : fn_type->AnonymousFnParams();
    } else {
      NOT_YET(decl->DebugString());
    }
  } else {
    if (auto const *fn_type = decl_type->if_as<type::Function>()) {
      return fn_type->AnonymousFnParams();
    } else {
      NOT_YET(decl->DebugString());
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

ir::Results PrepareOneArg(ir::Builder &bldr,
                          type::Typed<ir::Results> const &arg,
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
    static_cast<void>(tmp);
    // TODO type::ApplyTypes<>
    // ir::Store(arg_var , tmp);
    NOT_YET(tmp);
  } else {
    // TODO other implicit conversions?
    return arg.get();
  }
}

}  // namespace

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<type::QualType> const &fnargs) {
  std::vector<core::FnArgs<type::Type const *>> all_expanded_options(1);
  fnargs.ApplyWithIndex([&](auto &&index, type::QualType r) {
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

std::vector<ir::Results> PrepareCallArguments(
    Compiler *compiler,
    core::FnParams<type::Typed<ast::Declaration const *>> const &params,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  std::vector<ir::Results> arg_results;
  arg_results.reserve(params.size());

  auto &bldr = compiler->builder();
  size_t i   = 0;
  for (; i < args.pos().size(); ++i) {
    arg_results.push_back(
        PrepareOneArg(bldr, args.pos()[i], params.at(i).value.type()));
  }

  for (; i < params.size(); ++i) {
    auto const &param = params.at(i);
    if (auto *arg = args.at_or_null(param.name)) {
      arg_results.push_back(
          PrepareOneArg(bldr, *arg, params.at(i).value.type()));
    } else {
      arg_results.push_back(ir::Results{compiler->Visit(
          ASSERT_NOT_NULL(param.value.get()->init_val()), EmitValueTag{})});
    }
  }
  return arg_results;
}

}  // namespace compiler
