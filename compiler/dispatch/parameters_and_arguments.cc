#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "interpretter/evaluate.h"
#include "ir/value/fn.h"
#include "type/function.h"

namespace compiler {
namespace {
core::Params<type::QualType> ExtractParams(Compiler *compiler,
                                           ast::Declaration const *decl) {
  auto *decl_type = ASSERT_NOT_NULL(compiler->type_of(decl));
  if (decl->flags() & ast::Declaration::f_IsConst) {
    if (auto const *fn_type = decl_type->if_as<type::Function>()) {
      auto f = interpretter::EvaluateAs<ir::Fn>(
          compiler->MakeThunk(decl, decl_type));

      switch (f.kind()) {
        case ir::Fn::Kind::Native:
          return f.native()->params().Transform([](auto const &p) {
            return type::QualType::NonConstant(p.type());
          });
        case ir::Fn::Kind::Builtin:
          return fn_type->params().Transform(
              [](auto const &p) { return type::QualType::NonConstant(p); });
        case ir::Fn::Kind::Foreign:
          return fn_type->params().Transform(
              [](auto const &p) { return type::QualType::NonConstant(p); });
      }
    } else if (auto *jump_type = decl_type->if_as<type::Jump>()) {
      auto j = interpretter::EvaluateAs<ir::Jump const *>(
          compiler->MakeThunk(decl, decl_type));
      return j->params().Transform(
          [](auto const &p) { return type::QualType::NonConstant(p.type()); });
    } else if (decl_type == type::Generic) {
        // TODO determine how to evaluate this with an interpretter.
        if (auto *fn_lit = decl->init_val()->if_as<ast::FunctionLiteral>()) {
          return fn_lit->params().Transform([&](auto const &p) {
            type::Type const *t = interpretter::EvaluateAs<type::Type const *>(
                compiler->MakeThunk(p->type_expr(), type::Type_));
            return type::QualType::NonConstant(t);
          });
        } else {
          NOT_YET(decl->init_val()->DebugString());
        }
    } else {
      UNREACHABLE(decl->DebugString(), decl_type->to_string());
    }
  } else {
    if (auto const *fn_type = decl_type->if_as<type::Function>()) {
      return fn_type->params().Transform(
          [](type::Type const *t) { return type::QualType::NonConstant(t); });
    } else {
      NOT_YET(decl->DebugString());
    }
  }
}

core::Params<type::QualType> ExtractParams(Compiler *compiler,
                                           ast::FunctionLiteral const *fn_lit) {
  return fn_lit->params().Transform([compiler](auto const &expr) {
    auto qt = compiler->qual_type_of(expr.get());
    ASSERT(qt.has_value() == true);
    return *qt;
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

ir::Results PrepareOneArg(Compiler *c, type::Typed<ir::Results> const &arg,
                          type::Type const *param_type) {
  auto &bldr      = c->builder();
  auto *arg_var   = arg.type()->if_as<type::Variant>();
  auto *param_var = param_type->if_as<type::Variant>();
  if (arg_var and param_var) {
    NOT_YET();
  } else if (arg_var) {
    return ir::Results{bldr.PtrFix(
        bldr.VariantValue(nullptr, arg->get<ir::Addr>(0)), param_type)};
  } else if (param_var) {
    auto tmp = bldr.TmpAlloca(param_var);
    static_cast<void>(tmp);
    // TODO type::ApplyTypes<>
    // ir::Store(arg_var , tmp);
    NOT_YET(tmp);
  } else {
    // TODO other implicit conversions?
    auto *t = arg.type();
    if (t->is_big()) {
      auto r = bldr.TmpAlloca(t);
      c->EmitMoveInit(t, arg.get(), type::Typed(r, type::Ptr(t)));
      return ir::Results{r};
    } else {
      return arg.get();
    }
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

core::Params<type::QualType> ExtractParams(Compiler *compiler,
                                           ast::Expression const *expr) {
  if (auto const *decl = expr->if_as<ast::Declaration>()) {
    return ExtractParams(compiler, decl);
  } else if (auto const *fn_lit = expr->if_as<ast::FunctionLiteral>()) {
    return ExtractParams(compiler, fn_lit);
  } else {
    NOT_YET();
  }
}

std::vector<ir::Results> PrepareCallArguments(
    Compiler *compiler, type::Type const *state_ptr_type,
    core::Params<type::Type const *> const &params,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  std::vector<ir::Results> arg_results;
  arg_results.reserve(params.size());

  auto &bldr = compiler->builder();
  size_t i   = 0;
  size_t j   = 0;
  if (state_ptr_type) {
    arg_results.push_back(PrepareOneArg(compiler, args[i++], state_ptr_type));
  }
  while (i < args.pos().size()) {
    arg_results.push_back(PrepareOneArg(compiler, args[i++], params[j++].value));
  }

  for (; i < params.size(); ++i) {
    arg_results.push_back(
        PrepareOneArg(compiler, args[params[i].name], params[i].value));
  }

  return arg_results;
}

}  // namespace compiler
