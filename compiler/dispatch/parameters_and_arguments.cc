#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "base/debug.h"
#include "interpretter/evaluate.h"
#include "ir/value/fn.h"
#include "type/function.h"
#include "type/generic_function.h"

namespace compiler {
namespace {

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

ir::Value PrepareOneArg(Compiler *c, type::Typed<ir::Value> const &arg,
                        type::Type const *param_type) {
  auto &bldr      = c->builder();
  auto *arg_var   = arg.type()->if_as<type::Variant>();
  auto *param_var = param_type->if_as<type::Variant>();
  if (arg_var and param_var) {
    NOT_YET();
  } else if (arg_var) {
    return ir::Value(bldr.PtrFix(
        bldr.VariantValue(nullptr, arg->get<ir::Addr>()), param_type));
  } else if (param_var) {
    auto tmp = bldr.TmpAlloca(param_var);
    static_cast<void>(tmp);
    // TODO type::ApplyTypes<>
    // builder().Store(arg_var , tmp);
    NOT_YET(tmp);
  } else {
    // TODO other implicit conversions?
    auto *t = arg.type();
    if (t->is_big()) {
      auto r = bldr.TmpAlloca(t);
      c->EmitMoveInit(arg, type::Typed(r, type::Ptr(t)));
      return ir::Value(r);
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

std::vector<ir::Value> PrepareCallArguments(
    Compiler *compiler, type::Type const *state_ptr_type,
    core::Params<type::QualType> const &params,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  std::vector<ir::Value> arg_values;
  arg_values.reserve(params.size());

  auto &bldr = compiler->builder();
  size_t i   = 0;
  size_t j   = 0;
  if (state_ptr_type) {
    arg_values.push_back(PrepareOneArg(compiler, args[i++], state_ptr_type));
  }
  while (i < args.pos().size()) {
    arg_values.push_back(
        PrepareOneArg(compiler, args[i++], params[j++].value.type()));
  }

  for (; i < params.size(); ++i) {
    arg_values.push_back(
        PrepareOneArg(compiler, args[params[i].name], params[i].value.type()));
  }

  return arg_values;
}

}  // namespace compiler
