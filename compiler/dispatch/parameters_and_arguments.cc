#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "base/debug.h"
#include "ir/interpretter/evaluate.h"
#include "ir/value/fn.h"
#include "type/function.h"
#include "type/generic_function.h"

namespace compiler {
namespace {

template <typename IndexT>
void AddType(IndexT &&index, type::Type t,
             std::vector<core::FnArgs<type::Type>> *args) {
  std::for_each(args->begin(), args->end(),
                [&](core::FnArgs<type::Type> &fnargs) {
                  if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
                    fnargs.pos_emplace(t);
                  } else {
                    fnargs.named_emplace(index, t);
                  }
                });
}

ir::Value PrepareOneArg(Compiler *c, type::Typed<ir::Value> const &arg,
                        type::Type param_type) {
  auto &bldr = c->builder();
  // TODO other implicit conversions?
  auto t = arg.type();
  if (t->is_big()) {
    auto r = bldr.TmpAlloca(t);
    c->EmitMoveInit(arg, type::Typed<ir::Reg>(r, type::Ptr(t)));
    return ir::Value(r);
  } else {
    return arg.get();
  }
}

}  // namespace

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::FnArgs<type::Type>> ExpandedFnArgs(
    core::FnArgs<type::QualType> const &fnargs) {
  std::vector<core::FnArgs<type::Type>> all_expanded_options(1);
  fnargs.ApplyWithIndex([&](auto &&index, type::QualType r) {
    // TODO also maybe need the expression this came from to see if it needs
    // to be expanded.
    AddType(index, r.type(), &all_expanded_options);
  });

  return all_expanded_options;
}

std::vector<ir::Value> PrepareCallArguments(
    Compiler *compiler, type::Type state_ptr_type,
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
