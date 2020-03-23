#include "compiler/dispatch/runtime.h"

namespace compiler {

ir::RegOr<bool> EmitRuntimeDispatchOneComparison(
    ir::Builder &bldr, core::Params<type::Type const *> const &params,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  size_t i = 0;
  for (; i < args.pos().size(); ++i) {
    auto &arg     = args.pos()[i];
    auto *arg_var = arg.type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    auto runtime_type =
        bldr.Load<type::Type const *>(bldr.VariantType(arg->get<ir::Addr>(0)));
    // TODO Equality isn't the right thing to check
    return bldr.Eq(runtime_type, params[i].value);
  }
  for (; i < params.size(); ++i) {
    auto *arg = args.at_or_null(params[i].name);
    if (not arg) { continue; }  // Default arguments
    auto *arg_var = arg->type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    NOT_YET();
  }
  return ir::RegOr<bool>(false);
}

}  // namespace compiler
