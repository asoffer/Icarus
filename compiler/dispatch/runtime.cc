#include "compiler/dispatch/runtime.h"

namespace compiler {

ir::RegOr<bool> EmitRuntimeDispatchOneComparison(
    ir::Builder &bldr, core::Params<type::QualType> const &params,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  size_t i = 0;
  for (; i < args.pos().size(); ++i) {
    auto &arg     = args.pos()[i];
    auto *arg_var = arg.type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    auto runtime_type =
        bldr.Load<type::Type const *>(bldr.VariantType(arg->get<ir::Addr>()));
    // TODO Equality isn't the right thing to check
    return bldr.Eq(runtime_type, params[i].value.type());
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

void EmitRuntimeDispatch(
    DependentComputedData const &data, ir::Builder &bldr,
    ast::OverloadSet const &os,
    absl::flat_hash_map<ast::Expression const *, ir::BasicBlock *> const
        &callee_to_block,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  // TODO This is a simple linear search through the table which is certainly a
  // bad idea. We can optimize it later. Likely the right way to do this is to
  // find a perfect hash of the function variants that produces an index into a
  // block table so we pay for a hash and a single indirect jump. This may be
  // harder if you remove variant and implement `overlay`.

  auto iter = os.members().begin();

  while (true) {
    auto const *expr = *iter;
    ++iter;

    auto &block = callee_to_block.at(expr);
    if (iter == os.members().end()) {
      bldr.UncondJump(block);
      break;
    }

    core::Params<type::QualType> params =
        data.qual_type(expr)->type()->as<type::Function>().params();

    ir::RegOr<bool> match =
        EmitRuntimeDispatchOneComparison(bldr, params, args);
    bldr.CurrentBlock() = bldr.EarlyExitOn<true>(block, match);
  }
}

}  // namespace compiler
