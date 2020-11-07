#include <vector>

#include "compiler/compiler.h"
#include "core/arguments.h"
#include "core/params.h"
#include "ir/value/value.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

ir::Value PrepareOneArg(Compiler &c, type::Typed<ir::Value> const &arg,
                        type::Type param_type) {
  auto &bldr = c.builder();
  // TODO: other implicit conversions?
  auto t = arg.type();
  if (t.get()->is_big()) {
    auto r = bldr.TmpAlloca(t);
    c.EmitMoveInit(arg, type::Typed<ir::Reg>(r, t));
    return ir::Value(r);
  } else {
    return arg.get();
  }
}

}  // namespace

std::vector<ir::Value> Compiler::PrepareCallArguments(
    type::Type state_ptr_type, core::Params<type::QualType> const &params,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  std::vector<ir::Value> arg_values;
  arg_values.reserve(params.size());

  size_t i = 0;
  size_t j = 0;
  if (state_ptr_type) {
    arg_values.push_back(PrepareOneArg(*this, args[i++], state_ptr_type));
  }
  while (i < args.pos().size()) {
    arg_values.push_back(
        PrepareOneArg(*this, args[i++], params[j++].value.type()));
  }

  for (; i < params.size(); ++i) {
    arg_values.push_back(
        PrepareOneArg(*this, args[params[i].name], params[i].value.type()));
  }

  return arg_values;
}

}  // namespace compiler
