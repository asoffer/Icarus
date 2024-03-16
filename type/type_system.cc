#include "type/type_system.h"

#include "common/identifier.h"
#include "nth/debug/debug.h"

namespace ic::type {

ParametersType TypeSystem::parameter_type(
    std::vector<ParametersType::Parameter>&& p) {
  return ParametersType(
      parameters.index(parameters.insert(std::move(p)).first));
}

ParametersType TypeSystem::parameter_type(
    std::vector<ParametersType::Parameter> const& p) {
  return ParametersType(parameters.index(parameters.insert(p).first));
}

FunctionType TypeSystem::function(ParametersType p, std::vector<Type> r,
                                  Evaluation e) {
  uint64_t rt = returns.index(returns.insert(std::move(r)).first);
  return FunctionType(functions.index(functions.insert({p, rt, e}).first));
}

Type TypeSystem::ReindexTable::operator()(Type t) const {
  if (t.kind() == Type::Kind::Primitive) { return t; }
  auto iter = mapping_.find(t);
  NTH_REQUIRE((v.debug), iter != mapping_.end());
  return iter->second;
}

}  // namespace ic::type
