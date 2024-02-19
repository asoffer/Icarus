#include "type/type_system.h"

#include "nth/debug/debug.h"

namespace ic::type {

Type Reindex(Type t, TypeSystem const& from, TypeSystem& to) {
  switch (t.kind()) {
    case Type::Kind::Primitive: return t;
    case Type::Kind::Pointer:
      return to.pointer_type(from.pointee_types.from_index(t.index()));
    case Type::Kind::BufferPointer:
      return to.buffer_pointer_type(
          from.buffer_pointee_types.from_index(t.index()));
    case Type::Kind::Slice:
      return to.slice_type(from.slice_element_types.from_index(t.index()));
    case Type::Kind::Parameters: {
      auto const& params = from.parameters.from_index(t.index());
      std::vector<ParametersType::Parameter> ps;
      ps.reserve(params.size());
      // TODO: Reparent names.
      for (auto const& param : params) {
        auto& [name, type] = ps.emplace_back() = param;
        type                                   = Reindex(type, from, to);
      }
      return to.parameter_type(std::move(ps));
    } break;
    case Type::Kind::Function: {
      auto const& [param_type, ret_index, eval] =
          from.functions.from_index(t.index());
      auto p                 = Reindex(param_type, from, to).AsParameters();
      std::vector<Type> rets = from.returns.from_index(ret_index);
      for (auto& ret : rets) { ret = Reindex(ret, from, to); }
      return to.function(p, std::move(rets), eval);
    }
    case Type::Kind::Opaque:
      return t;  // TODO: This doesn't actually work, but type systems don't
                 // support the required functionality to make it work yet. The
                 // problem is you could merge two opaque types with the same
                 // index from different modules, but we don't have any
                 // mechanism yet to distinguish them anyway.
    default: NTH_UNIMPLEMENTED("{}") <<= {t.kind()};
  }
}

PointerType TypeSystem::pointer_type(Type t) {
  return PointerType(pointee_types.index(pointee_types.insert(t).first));
}

BufferPointerType TypeSystem::buffer_pointer_type(Type t) {
  return BufferPointerType(
      buffer_pointee_types.index(buffer_pointee_types.insert(t).first));
}

SliceType TypeSystem::slice_type(Type t) {
  return SliceType(
      slice_element_types.index(slice_element_types.insert(t).first));
}

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

void TypeSystem::merge_from(TypeSystem const& ts) {
  for (Type t : ts.pointee_types) { pointer_type(Reindex(t, ts, *this)); }
  for (Type t : ts.buffer_pointee_types) {
    buffer_pointer_type(Reindex(t, ts, *this));
  }
  for (Type t : ts.slice_element_types) { slice_type(Reindex(t, ts, *this)); }
  absl::flat_hash_map<std::vector<Type>, uint64_t> reindexed_rets;
  for (std::vector<Type> const& rets : ts.returns) {
    std::vector<Type> v;
    v.reserve(rets.size());
    for (Type t : rets) { v.push_back(Reindex(t, ts, *this)); }
    auto iter = returns.insert(v).first;
    reindexed_rets.emplace(std::move(v), returns.index(iter));
  }
  for (auto const& [p, n, e] : ts.functions) {
    auto iter = reindexed_rets.find(ts.returns.from_index(n));
    NTH_REQUIRE((v.debug), iter != reindexed_rets.end());
    functions.insert(
        std::make_tuple(Reindex(p, ts, *this).AsParameters(), iter->second, e));
  }
}

}  // namespace ic::type
