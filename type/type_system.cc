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

    default: NTH_UNIMPLEMENTED();
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

void TypeSystem::merge_from(TypeSystem const& ts) {
  for (Type t : ts.pointee_types) { pointer_type(Reindex(t, ts, *this)); }
  for (Type t : ts.buffer_pointee_types) {
    buffer_pointer_type(Reindex(t, ts, *this));
  }
  for (Type t : ts.slice_element_types) { slice_type(Reindex(t, ts, *this)); }
}

}  // namespace ic::type
