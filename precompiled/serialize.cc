#include "precompiled/serialize.h"

#include "base/macros.h"
#include "ir/value/char.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/legacy_generic.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"
#include "type/variable.h"

namespace precompiled {
namespace {

struct ValueSerializer {
  using signature = void(ir::CompleteResultRef);

  explicit ValueSerializer(type::TypeSystem const* system, Value* value)
      : system_(*ASSERT_NOT_NULL(system)), value_(*ASSERT_NOT_NULL(value)) {}

  void operator()(type::Type t, ir::CompleteResultRef ref) {
    t.visit<ValueSerializer>(*this, ref);
  }

  void operator()(auto const* t, ir::CompleteResultRef ref) {
    NOT_YET(t->to_string());
  }

  void operator()(type::Function const* t, ir::CompleteResultRef ref) {
    ir::Fn fn        = ref.get<ir::Fn>();
    auto& proto_func = *value_.mutable_function();
    proto_func.set_module_id(fn.module().value());
    proto_func.set_function_id(fn.local().value());
  }

  void operator()(type::Primitive const* p, ir::CompleteResultRef ref) {
    switch (p->kind()) {
      case type::Primitive::Kind::Bool:
        value_.set_boolean(ref.get<bool>());
        break;
      case type::Primitive::Kind::Char:
        value_.set_unsigned_integer(ref.get<ir::Char>().as_type<uint8_t>());
        break;
      case type::Primitive::Kind::I8:
        value_.set_signed_integer(ref.get<int8_t>());
        break;
      case type::Primitive::Kind::I16:
        value_.set_signed_integer(ref.get<int16_t>());
        break;
      case type::Primitive::Kind::I32:
        value_.set_signed_integer(ref.get<int32_t>());
        break;
      case type::Primitive::Kind::I64:
        value_.set_signed_integer(ref.get<int64_t>());
        break;
      case type::Primitive::Kind::U8:
        value_.set_unsigned_integer(ref.get<uint8_t>());
        break;
      case type::Primitive::Kind::U16:
        value_.set_unsigned_integer(ref.get<uint16_t>());
        break;
      case type::Primitive::Kind::U32:
        value_.set_unsigned_integer(ref.get<uint32_t>());
        break;
      case type::Primitive::Kind::U64:
        value_.set_unsigned_integer(ref.get<uint64_t>());
        break;
      case type::Primitive::Kind::F32: value_.set_real(ref.get<float>()); break;
      case type::Primitive::Kind::F64:
        value_.set_real(ref.get<double>());
        break;
      case type::Primitive::Kind::Byte:
        value_.set_unsigned_integer(static_cast<uint8_t>(ref.get<std::byte>()));
        break;
      case type::Primitive::Kind::Type_: {
        size_t index = system_.index(ref.get<type::Type>());
        ASSERT(index != std::numeric_limits<size_t>::max());
        value_.set_type(index);
      } break;
      default: NOT_YET((int)p->kind());
    }
  }

 private:
  type::TypeSystem const& system_;
  Value& value_;
};

struct ValueDeserializer {
  using signature = void(ir::CompleteResultBuffer& buffer);

  explicit ValueDeserializer(
      module::ModuleTable const* module_table,
      google::protobuf::Map<uint32_t, std::string> const* module_map,
      type::TypeSystem const* system, Value const* value)
      : module_table_(*ASSERT_NOT_NULL(module_table)),
        module_map_(*ASSERT_NOT_NULL(module_map)),
        system_(*ASSERT_NOT_NULL(system)),
        value_(*ASSERT_NOT_NULL(value)) {}

  void operator()(type::Type t, ir::CompleteResultBuffer& buffer) {
    return t.visit<ValueDeserializer>(*this, buffer);
  }

  void operator()(auto const* t, ir::CompleteResultBuffer& buffer) {
    NOT_YET();
  }

  void operator()(type::Function const*, ir::CompleteResultBuffer& buffer) {
    auto const& fn = value_.function();
    auto iter      = module_map_.find(fn.module_id());
    ASSERT(iter != module_map_.end());
    auto [id, m] = module_table_.module(iter->second);
    ASSERT(m != nullptr);
    ASSERT(id != ir::ModuleId::Invalid());
    buffer.append(ir::Fn(id, ir::LocalFnId(fn.function_id())));
  }

  void operator()(type::Primitive const* p, ir::CompleteResultBuffer& buffer) {
    switch (p->kind()) {
      case type::Primitive::Kind::Bool: buffer.append(value_.boolean()); break;
      case type::Primitive::Kind::Char: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint8_t>::max());
        buffer.append(
            ir::Char(static_cast<uint8_t>(value_.unsigned_integer())));
      } break;
      case type::Primitive::Kind::I8: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int8_t>::max());
        buffer.append(static_cast<int8_t>(value_.signed_integer()));
      } break;
      case type::Primitive::Kind::I16: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int16_t>::max());
        buffer.append(static_cast<int16_t>(value_.signed_integer()));
      } break;
      case type::Primitive::Kind::I32: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int32_t>::max());
        buffer.append(static_cast<int32_t>(value_.signed_integer()));
      } break;
      case type::Primitive::Kind::I64: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int64_t>::max());
        buffer.append(static_cast<int64_t>(value_.signed_integer()));
      } break;
      case type::Primitive::Kind::U8: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint8_t>::max());
        buffer.append(static_cast<uint8_t>(value_.unsigned_integer()));
      } break;
      case type::Primitive::Kind::U16: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint16_t>::max());
        buffer.append(static_cast<uint16_t>(value_.unsigned_integer()));
      } break;
      case type::Primitive::Kind::U32: {
        ASSERT(value_.unsigned_integer() >= 0);
        buffer.append(static_cast<uint32_t>(value_.unsigned_integer()));
      } break;
      case type::Primitive::Kind::U64: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint64_t>::max());
        buffer.append(static_cast<uint64_t>(value_.unsigned_integer()));
      } break;
      case type::Primitive::Kind::F32: {
        buffer.append(static_cast<float>(value_.real()));
      } break;
      case type::Primitive::Kind::F64: {
        buffer.append(static_cast<double>(value_.real()));
      } break;
      case type::Primitive::Kind::Byte: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint8_t>::max());
        buffer.append(
            std::byte(static_cast<uint8_t>(value_.unsigned_integer())));
      } break;
      case type::Primitive::Kind::Type_: {
        buffer.append(system_.from_index(value_.type()));
      } break;
      default: NOT_YET();
    }
  }

 private:
  module::ModuleTable const& module_table_;
  google::protobuf::Map<uint32_t, std::string> const& module_map_;
  type::TypeSystem const& system_;
  Value const& value_;
};

struct TypeSystemSerializingVisitor {
  using signature = void(TypeDefinition& out);

  explicit TypeSystemSerializingVisitor(type::TypeSystem const* system)
      : system_(*ASSERT_NOT_NULL(system)) {}

  void operator()(type::Type t, TypeDefinition& out) { t.visit(*this, out); }

  void operator()(auto const* t, TypeDefinition& out) { Visit(t, out); }

 private:
  void Visit(type::Primitive const* p, TypeDefinition& out) {
    out.set_primitive(static_cast<int>(p->kind()));
  }

  void Visit(type::Pointer const* p, TypeDefinition& out) {
    out.set_pointer(system_.index(p->pointee()));
  }
  void Visit(type::BufferPointer const* p, TypeDefinition& out) {
    out.set_buffer_pointer(system_.index(p->pointee()));
  }
  void Visit(type::Function const* f, TypeDefinition& out) {
    auto& fn = *out.mutable_function();
    fn.set_eager(f->eager());
    for (auto const& param : f->parameters()) {
      auto& p = *fn.add_parameter();
      p.set_name(param.name);
      p.set_type(system_.index(param.value.type()));
      p.set_flags((param.flags.value() << uint8_t{8}) |
                  param.value.quals().value());
    }
    for (type::Type t : f->return_types()) {
      fn.add_return_type(system_.index(t));
    }
  }
  void Visit(type::Slice const* s, TypeDefinition& out) {
    out.set_slice(system_.index(s->data_type()));
  }

  void Visit(type::Opaque const* o, TypeDefinition& out) {
    auto& opaque = *out.mutable_opaque();
    opaque.set_module_id(o->defining_module().value());
    opaque.set_numeric_id(o->numeric_id());
  }

  void Visit(type::Enum const* e, TypeDefinition& out) {
    auto value_range = e->values();
    auto& t          = *out.mutable_enum_type();
    t.set_module_id(e->defining_module().value());
    t.mutable_values()->insert(value_range.begin(), value_range.end());
  }

  void Visit(type::Flags const* f, TypeDefinition& out) {
    auto value_range = f->values();
    auto& t          = *out.mutable_flags_type();
    t.set_module_id(f->defining_module().value());
    t.mutable_values()->insert(value_range.begin(), value_range.end());
  }

  void Visit(auto const* s, TypeDefinition& out) { NOT_YET(s->to_string()); }

  type::TypeSystem const& system_;
};

}  // namespace

void SerializeValue(type::TypeSystem const& system, type::Type t,
                    ir::CompleteResultRef ref, Value& value) {
  value.set_type_id(system.index(t));
  ValueSerializer vs(&system, &value);
  vs(t, ref);
}

ir::CompleteResultBuffer DeserializeValue(
    module::ModuleTable const& module_table,
    google::protobuf::Map<uint32_t, std::string> const& module_map,
    type::TypeSystem const& system, Value const& value) {
  ir::CompleteResultBuffer result;
  ValueDeserializer vd(&module_table, &module_map, &system, &value);
  vd(system.from_index(value.type_id()), result);
  return result;
}

SymbolInformation ToProto(type::TypeSystem const& system,
                          module::Module::SymbolInformation const& info) {
  SymbolInformation s;
  s.set_qualifiers(info.qualified_type.quals().value());
  s.set_visible(info.visibility == module::Module::Visibility::Exported);
  SerializeValue(type::GlobalTypeSystem, info.qualified_type.type(),
                 info.value[0], *s.mutable_value());
  return s;
}

TypeSystem ToProto(type::TypeSystem const& system) {
  TypeSystem proto;
  TypeSystemSerializingVisitor v(&system);
  for (type::Type t : system.types()) { v(t, *proto.add_type()); }
  return proto;
}

void FromProto(TypeSystem const& proto, type::TypeSystem& system) {
  for (auto const& t : proto.type()) {
    switch (t.type_case()) {
      case TypeDefinition::kPrimitive:
        system.insert(
            MakePrimitive(static_cast<type::Primitive::Kind>(t.primitive())));
        break;
      case TypeDefinition::kPointer:
        system.insert(Ptr(system.from_index(t.pointer())));
        break;
      case TypeDefinition::kBufferPointer:
        system.insert(BufPtr(system.from_index(t.buffer_pointer())));
        break;
      case TypeDefinition::kFunction: {
        core::Parameters<type::QualType> parameters;
        for (auto const& p : t.function().parameter()) {
          parameters.append(
              p.name(),
              type::QualType(system.from_index(p.type()),
                             type::Qualifiers::FromValue(p.flags() & 0xff)),
              core::ParameterFlags::FromValue(p.flags() >> uint8_t{8}));
        }
        std::vector<type::Type> return_types;
        return_types.reserve(t.function().return_type().size());
        for (int64_t n : t.function().return_type()) {
          return_types.push_back(system.from_index(n));
        }

        auto* make_func = (t.function().eager() ? type::EagerFunc : type::Func);
        system.insert(
            make_func(std::move(parameters), std::move(return_types)));
      } break;
      case TypeDefinition::kSlice:
        system.insert(Slc(system.from_index(t.slice())));
        break;
      case TypeDefinition::kOpaque: NOT_YET(); break;
      case TypeDefinition::kEnumType: {
        auto const& proto_enum = t.enum_type();
        auto* e =
            type::Allocate<type::Enum>(ir::ModuleId(proto_enum.module_id()));
        auto [index, inserted] = system.insert(e);
        ASSERT(inserted == true);
        absl::flat_hash_map<std::string, type::Enum::underlying_type> members(
            proto_enum.values().begin(), proto_enum.values().end());
        e->SetMembers(std::move(members));
        e->complete();
      } break;
      case TypeDefinition::kFlagsType: {
        auto const& proto_flags = t.flags_type();
        auto* f =
            type::Allocate<type::Flags>(ir::ModuleId(proto_flags.module_id()));
        auto [index, inserted] = system.insert(f);
        ASSERT(inserted == true);
        absl::flat_hash_map<std::string, type::Flags::underlying_type> members(
            proto_flags.values().begin(), proto_flags.values().end());
        f->SetMembers(std::move(members));
        f->complete();
      } break;
      case TypeDefinition::TYPE_NOT_SET: UNREACHABLE();
    }
  }
}

}  // namespace precompiled
