#include "type/serialize.h"

#include "base/macros.h"
#include "ir/value/char.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/generic_function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace type {
namespace {

struct ValueSerializer {
  using signature = void(ir::CompleteResultRef);

  // TODO: This is a weird requirement to be a serializer since we're not using
  // it.
  void write_bytes(absl::Span<std::byte const> bytes) { UNREACHABLE(); }

  explicit ValueSerializer(TypeSystem const* system, std::string* out)
      : system_(*ASSERT_NOT_NULL(system)), out_(*ASSERT_NOT_NULL(out)) {}

  void operator()(Type t, ir::CompleteResultRef ref) {
    t.visit<ValueSerializer>(*this, ref);
  }

  void operator()(auto const* t, ir::CompleteResultRef ref) {
    NOT_YET(t->to_string());
  }

  void operator()(Function const* t, ir::CompleteResultRef ref) { NOT_YET(); }

  void operator()(Primitive const* p, ir::CompleteResultRef ref) {
    switch (p->kind()) {
      case Primitive::Kind::Bool: write(ref.get<bool>()); break;
      case Primitive::Kind::Char: write(ref.get<ir::Char>()); break;
      case Primitive::Kind::I8: write(ref.get<int8_t>()); break;
      case Primitive::Kind::I16: write(ref.get<int16_t>()); break;
      case Primitive::Kind::I32: write(ref.get<int32_t>()); break;
      case Primitive::Kind::I64: write(ref.get<int64_t>()); break;
      case Primitive::Kind::U8: write(ref.get<uint8_t>()); break;
      case Primitive::Kind::U16: write(ref.get<uint16_t>()); break;
      case Primitive::Kind::U32: write(ref.get<uint32_t>()); break;
      case Primitive::Kind::U64: write(ref.get<uint64_t>()); break;
      case Primitive::Kind::F32: write(ref.get<float>()); break;
      case Primitive::Kind::F64: write(ref.get<double>()); break;
      case Primitive::Kind::Byte: write(ref.get<std::byte>()); break;
      case Primitive::Kind::Type_: write(ref.get<Type>()); break;
      default: NOT_YET((int)p->kind());
    }
  }

  template <typename T>
  void write(T const& t) requires(std::integral<T> or std::floating_point<T> or
                                  base::meta<T> ==
                                      base::meta<core::ParameterFlags> or
                                  base::meta<T> == base::meta<Quals>) {
    out_.append(std::string_view(reinterpret_cast<char const*>(&t), sizeof(t)));
  }

  void write(ir::Char c) { out_.push_back(static_cast<char>(c)); }
  void write(std::byte b) { out_.push_back(static_cast<char>(b)); }

  void write(QualType qt) { base::Serialize(*this, qt.quals(), qt.type()); }
  void write(Type t) { base::Serialize(*this, system_.index(t)); }

 private:
  TypeSystem const& system_;
  std::string& out_;
};

struct ValueDeserializer {
  using signature = bool(ir::CompleteResultBuffer&);

  explicit ValueDeserializer(
      absl::Span<std::byte const> span,
      base::flyweight_map<std::pair<std::string, Function const*>, void (*)()>*
          foreign_fn_map,
      TypeSystem* system)
      : head_(span.begin()),
        end_(span.end()),
        foreign_fn_map_(*ASSERT_NOT_NULL(foreign_fn_map)),
        system_(*ASSERT_NOT_NULL(system)) {}

  bool operator()(Type t, ir::CompleteResultBuffer& buffer) {
    return t.visit<ValueDeserializer>(*this, buffer);
  }

  bool operator()(auto const* t, ir::CompleteResultBuffer& buffer) {
    NOT_YET();
  }

  bool operator()(Function const* f, ir::CompleteResultBuffer& buffer) {
    NOT_YET(foreign_fn_map_);
    // std::string s;
    // Type t;
    // if (not base::Deserialize(*this, s, t)) { return false; }
    // ir::ForeignFn fn(
    //     foreign_fn_map_.try_emplace(std::pair(std::move(s),
    //     &t.as<Function>()))
    //         .first);
    // buffer.append(ir::Fn(fn));
    // return true;
  }

  bool operator()(Primitive const* p, ir::CompleteResultBuffer& buffer) {
    switch (p->kind()) {
      case Primitive::Kind::Bool: return Read<bool>(buffer);
      case Primitive::Kind::Char: return Read<ir::Char>(buffer);
      case Primitive::Kind::I8: return Read<int8_t>(buffer);
      case Primitive::Kind::I16: return Read<int16_t>(buffer);
      case Primitive::Kind::I32: return Read<int32_t>(buffer);
      case Primitive::Kind::I64: return Read<int64_t>(buffer);
      case Primitive::Kind::U8: return Read<uint8_t>(buffer);
      case Primitive::Kind::U16: return Read<uint16_t>(buffer);
      case Primitive::Kind::U32: return Read<uint32_t>(buffer);
      case Primitive::Kind::U64: return Read<uint64_t>(buffer);
      case Primitive::Kind::F32: return Read<float>(buffer);
      case Primitive::Kind::F64: return Read<double>(buffer);
      case Primitive::Kind::Byte: return Read<std::byte>(buffer);
      case Primitive::Kind::Type_: return Read<Type>(buffer);
      default: NOT_YET();
    }
  }

  absl::Span<std::byte const> read_bytes(size_t n) {
    std::byte const* p = head_;
    head_ += n;
    return absl::Span<std::byte const>(p, end_ - p);
  }

  template <typename T>
  bool read(T& t) requires(std::integral<T> or std::floating_point<T> or
                           base::meta<T> == base::meta<ir::Char> or
                           base::meta<T> == base::meta<std::byte> or
                           base::meta<T> == base::meta<core::ParameterFlags> or
                           base::meta<T> == base::meta<Quals>) {
    if (end_ - head_ < sizeof(T)) { return false; }
    std::memcpy(&t, head_, sizeof(T));
    head_ += sizeof(T);
    return true;
  }

  bool read(core::Parameters<QualType>& params) {
    std::vector<core::Parameter<QualType>> qts;
    if (not base::Deserialize(*this, qts)) { return false; }
    params = core::Parameters<QualType>(std::move(qts));
    return true;
  }

  bool read(QualType& qt) {
    Quals quals = Quals::Unqualified();
    Type t;
    if (not base::Deserialize(*this, quals, t)) { return false; }
    qt = QualType(t, quals);
    return true;
  }

  bool read(Type& t) {
    size_t index;
    if (not base::Deserialize(*this, index)) { return false; }
    t = system_.from_index(index);
    return true;
  }

  std::byte const* head() const { return head_; }

 private:
  template <typename T>
  bool Read(ir::CompleteResultBuffer& buffer) {
    T t;
    if (not base::Deserialize(*this, t)) { return false; }
    buffer.append(t);
    return true;
  };

  std::byte const* head_;
  std::byte const* end_;

  base::flyweight_map<std::pair<std::string, Function const*>, void (*)()>&
      foreign_fn_map_;
  TypeSystem& system_;
};

struct TypeSystemSerializingVisitor {
  using signature = void(module_proto::TypeDefinition& out);

  explicit TypeSystemSerializingVisitor(TypeSystem const* system)
      : system_(*ASSERT_NOT_NULL(system)) {}

  void operator()(Type t, module_proto::TypeDefinition& out) {
    t.visit(*this, out);
  }

  void operator()(auto const* t, module_proto::TypeDefinition& out) {
    Visit(t, out);
  }

 private:
  void Visit(Primitive const* p, module_proto::TypeDefinition& out) {
    out.set_primitive(static_cast<int>(p->kind()));
  }

  void Visit(Array const* a, module_proto::TypeDefinition& out) { ; }
  void Visit(Pointer const* p, module_proto::TypeDefinition& out) {
    out.set_pointer(system_.index(p->pointee()));
  }
  void Visit(BufferPointer const* p, module_proto::TypeDefinition& out) {
    out.set_buffer_pointer(system_.index(p->pointee()));
  }
  void Visit(Function const* f, module_proto::TypeDefinition& out) {
    auto& fn = *out.mutable_function();
    fn.set_eager(f->eager());
    for (auto const& param : f->params()) {
      auto& p = *fn.add_parameter();
      p.set_name(param.name);
      p.set_type(system_.index(param.value.type()));
      p.set_flags((param.flags.value() << uint8_t{8}) |
                  param.value.quals().value());
    }
    for (Type t : f->return_types()) { fn.add_return_type(system_.index(t)); }
  }
  void Visit(Slice const* s, module_proto::TypeDefinition& out) {
    out.set_slice(system_.index(s->data_type()));
  }
  void Visit(auto const* s, module_proto::TypeDefinition& out) { ; }

  TypeSystem const& system_;
};

}  // namespace

void SerializeValue(TypeSystem const& system, Type t, ir::CompleteResultRef ref,
                    std::string& out) {
  ValueSerializer vs(&system, &out);
  vs(t, ref);
}

ssize_t DeserializeValue(
    Type t, absl::Span<std::byte const> span, ir::CompleteResultBuffer& buffer,
    base::flyweight_map<std::pair<std::string, Function const*>, void (*)()>&
        foreign_fn_map,
    TypeSystem& system) {
  ValueDeserializer vd(span, &foreign_fn_map, &system);
  return vd(t, buffer) ? vd.head() - span.begin() : -1;
}

module_proto::TypeSystem SerializeTypeSystem(TypeSystem const& system) {
  module_proto::TypeSystem proto;
  TypeSystemSerializingVisitor v(&system);
  for (Type t : system.types()) { v(t, *proto.add_type()); }
  return proto;
}

bool DeserializeTypeSystem(module_proto::TypeSystem& proto,
                           module::SharedContext& context, TypeSystem& system) {
  for (auto const& t : proto.type()) {
    switch (t.type_case()) {
      case module_proto::TypeDefinition::kPrimitive:
        system.insert(
            MakePrimitive(static_cast<Primitive::Kind>(t.primitive())));
        break;
      case module_proto::TypeDefinition::kPointer:
        system.insert(Ptr(system.from_index(t.pointer())));
        break;
      case module_proto::TypeDefinition::kBufferPointer:
        system.insert(BufPtr(system.from_index(t.buffer_pointer())));
        break;
      case module_proto::TypeDefinition::kFunction: {
        core::Parameters<QualType> parameters;
        for (auto const& p : t.function().parameter()) {
          parameters.append(
              p.name(),
              QualType(system.from_index(p.type()),
                       Quals::FromValue(p.flags() & 0xff)),
              core::ParameterFlags::FromValue(p.flags() >> uint8_t{8}));
        }
        std::vector<Type> return_types;
        return_types.reserve(t.function().return_type().size());
        for (int64_t n : t.function().return_type()) {
          return_types.push_back(system.from_index(n));
        }

        auto* make_func = (t.function().eager() ? EagerFunc : Func);
        system.insert(
            make_func(std::move(parameters), std::move(return_types)));
      } break;
      case module_proto::TypeDefinition::kSlice:
        system.insert(Slc(system.from_index(t.slice())));
        break;
      case module_proto::TypeDefinition::TYPE_NOT_SET: UNREACHABLE();
    }
  }
  return true;
}

}  // namespace type
