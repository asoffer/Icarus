#include "type/serialize.h"

#include "base/macros.h"
#include "ir/value/char.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
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

  void operator()(Function const* t, ir::CompleteResultRef ref) {
    auto f = ref.get<ir::Fn>().foreign();
    base::Serialize(*this, f.name());
    write(type::Type(f.type()));
  }

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
                                      base::meta<core::ParamFlags> or
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
    std::string s;
    Type t;
    if (not base::Deserialize(*this, s, t)) { return false; }
    ir::ForeignFn fn(
        foreign_fn_map_.try_emplace(std::pair(std::move(s), &t.as<Function>()))
            .first);
    buffer.append(ir::Fn(fn));
    return true;
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
                           base::meta<T> == base::meta<core::ParamFlags> or
                           base::meta<T> == base::meta<Quals>) {
    if (end_ - head_ < sizeof(T)) { return false; }
    std::memcpy(&t, head_, sizeof(T));
    head_ += sizeof(T);
    return true;
  }

  bool read(core::Params<QualType>& params) {
    std::vector<core::Param<QualType>> qts;
    if (not base::Deserialize(*this, qts)) { return false; }
    params = core::Params<QualType>(std::move(qts));
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
  using signature = void();

  explicit TypeSystemSerializingVisitor(TypeSystem const* system,
                                        std::string* out)
      : system_(*ASSERT_NOT_NULL(system)), out_(*ASSERT_NOT_NULL(out)) {}

  void operator()(Type t) { t.visit(*this); }

  void operator()(auto const* t) {
    base::Serialize(*this, IndexOf<std::decay_t<decltype(*t)>>());
    Visit(t);
  }

  void write_bytes(absl::Span<std::byte const> bytes) {
    out_.append(std::string_view(reinterpret_cast<char const*>(bytes.data()),
                                 bytes.size()));
  }

  template <typename T>
  void write(T const& t) requires(std::is_enum_v<T> or
                                  std::is_arithmetic_v<T> or
                                  base::meta<T> == base::meta<type::Quals>) {
    auto const* p = reinterpret_cast<std::byte const*>(&t);
    write_bytes(absl::MakeConstSpan(p, p + sizeof(T)));
  }

  void write(Type t) {
    size_t index = system_.index(t);
    ASSERT(index != system_.end_index());
    base::Serialize(*this, index);
  }

  void write(QualType qt) { base::Serialize(*this, qt.quals(), qt.type()); }

 private:
  void Visit(Primitive const* p) { base::Serialize(*this, p->kind()); }

  void Visit(Array const* a) {
    base::Serialize(*this, a->length(), a->data_type());
  }
  void Visit(Pointer const* p) { base::Serialize(*this, p->pointee()); }
  void Visit(BufferPointer const* p) { base::Serialize(*this, p->pointee()); }

  void Visit(Function const* f) {
    base::Serialize(*this, f->eager(), f->params(), f->return_types());
  }

  void Visit(Opaque const* o) {
    base::Serialize(*this, o->defining_module()->identifier(), o->numeric_id());
  }

  void Visit(Slice const* s) { base::Serialize(*this, s->data_type()); }

  void Visit(auto const* s) { NOT_YET(); }

  TypeSystem const& system_;
  std::string& out_;
};

struct TypeSystemDeserializingVisitor {
  explicit TypeSystemDeserializingVisitor(std::string_view* content,
                                          TypeSystem* system)
      : content_(*ASSERT_NOT_NULL(content)),
        system_(*ASSERT_NOT_NULL(system)) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    absl::Span<std::byte const> result(
        reinterpret_cast<std::byte const*>(content_.data()), num_bytes);
    content_.remove_prefix(num_bytes);
    return result;
  }

  template <typename T>
  bool read(T& t) requires(std::is_arithmetic_v<T> or std::is_enum_v<T> or
                           base::meta<T> == base::meta<Quals>) {
    absl::Span span = read_bytes(sizeof(t));
    std::memcpy(&t, span.data(), sizeof(t));
    return true;
  }

  bool read(Type& t) {
    size_t index;
    if (not base::Deserialize(*this, index)) { return false; }
    t = system_.from_index(index);
    return true;
  }

  bool read(QualType& qt) {
    Quals quals = Quals::Unqualified();
    Type t;
    if (not base::Deserialize(*this, quals, t)) { return false; }
    qt = QualType(t, quals);
    return true;
  }

  bool operator()() {
    int8_t which;
    base::Deserialize(*this, which);
    switch (which) {
      case IndexOf<Array>(): {
        ir::Integer length;
        Type t;
        if (not base::Deserialize(*this, length, t)) { return false; }
        [[maybe_unused]] auto [iter, inserted] = system_.insert(Arr(length, t));
        ASSERT(inserted == true);
        return true;
      }
      case IndexOf<Primitive>(): {
        Primitive::Kind k;
        if (not base::Deserialize(*this, k)) { return false; }
        [[maybe_unused]] auto [iter, inserted] =
            system_.insert(MakePrimitive(k));
        ASSERT(inserted == true);
        return true;
      }
      case IndexOf<Pointer>(): {
        Type pointee;
        if (not base::Deserialize(*this, pointee)) { return false; }
        [[maybe_unused]] auto [iter, inserted] = system_.insert(Ptr(pointee));
        ASSERT(inserted == true);
        return true;
      }
      case IndexOf<BufferPointer>(): {
        Type pointee;
        if (not base::Deserialize(*this, pointee)) { return false; }
        [[maybe_unused]] auto [iter, inserted] =
            system_.insert(BufPtr(pointee));
        ASSERT(inserted == true);
        return true;
      }
      case IndexOf<Slice>(): {
        Type data_type;
        if (not base::Deserialize(*this, data_type)) { return false; }
        [[maybe_unused]] auto [iter, inserted] = system_.insert(Slc(data_type));
        ASSERT(inserted == true);
        return true;
      }
      case IndexOf<Function>(): {
        bool eager;
        core::Params<QualType> params;
        std::vector<Type> return_types;

        if (not base::Deserialize(*this, eager, params, return_types)) {
          return false;
        }

        auto* f = eager ? EagerFunc(std::move(params), std::move(return_types))
                        : Func(std::move(params), std::move(return_types));
        [[maybe_unused]] auto [iter, inserted] = system_.insert(f);
        ASSERT(inserted == true);
        return true;
      }
      case IndexOf<Opaque>(): {
        std::string module_identifier;
        uintptr_t numeric_id;
        if (not base::Deserialize(*this, module_identifier, numeric_id)) {
          return false;
        }
      }
      default: UNREACHABLE((int)which);
    }
  }

 private:
  std::string_view &content_;
  TypeSystem& system_;
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

void SerializeTypeSystem(TypeSystem const& system, std::string& out) {
  TypeSystemSerializingVisitor v(&system, &out);
  base::Serialize(v, system.size());
  for (Type t : system.types()) { v(t); }
}

bool DeserializeTypeSystem(std::string_view& content, TypeSystem& system) {
  TypeSystemDeserializingVisitor v(&content, &system);
  size_t num_types;
  base::Deserialize(v, num_types);
  for (size_t i = 0; i < num_types; ++i) {
    if (not v()) { return false; }
  }

  return true;
}

}  // namespace type
