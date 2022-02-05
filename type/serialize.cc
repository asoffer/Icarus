#include "type/serialize.h"

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

  explicit ValueSerializer(std::string* out) : out_(*ASSERT_NOT_NULL(out)) {}

  void operator()(Type t, ir::CompleteResultRef ref) {
    t.visit<ValueSerializer>(*this, ref);
  }

  void operator()(auto const* t, ir::CompleteResultRef ref) { NOT_YET(); }

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
      default: NOT_YET();
    }
  }

 private:
  template <typename T>
  void write(T const& t) requires(std::integral<T> or std::floating_point<T>) {
    out_.append(std::string_view(reinterpret_cast<char const*>(&t), sizeof(t)));
  }

  void write(ir::Char c) { out_.push_back(static_cast<char>(c)); }
  void write(std::byte b) { out_.push_back(static_cast<char>(b)); }
  void write(Type t) {
    if (t.is<Primitive>()) {
      write(static_cast<std::underlying_type_t<Primitive::Kind>>(
          t.as<Primitive>().kind()));
    } else {
      NOT_YET();
    }
  }

  std::string& out_;
};

struct ValueDeserializer {
  using signature = bool(ir::CompleteResultBuffer&);

  explicit ValueDeserializer(absl::Span<std::byte const> span)
      : head_(span.begin()), end_(span.end()) {}

  bool operator()(Type t, ir::CompleteResultBuffer& buffer) {
    return t.visit<ValueDeserializer>(*this, buffer);
  }

  bool operator()(auto const* t, ir::CompleteResultBuffer& buffer) {
    NOT_YET();
  }

  bool operator()(Primitive const* p, ir::CompleteResultBuffer& buffer) {
    switch (p->kind()) {
      case Primitive::Kind::Bool: return read<bool>(buffer);
      case Primitive::Kind::Char: return read<ir::Char>(buffer);
      case Primitive::Kind::I8: return read<int8_t>(buffer);
      case Primitive::Kind::I16: return read<int16_t>(buffer);
      case Primitive::Kind::I32: return read<int32_t>(buffer);
      case Primitive::Kind::I64: return read<int64_t>(buffer);
      case Primitive::Kind::U8: return read<uint8_t>(buffer);
      case Primitive::Kind::U16: return read<uint16_t>(buffer);
      case Primitive::Kind::U32: return read<uint32_t>(buffer);
      case Primitive::Kind::U64: return read<uint64_t>(buffer);
      case Primitive::Kind::F32: return read<float>(buffer);
      case Primitive::Kind::F64: return read<double>(buffer);
      case Primitive::Kind::Byte: return read<std::byte>(buffer);
      case Primitive::Kind::Type_: {
        read<std::underlying_type_t<Primitive::Kind>>(buffer);
        auto k = static_cast<Primitive::Kind>(
            buffer[0].get<std::underlying_type_t<Primitive::Kind>>());
        buffer.clear();
        buffer.append(MakePrimitive(k));
        return sizeof(std::underlying_type_t<Primitive::Kind>);
      } break;
      default: NOT_YET();
    }
  }

 private:
  template <typename T>
  bool read(ir::CompleteResultBuffer& buffer) requires(
      std::is_trivially_copyable_v<T>) {
    if (end_ - head_ < sizeof(T)) { return false; }
    T t;
    std::memcpy(&t, head_, sizeof(T));
    head_ += sizeof(T);
    buffer.append(t);
    return true;
  };

  std::byte const* head_;
  std::byte const* end_;
};

}  // namespace

void SerializeValue(Type t, ir::CompleteResultRef ref, std::string& out) {
  ValueSerializer vs(&out);
  vs(t, ref);
}

ssize_t DeserializeValue(Type t, absl::Span<std::byte const> span,
                         ir::CompleteResultBuffer& buffer) {
  ValueDeserializer vd(span);
  return vd(t, buffer);
}

}  // namespace type
