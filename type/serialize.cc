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

  explicit ValueSerializer(std::string* out) : out_(*ASSERT_NOT_NULL(out)) {}

  void operator()(Type t, ir::CompleteResultRef ref) {
    t.visit<ValueSerializer>(*this, ref);
  }

  void operator()(auto const* t, ir::CompleteResultRef ref) {
    NOT_YET(t->to_string());
  }

  void operator()(Function const* t, ir::CompleteResultRef ref) {
    auto f = ref.get<ir::Fn>().foreign()
    NOT_YET(t->to_string());
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
      default: NOT_YET();
    }
  }

  template <typename T>
  void write(T const& t) requires(std::integral<T> or std::floating_point<T> or
                                  base::meta<T> == base::meta<Quals>) {
    out_.append(std::string_view(reinterpret_cast<char const*>(&t), sizeof(t)));
  }

  void write(ir::Char c) { out_.push_back(static_cast<char>(c)); }
  void write(std::byte b) { out_.push_back(static_cast<char>(b)); }

  void write(core::Param<QualType> const& p) {
    // TODO: Flags.
    base::Serialize(*this, p.name, p.value);
  }

  void write(QualType qt) { base::Serialize(*this, qt.quals(), qt.type()); }

  void write(Type t) {
    write(t.get()->which());
    if (auto const* p = t.if_as<Primitive>()) {
      write(static_cast<std::underlying_type_t<Primitive::Kind>>(p->kind()));
    } else if (auto const* f = t.if_as<Function>()) {
      base::Serialize(*this, f->params(), f->return_types());
    } else {
      NOT_YET(t.to_string());
    }
  }

 private:
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
        ASSIGN_OR(return false, int8_t which_type, read<int8_t>());
        switch (which_type) {
          case IndexOf<Primitive>(): {
            auto k = read<std::underlying_type_t<Primitive::Kind>>();
            if (not k.has_value()) { return false; }
            buffer.append(MakePrimitive(static_cast<Primitive::Kind>(*k)));
            return true;
          } break;
          default: NOT_YET();
        }
      } break;
      default: NOT_YET();
    }
  }

  std::byte const* head() const { return head_; }

 private:
  template <typename T>
  std::optional<T> read() requires(std::is_trivially_copyable_v<T>) {
    std::optional<T> result = std::nullopt;
    if (end_ - head_ >= sizeof(T)) {
      result.emplace();
      std::memcpy(&*result, head_, sizeof(T));
      head_ += sizeof(T);
    }
    return result;
  };

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
  return vd(t, buffer) ? vd.head() - span.begin() : -1;
}

}  // namespace type
