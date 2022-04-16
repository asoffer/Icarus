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

  explicit ValueSerializer(TypeSystem const* system, precompiled::Value* value)
      : system_(*ASSERT_NOT_NULL(system)), value_(*ASSERT_NOT_NULL(value)) {}

  void operator()(Type t, ir::CompleteResultRef ref) {
    t.visit<ValueSerializer>(*this, ref);
  }

  void operator()(auto const* t, ir::CompleteResultRef ref) {
    NOT_YET(t->to_string());
  }

  void operator()(Function const* t, ir::CompleteResultRef ref) { NOT_YET(); }

  void operator()(Primitive const* p, ir::CompleteResultRef ref) {
    switch (p->kind()) {
      case Primitive::Kind::Bool: value_.set_boolean(ref.get<bool>()); break;
      case Primitive::Kind::Char:
        value_.set_unsigned_integer(ref.get<ir::Char>().as_type<uint8_t>());
        break;
      case Primitive::Kind::I8:
        value_.set_signed_integer(ref.get<int8_t>());
        break;
      case Primitive::Kind::I16:
        value_.set_signed_integer(ref.get<int16_t>());
        break;
      case Primitive::Kind::I32:
        value_.set_signed_integer(ref.get<int32_t>());
        break;
      case Primitive::Kind::I64:
        value_.set_signed_integer(ref.get<int64_t>());
        break;
      case Primitive::Kind::U8:
        value_.set_unsigned_integer(ref.get<uint8_t>());
        break;
      case Primitive::Kind::U16:
        value_.set_unsigned_integer(ref.get<uint16_t>());
        break;
      case Primitive::Kind::U32:
        value_.set_unsigned_integer(ref.get<uint32_t>());
        break;
      case Primitive::Kind::U64:
        value_.set_unsigned_integer(ref.get<uint64_t>());
        break;
      case Primitive::Kind::F32: value_.set_real(ref.get<float>()); break;
      case Primitive::Kind::F64: value_.set_real(ref.get<double>()); break;
      case Primitive::Kind::Byte:
        value_.set_unsigned_integer(static_cast<uint8_t>(ref.get<std::byte>()));
        break;
      case Primitive::Kind::Type_:
        value_.set_type(system_.index(ref.get<Type>()));
        break;
      default: NOT_YET((int)p->kind());
    }
  }

 private:
  TypeSystem const& system_;
  precompiled::Value& value_;
};

struct ValueDeserializer {
  using signature = void(ir::CompleteResultBuffer& buffer);

  explicit ValueDeserializer(TypeSystem const* system,
                             precompiled::Value const* value)
      : system_(*ASSERT_NOT_NULL(system)), value_(*ASSERT_NOT_NULL(value)) {}

  void operator()(Type t, ir::CompleteResultBuffer& buffer) {
    return t.visit<ValueDeserializer>(*this, buffer);
  }

  void operator()(auto const* t, ir::CompleteResultBuffer& buffer) {
    NOT_YET();
  }

  void operator()(Function const* , ir::CompleteResultBuffer& buffer) {
    uint64_t fn = value_.function();
    // TODO: We need to update the module identifier to be accurate within the
    // compilation of the given module.
    buffer.append(ir::Fn(ir::ModuleId(fn >> uint64_t{32}),
                         ir::LocalFnId(fn & uint64_t{0xffffffff})));
  }

  void operator()(Primitive const* p, ir::CompleteResultBuffer& buffer) {
    switch (p->kind()) {
      case Primitive::Kind::Bool: buffer.append(value_.boolean()); break;
      case Primitive::Kind::Char: {
        ASSERT(value_.unsigned_integer() <= std::numeric_limits<uint8_t>::max());
        buffer.append(ir::Char(static_cast<uint8_t>(value_.unsigned_integer())));
      } break;
      case Primitive::Kind::I8: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int8_t>::max());
        buffer.append(static_cast<int8_t>(value_.signed_integer()));
      } break;
      case Primitive::Kind::I16: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int16_t>::max());
        buffer.append(static_cast<int16_t>(value_.signed_integer()));
      } break;
      case Primitive::Kind::I32: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int32_t>::max());
        buffer.append(static_cast<int32_t>(value_.signed_integer()));
      } break;
      case Primitive::Kind::I64: {
        ASSERT(value_.signed_integer() <= std::numeric_limits<int64_t>::max());
        buffer.append(static_cast<int64_t>(value_.signed_integer()));
      } break;
      case Primitive::Kind::U8: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint8_t>::max());
        buffer.append(static_cast<uint8_t>(value_.unsigned_integer()));
      } break;
      case Primitive::Kind::U16: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint16_t>::max());
        buffer.append(static_cast<uint16_t>(value_.unsigned_integer()));
      } break;
      case Primitive::Kind::U32: {
        ASSERT(value_.unsigned_integer() >= 0);
        buffer.append(static_cast<uint32_t>(value_.unsigned_integer()));
      } break;
      case Primitive::Kind::U64: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint64_t>::max());
        buffer.append(static_cast<uint64_t>(value_.unsigned_integer()));
      } break;
      case Primitive::Kind::F32: {
        buffer.append(static_cast<float>(value_.real()));
      } break;
      case Primitive::Kind::F64: {
        buffer.append(static_cast<double>(value_.real()));
      } break;
      case Primitive::Kind::Byte: {
        ASSERT(value_.unsigned_integer() <=
               std::numeric_limits<uint8_t>::max());
        buffer.append(
            std::byte(static_cast<uint8_t>(value_.unsigned_integer())));
      } break;
      case Primitive::Kind::Type_: {
        buffer.append(system_.from_index(value_.type()));
      } break;
      default: NOT_YET();
    }
  }

 private:
  TypeSystem const& system_;
  precompiled::Value const & value_;
};

}  // namespace

void SerializeValue(TypeSystem const& system, Type t, ir::CompleteResultRef ref,
                    precompiled::Value& value) {
  value.set_type(system.index(t));
  ValueSerializer vs(&system, &value);
  vs(t, ref);
}

ir::CompleteResultBuffer DeserializeValue(TypeSystem const& system,
                                          precompiled::Value const& value) {
  ir::CompleteResultBuffer result;
  ValueDeserializer vd(&system, &value);
  vd(system.from_index(value.type_id()), result);
  return result;
}

}  // namespace type
