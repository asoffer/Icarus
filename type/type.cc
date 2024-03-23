#include "type/type.h"

#include <cstring>
#include <utility>
#include <vector>

#include "common/constant/manifest.h"
#include "nth/debug/debug.h"
#include "nth/utility/no_destructor.h"
#include "type/type_contour.h"

namespace ic::type {
namespace {

uint64_t opaque_count = 0;

}  // namespace

Type::Type(from_index_t, size_t index) {
  mutable_value() =
      (index << 8) |
      static_cast<uint8_t>(ConstantManifest::Global()[index].category());
}

size_t JasminSize(Type t) {
  switch (t.kind()) {
    case Type::Kind::Primitive: return 1;
    case Type::Kind::Parameters: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::Pattern:
    case Type::Kind::Function: return 1;
    case Type::Kind::Slice: return 2;
    case Type::Kind::Pointer: return 1;
    case Type::Kind::BufferPointer: return 1;
    case Type::Kind::Opaque: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::DependentFunction: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::Refinement:
      NTH_UNIMPLEMENTED();
      // return JasminSize(t.as<type::RefinementType>().underlying());
  }
}

TypeContour Contour(Type t) {
  NTH_UNIMPLEMENTED();
  //   switch (t.kind()) {
  //     case Type::Kind::Primitive:
  //       switch (t.as<type::PrimitiveType>().primitive_kind()) {
  //         case PrimitiveType::Kind::Error:
  //         case PrimitiveType::Kind::Bottom:
  //         default: NTH_UNREACHABLE();
  //         case PrimitiveType::Kind::NullType:
  //         case PrimitiveType::Kind::Scope_:
  //           return TypeContour(ByteWidth(0), Alignment(1));
  //         case PrimitiveType::Kind::Bool:
  //         case PrimitiveType::Kind::Char:
  //         case PrimitiveType::Kind::Byte:
  //         case PrimitiveType::Kind::I8:
  //         case PrimitiveType::Kind::U8:
  //           return TypeContour(ByteWidth(1), Alignment(1));
  //         case PrimitiveType::Kind::I16:
  //         case PrimitiveType::Kind::U16:
  //           return TypeContour(ByteWidth(2), Alignment(2));
  //         case PrimitiveType::Kind::I32:
  //         case PrimitiveType::Kind::U32:
  //         case PrimitiveType::Kind::F32:
  //           return TypeContour(ByteWidth(4), Alignment(4));
  //         case PrimitiveType::Kind::Module:
  //         case PrimitiveType::Kind::Type:
  //         case PrimitiveType::Kind::Integer:
  //         case PrimitiveType::Kind::I64:
  //         case PrimitiveType::Kind::U64:
  //         case PrimitiveType::Kind::F64:
  //           return TypeContour(ByteWidth(8), Alignment(8));
  //       }
  //     case Type::Kind::Slice: return TypeContour(ByteWidth(16),
  //     Alignment(8)); case Type::Kind::Function: case Type::Kind::Pointer:
  //     case Type::Kind::BufferPointer:
  //       return TypeContour(ByteWidth(8), Alignment(8));
  //     case Type::Kind::Opaque: NTH_UNREACHABLE("{}") <<= {t};
  //     case Type::Kind::Refinement: NTH_UNIMPLEMENTED();
  //       // return Contour(t.as<type::RefinementType>().underlying());
  //     default: NTH_UNIMPLEMENTED("{}") <<= {t.kind()};
  //   }
}

}  // namespace ic::type
