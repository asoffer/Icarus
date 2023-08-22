#include "semantic_analysis/type_system.h"

#include "core/arch.h"
#include "core/integer.h"

namespace semantic_analysis {
namespace {

void DebugType(std::ostream& os, core::Type t) {
  if (auto p = t.get_if<PrimitiveType>(GlobalTypeSystem)) {
    static constexpr std::array kPrimitiveTypes{
        "bool",    "char",   "byte",      "f32",         "f64",     "type",
        "integer", "module", "no-return", "empty-array", "nullptr", "error"};
    size_t value =
        static_cast<std::underlying_type_t<decltype(p->value())>>(p->value());
    NTH_ASSERT(value < kPrimitiveTypes.size());
    os << kPrimitiveTypes[value];
  } else if (auto p = t.get_if<core::PointerType>(GlobalTypeSystem)) {
    os << "*(";
    DebugType(os, p->pointee());
    os << ')';
  } else if (auto p = t.get_if<BufferPointerType>(GlobalTypeSystem)) {
    os << "[*](";
    DebugType(os, p->pointee());
    os << ')';
  } else if (auto i = t.get_if<core::SizedIntegerType>(GlobalTypeSystem)) {
    os << (i->is_signed() ? 'i' : 'u') << i->bits();
    if (i->alignment() != core::SizedIntegerType::DefaultAlignment(i->bits())) {
      os << "@" << i->alignment();
    }
  } else if (auto s = t.get_if<SliceType>(GlobalTypeSystem)) {
    os << "[/](";
    DebugType(os, s->pointee());
    os << ')';
  } else if (auto a = t.get_if<ArrayType>(GlobalTypeSystem)) {
    os << '[' << a->length() << "; ";
    DebugType(os, a->data_type());
    os << ']';
  } else if (auto f = t.get_if<core::FunctionType>(GlobalTypeSystem)) {
    os << "((";
    std::string_view separator = "";
    for (auto const& p : f->parameters()) {
      os << std::exchange(separator, ", ") << p.name << ": ";
      DebugType(os, p.value);
    }
    os << ") -> (";
    for (core::Type t : f->returns()) { DebugType(os, t); }
    os << "))";
  } else if (auto p = t.get_if<core::ParameterType>(GlobalTypeSystem)) {
    std::string_view separator = "";
    for (auto const& param : p->value()) {
      os << std::exchange(separator, ", ");
      if (not param.name.empty()) { os << param.name << ": "; }
      DebugType(os, param.value);
    }
  } else if (auto p = t.get_if<EnumType>(GlobalTypeSystem)) {
    os << "Enum(" << t.index() << ")";
  } else if (auto p = t.get_if<OpaqueType>(GlobalTypeSystem)) {
    os << "OpaqueType(" << t.index() << ")";
  } else {
    os << "Unknown(" << t.index() << ", " << t.category() << ")";
  }
}

void DebugQualifiedType(std::ostream& os, QualifiedType qt) {
  std::string_view separator = "";
  Qualifiers qualifiers      = qt.qualifiers();
  if (qualifiers >= Qualifiers::Constant()) {
    os << std::exchange(separator, "-") << "constant";
  }
  if (qualifiers >= Qualifiers::Buffer()) {
    os << std::exchange(separator, "-") << "buffer";
  } else if (qualifiers >= Qualifiers::Reference()) {
    os << std::exchange(separator, "-") << "reference";
  }
  if (qualifiers >= Qualifiers::Error()) {
    os << std::exchange(separator, "-") << "error";
  }
  os << '(';
  DebugType(os, qt.type());
  os << ')';
}

}  // namespace

QualifiedType Constant(core::Type t) {
  return QualifiedType(t, Qualifiers::Constant());
}

QualifiedType Constant(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Constant());
}

QualifiedType Reference(core::Type t) {
  return QualifiedType(t, Qualifiers::Reference());
}

QualifiedType Reference(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Reference());
}

QualifiedType Error() { return QualifiedType(ErrorType, Qualifiers::Error()); }

QualifiedType Error(core::Type t) {
  return QualifiedType(t, Qualifiers::Error());
}

QualifiedType Error(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Error());
}

QualifiedType Error(Qualifiers q) {
  return QualifiedType(ErrorType, q | Qualifiers::Error());
}

bool IsIntegral(core::Type t) {
  return t == Integer or
         t.category() == TypeSystem::index<core::SizedIntegerType>();
}

bool IsNumeric(core::Type t) { return IsIntegral(t) or t == F32 or t == F64; }

std::string DebugQualifiedType(QualifiedType qt) {
  std::stringstream ss;
  DebugQualifiedType(ss, qt);
  return ss.str();
}

std::string DebugType(core::Type qt) {
  std::stringstream ss;
  DebugType(ss, qt);
  return ss.str();
}

core::Bytes SizeOf(core::Type t) { return ContourOf(t).bytes(); }
core::Alignment AlignmentOf(core::Type t) { return ContourOf(t).alignment(); }
core::TypeContour ContourOf(core::Type t) {
  if (auto p = t.get_if<PrimitiveType>(GlobalTypeSystem)) {
    static constexpr std::array kPrimitiveTypes{
        core::TypeContour(core::Bytes{1}, core::Alignment{1}),  // Bool
        core::TypeContour(core::Bytes{1}, core::Alignment{1}),  // Char
        core::TypeContour(core::Bytes{1}, core::Alignment{1}),  // Byte
        core::TypeContour(core::Bytes{4}, core::Alignment{4}),  // F32
        core::TypeContour(core::Bytes{8}, core::Alignment{8}),  // F64
        core::TypeContour(core::Bytes{8}, core::Alignment{8}),  // Type
        core::TypeContour::Get<core::Integer>(),                // Integer
        core::TypeContour(core::Bytes{8}, core::Alignment{8})   // Module
    };
    size_t value =
        static_cast<std::underlying_type_t<decltype(p->value())>>(p->value());
    NTH_ASSERT(value < kPrimitiveTypes.size());
    return kPrimitiveTypes[value];
  } else if (t.is<core::PointerType>(GlobalTypeSystem) or
             t.is<BufferPointerType>(GlobalTypeSystem)) {
    return core::TypeContour::Get<void*>();
  } else if (auto i = t.get_if<core::SizedIntegerType>(GlobalTypeSystem)) {
    if (i->bits() <= 64) {
      size_t result = 2 * std::bit_floor((i->bits() - 1) / CHAR_BIT);
      if (result == 0) { result = 1; }
      return core::TypeContour(core::Bytes{static_cast<int64_t>(result)},
                               core::Alignment{result});
    } else {
      NTH_UNIMPLEMENTED("{}") <<= {DebugType(t)};
    }
  } else if (auto s = t.get_if<SliceType>(GlobalTypeSystem)) {
    // TODO: Take architecture into account.
    return core::TypeContour(core::Bytes{16}, core::Alignment{8});
  } else if (auto a = t.get_if<ArrayType>(GlobalTypeSystem)) {
    // TODO: Take alignment into account.
    auto contour = ContourOf(a->data_type());
    return core::TypeContour(
        core::FwdAlign(contour.bytes(), contour.alignment()) * a->length(),
        contour.alignment());
  } else if (auto f = t.get_if<core::FunctionType>(GlobalTypeSystem)) {
    return core::TypeContour::Get<void (*)()>();
  } else if (auto e = t.get_if<EnumType>(GlobalTypeSystem)) {
    // TODO: Are all enums going to be 64-bits?
    return core::TypeContour::Get<uint64_t>();
  } else {
    NTH_UNIMPLEMENTED("{}") <<= {DebugType(t)};
  }
}

bool PassInRegister(QualifiedType qt) {
  return PassInRegister(ContourOf(qt.type()));
}

bool PassInRegister(core::TypeContour contour) {
  return contour.bytes() <= 2 * core::Bytes{jasmin::ValueSize} and
         contour.alignment() <= core::Alignment{jasmin::ValueAlignment};
}

std::atomic<size_t> OpaqueType::counter = 0;

}  // namespace semantic_analysis