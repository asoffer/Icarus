#include "semantic_analysis/type_system.h"

namespace semantic_analysis {
namespace {

void DebugType(std::ostream& os, core::Type t, TypeSystem& ts) {
  if (auto p = t.get_if<PrimitiveType>(ts)) {
    static constexpr std::array kPrimitiveTypes{
        "bool", "char",    "byte",   "f32",         "f64",
        "type", "integer", "module", "empty-array", "error"};
    size_t value =
        static_cast<std::underlying_type_t<decltype(p->value())>>(p->value());
    ASSERT(value < kPrimitiveTypes.size());
    os << kPrimitiveTypes[value];
  } else if (auto p = t.get_if<core::PointerType>(ts)) {
    os << "*(";
    DebugType(os, p->pointee(), ts);
    os << ')';
  } else if (auto p = t.get_if<BufferPointerType>(ts)) {
    os << "[*](";
    DebugType(os, p->pointee(), ts);
    os << ')';
  } else if (auto i = t.get_if<core::SizedIntegerType>(ts)) {
    os << (i->is_signed() ? 'i' : 'u') << i->bits();
    if (i->alignment() != core::SizedIntegerType::DefaultAlignment(i->bits())) {
      os << "@" << i->alignment();
    }
  } else if (auto s = t.get_if<SliceType>(ts)) {
    os << "[/](";
    DebugType(os, s->pointee(), ts);
    os << ')';
  } else if (auto a = t.get_if<ArrayType>(ts)) {
    os << '[' << a->length() << "; ";
    DebugType(os, a->data_type(), ts);
    os << ']';
  } else if (auto f = t.get_if<core::FunctionType>(ts)) {
    os << "((";
    std::string_view separator = "";
    for (auto const& p : f->parameters()) {
      os << std::exchange(separator, ", ") << p.name << ": ";
      DebugType(os, p.value, ts);
    }
    os << ") -> (";
    for (core::Type t : f->returns()) { DebugType(os, t, ts); }
    os << "))";
  } else {
    os << "???";
  }
}

void DebugQualifiedType(std::ostream& os, QualifiedType qt, TypeSystem& ts) {
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
  DebugType(os, qt.type(), ts);
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

std::string DebugQualifiedType(QualifiedType qt, TypeSystem& ts) {
  std::stringstream ss;
  DebugQualifiedType(ss, qt, ts);
  return ss.str();
}

std::string DebugType(core::Type qt, TypeSystem& ts) {
  std::stringstream ss;
  DebugType(ss, qt, ts);
  return ss.str();
}

}  // namespace semantic_analysis
