#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::CastTo(ast::Expression const* node,
                                  QualifiedType to_qt, FunctionData data) {
  QualifiedType from_qt = context().qualified_type(node);
  // TODO: Checking the types isn't exactly what you want because this cast
  // could include value -> reference casting.
  if (from_qt.type() == to_qt.type()) {
    Emit(node, data);
    return;
  }
  if (from_qt.qualifiers() >= Qualifiers::Reference()) {
    if (auto p = to_qt.type().get_if<core::PointerType>(type_system());
        p and p->pointee() == from_qt.type()) {
      as<ByteCodeReferenceEmitter>().Emit(node, data);
      return;
    } else if (auto p = to_qt.type().get_if<BufferPointerType>(type_system());
               p and p->pointee() == from_qt.type()) {
      as<ByteCodeReferenceEmitter>().Emit(node, data);
      return;
    }
  }

  if (from_qt.type() == Integer) {
    std::span<std::byte const> evaluation = EvaluateConstant(node, from_qt);
    NTH_ASSERT(sizeof(absl::int128 const*) == evaluation.size());
    absl::int128 const* i;
    std::memcpy(&i, evaluation.data(), sizeof(i));
    // TODO: Actually validate that the number is properly bounded.
    if (to_qt.type() == I(8)) {
      intptr_t value = absl::Int128Low64(*i);
      if (*i < 0) { value = -value; }
      data.function().AppendPush(static_cast<int8_t>(value));
    } else if (to_qt.type() == I(16)) {
      intptr_t value = absl::Int128Low64(*i);
      if (*i < 0) { value = -value; }
      data.function().AppendPush(static_cast<int16_t>(value));
    } else if (to_qt.type() == I(32)) {
      intptr_t value = absl::Int128Low64(*i);
      if (*i < 0) { value = -value; }
      data.function().AppendPush(static_cast<int32_t>(value));
    } else if (to_qt.type() == I(64)) {
      intptr_t value = absl::Int128Low64(*i);
      if (*i < 0) { value = -value; }
      data.function().AppendPush(static_cast<int64_t>(value));
    } else if (to_qt.type() == U(8)) {
      uintptr_t value = absl::Int128Low64(*i);
      data.function().AppendPush(static_cast<uint8_t>(value));
    } else if (to_qt.type() == U(16)) {
      uintptr_t value = absl::Int128Low64(*i);
      data.function().AppendPush(static_cast<uint16_t>(value));
    } else if (to_qt.type() == U(32)) {
      uintptr_t value = absl::Int128Low64(*i);
      data.function().AppendPush(static_cast<uint32_t>(value));
    } else if (to_qt.type() == U(64)) {
      uintptr_t value = absl::Int128Low64(*i);
      data.function().AppendPush(static_cast<uint64_t>(value));
    } else {
      NTH_UNIMPLEMENTED("{} {}") <<= {DebugType(to_qt.type(), type_system()),
                                      DebugType(from_qt.type(), type_system())};
    }
  } else if (auto from =
                 from_qt.type().get_if<core::SizedIntegerType>(type_system())) {
    Emit(node, data);
    if (from_qt.type() == to_qt.type()) { return; }

    if (auto to = to_qt.type().get_if<core::SizedIntegerType>(type_system())) {
      if (to->is_signed()) {
        if (from->is_signed()) {
          data.function().AppendZeroExtendSignedSigned(vm::ZeroExtendOptions{
              .from_bits = static_cast<uint32_t>(from->bits()),
              .to_bits   = static_cast<uint32_t>(to->bits())});
        } else {
          data.function().AppendZeroExtendUnsignedSigned(vm::ZeroExtendOptions{
              .from_bits = static_cast<uint32_t>(from->bits()),
              .to_bits   = static_cast<uint32_t>(to->bits())});
        }
      } else {
        NTH_ASSERT(from->is_signed() == false);
        data.function().AppendZeroExtendUnsignedUnsigned(vm::ZeroExtendOptions{
            .from_bits = static_cast<uint32_t>(from->bits()),
            .to_bits   = static_cast<uint32_t>(to->bits())});
      }
    } else {
      NTH_UNIMPLEMENTED();
    }
  } else if (auto from = from_qt.type() == NullPtr) {
    Emit(node, data);
  } else {
    NTH_UNIMPLEMENTED("{} -> {}") <<=
        {DebugQualifiedType(from_qt, type_system()),
         DebugQualifiedType(to_qt, type_system())};
  }
}

void ByteCodeValueEmitter::operator()(ast::Cast const* node,
                                      FunctionData data) {
  QualifiedType to_qt = context().qualified_type(node);
  CastTo(node->expr(), to_qt, data);
}

void ByteCodeStatementEmitter::operator()(ast::Cast const* node,
                                          FunctionData data) {
  core::Type from_type = context().qualified_type(node->expr()).type();
  core::Type to_type   = context().qualified_type(node).type();
  if (IsNumeric(from_type) and IsNumeric(to_type)) {
    Emit(node->expr(), data);
  } else {
    as<ByteCodeValueEmitter>().Emit(node->expr(), data);
    data.function().AppendDrop(SizeOf(to_type, type_system()).value());
  }
}

}  // namespace semantic_analysis
