#include "jasmin/debug.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Index const *node,
                                      FunctionData data) {
  as<ByteCodeReferenceEmitter>()(node, data);

  auto qt          = context().qualified_type(node);
  core::Bytes size = SizeOf(qt.type());

  if (PassInRegister(qt)) {
    if (size <= core::Bytes(jasmin::ValueSize)) {
      data.function().AppendLoad(jasmin::ValueSize);
    } else if (size <= 2 * core::Bytes(jasmin::ValueSize)) {
      data.function().AppendDuplicate();
      data.function().AppendLoad(jasmin::ValueSize);
      data.function().AppendSwap();
      data.function().AppendIncrementPointer(jasmin::ValueSize);
      data.function().AppendLoad(size.value() - jasmin::ValueSize);
      // TODO:This swap is specific to it being a slice.
      data.function().AppendSwap();
    } else {
      NTH_UNREACHABLE();
    }
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void ByteCodeStatementEmitter::operator()(ast::Index const *node,
                                          FunctionData data) {
  NTH_UNIMPLEMENTED();
}

void ByteCodeReferenceEmitter::operator()(ast::Index const *node,
                                          FunctionData data) {
  if (context().qualified_type(node->lhs()).qualifiers() >=
      Qualifiers::Reference()) {
    NTH_UNIMPLEMENTED();
  } else {
    auto &f = data.function();
    as<ByteCodeValueEmitter>().Emit(node->lhs(), data);
    f.AppendDrop(1);
    as<ByteCodeValueEmitter>().CastTo(node->rhs(), QualifiedType(U(64)), data);
    uint64_t size = SizeOf(context().qualified_type(node).type()).value();
    f.AppendPush(size);
    f.AppendBinary<'*', uint64_t>();
    f.AppendIncrementPointer();
  }
}

}  // namespace semantic_analysis
