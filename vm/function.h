#ifndef ICARUS_VM_FUNCTION_H
#define ICARUS_VM_FUNCTION_H

#include "core/type_system/type.h"
#include "jasmin/function.h"
#include "jasmin/value.h"
#include "semantic_analysis/type_system.h"
#include "serialization/foreign_symbol_map.h"
#include "vm/instructions.h"

namespace vm {

// While using `jasmin::Function<...>` is sufficient, this wrapper type allows
// us to hide the specific instruction set outside of the header so as to reduce
// compilation time by not requiring every translation unit to reinstantiate
// `jasmin::Function<...>`.
struct Function {
  explicit Function(uint8_t parameter_count, uint8_t return_count);
  Function(Function const &);
  Function(Function &&);
  Function &operator=(Function const &);
  Function &operator=(Function &&);
  ~Function();

  // Returns the number of parameters this function accepts.
  uint8_t parameter_count() const;

  // Returns the number of values this function returns.
  uint8_t return_count() const;

  std::span<jasmin::Value const> raw_instructions() const;
  std::span<jasmin::Value> raw_instructions();

  void set_value(jasmin::OpCodeRange range, size_t index, jasmin::Value value);

  void AppendCall();
  void AppendReturn();
  void AppendDrop(size_t n);
  void AppendSwap();
  void AppendPush(jasmin::Value v);
  void AppendPushFunction(jasmin::Value v);
  void AppendPushStringLiteral(std::string_view s);
  void AppendDuplicate();
  void AppendDuplicateAt(size_t n);

  void AppendStore(uint8_t size);
  void AppendLoad(uint8_t size);

  void AppendStackOffset(size_t size);
  void AppendStackAllocate(size_t size);

  void AppendBuiltinForeignFunction(
      core::Type t, void *raw_table,
      serialization::ForeignSymbolMap *foreign_symbol_map,
      semantic_analysis::TypeSystem *ts);
  void AppendBuiltinForeignPointer(core::Type t);

  void AppendNot();
  void AppendXor();

  void AppendZeroExtendSignedSigned(ZeroExtendOptions options);
  void AppendZeroExtendUnsignedSigned(ZeroExtendOptions options);
  void AppendZeroExtendUnsignedUnsigned(ZeroExtendOptions options);

  struct Consume;
  struct Append;
  template <typename Behavior, typename T>
  void AppendLessThan();

  template <typename Behavior, typename T>
  void AppendEqual();

  template <char Op, typename T>
  void AppendBinary();

  template <typename T>
  void AppendNegate();

  template <typename T>
  void AppendConstruct(std::type_identity_t<T> value);

  void AppendIncrementPointer(size_t amount);
  void AppendInvokeForeignFunction();

  void AppendMakeSliceType();
  void AppendMakePointerType();
  void AppendMakeBufferPointerType();
  void AppendBeginParameterType();
  void AppendNamedParameter();
  void AppendAnonymousParameter();
  void AppendEndParameterType(semantic_analysis::TypeSystem *ts);
  void AppendEndFunctionType(semantic_analysis::TypeSystem *ts, size_t count);

  jasmin::OpCodeRange AppendJumpWithPlaceholders();
  jasmin::OpCodeRange AppendJumpIfWithPlaceholders();

  // TODO: Should this be made more private?
  char const *raw() const { return data_; }
  char *raw() { return data_; }

 private:
  alignas(jasmin::internal::FunctionBase) char data_[sizeof(
      jasmin::internal::FunctionBase)];
};

extern template void Function::AppendBinary<'+', int8_t>();
extern template void Function::AppendBinary<'+', int16_t>();
extern template void Function::AppendBinary<'+', int32_t>();
extern template void Function::AppendBinary<'+', int64_t>();
extern template void Function::AppendBinary<'+', uint8_t>();
extern template void Function::AppendBinary<'+', uint16_t>();
extern template void Function::AppendBinary<'+', uint32_t>();
extern template void Function::AppendBinary<'+', uint64_t>();
extern template void Function::AppendBinary<'+', float>();
extern template void Function::AppendBinary<'+', double>();
extern template void Function::AppendBinary<'-', int8_t>();
extern template void Function::AppendBinary<'-', int16_t>();
extern template void Function::AppendBinary<'-', int32_t>();
extern template void Function::AppendBinary<'-', int64_t>();
extern template void Function::AppendBinary<'-', uint8_t>();
extern template void Function::AppendBinary<'-', uint16_t>();
extern template void Function::AppendBinary<'-', uint32_t>();
extern template void Function::AppendBinary<'-', uint64_t>();
extern template void Function::AppendBinary<'-', float>();
extern template void Function::AppendBinary<'-', double>();
extern template void Function::AppendBinary<'*', int8_t>();
extern template void Function::AppendBinary<'*', int16_t>();
extern template void Function::AppendBinary<'*', int32_t>();
extern template void Function::AppendBinary<'*', int64_t>();
extern template void Function::AppendBinary<'*', uint8_t>();
extern template void Function::AppendBinary<'*', uint16_t>();
extern template void Function::AppendBinary<'*', uint32_t>();
extern template void Function::AppendBinary<'*', uint64_t>();
extern template void Function::AppendBinary<'*', float>();
extern template void Function::AppendBinary<'*', double>();
extern template void Function::AppendBinary<'/', int8_t>();
extern template void Function::AppendBinary<'/', int16_t>();
extern template void Function::AppendBinary<'/', int32_t>();
extern template void Function::AppendBinary<'/', int64_t>();
extern template void Function::AppendBinary<'/', uint8_t>();
extern template void Function::AppendBinary<'/', uint16_t>();
extern template void Function::AppendBinary<'/', uint32_t>();
extern template void Function::AppendBinary<'/', uint64_t>();
extern template void Function::AppendBinary<'/', float>();
extern template void Function::AppendBinary<'/', double>();
extern template void Function::AppendBinary<'%', int8_t>();
extern template void Function::AppendBinary<'%', int16_t>();
extern template void Function::AppendBinary<'%', int32_t>();
extern template void Function::AppendBinary<'%', int64_t>();
extern template void Function::AppendBinary<'%', uint8_t>();
extern template void Function::AppendBinary<'%', uint16_t>();
extern template void Function::AppendBinary<'%', uint32_t>();
extern template void Function::AppendBinary<'%', uint64_t>();

extern template void Function::AppendNegate<int8_t>();
extern template void Function::AppendNegate<int16_t>();
extern template void Function::AppendNegate<int32_t>();
extern template void Function::AppendNegate<int64_t>();
extern template void Function::AppendNegate<float>();
extern template void Function::AppendNegate<double>();
extern template void Function::AppendNegate<data_types::IntegerHandle>();

extern template void Function::AppendConstruct<bool>(bool value);
extern template void Function::AppendConstruct<data_types::Char>(
    data_types::Char value);
extern template void Function::AppendConstruct<int8_t>(int8_t value);
extern template void Function::AppendConstruct<int16_t>(int16_t value);
extern template void Function::AppendConstruct<int32_t>(int32_t value);
extern template void Function::AppendConstruct<int64_t>(int64_t value);
extern template void Function::AppendConstruct<uint8_t>(uint8_t value);
extern template void Function::AppendConstruct<uint16_t>(uint16_t value);
extern template void Function::AppendConstruct<uint32_t>(uint32_t value);
extern template void Function::AppendConstruct<uint64_t>(uint64_t value);
extern template void Function::AppendConstruct<float>(float value);
extern template void Function::AppendConstruct<double>(double value);

extern template void Function::AppendLessThan<Function::Append, int8_t>();
extern template void Function::AppendLessThan<Function::Append, int16_t>();
extern template void Function::AppendLessThan<Function::Append, int32_t>();
extern template void Function::AppendLessThan<Function::Append, int64_t>();
extern template void Function::AppendLessThan<Function::Append, uint8_t>();
extern template void Function::AppendLessThan<Function::Append, uint16_t>();
extern template void Function::AppendLessThan<Function::Append, uint32_t>();
extern template void Function::AppendLessThan<Function::Append, uint64_t>();
extern template void Function::AppendLessThan<Function::Append, float>();
extern template void Function::AppendLessThan<Function::Append, double>();
extern template void Function::AppendLessThan<Function::Consume, int8_t>();
extern template void Function::AppendLessThan<Function::Consume, int16_t>();
extern template void Function::AppendLessThan<Function::Consume, int32_t>();
extern template void Function::AppendLessThan<Function::Consume, int64_t>();
extern template void Function::AppendLessThan<Function::Consume, uint8_t>();
extern template void Function::AppendLessThan<Function::Consume, uint16_t>();
extern template void Function::AppendLessThan<Function::Consume, uint32_t>();
extern template void Function::AppendLessThan<Function::Consume, uint64_t>();
extern template void Function::AppendLessThan<Function::Consume, float>();
extern template void Function::AppendLessThan<Function::Consume, double>();

extern template void Function::AppendEqual<Function::Append, int8_t>();
extern template void Function::AppendEqual<Function::Append, int16_t>();
extern template void Function::AppendEqual<Function::Append, int32_t>();
extern template void Function::AppendEqual<Function::Append, int64_t>();
extern template void Function::AppendEqual<Function::Append, uint8_t>();
extern template void Function::AppendEqual<Function::Append, uint16_t>();
extern template void Function::AppendEqual<Function::Append, uint32_t>();
extern template void Function::AppendEqual<Function::Append, uint64_t>();
extern template void Function::AppendEqual<Function::Append, float>();
extern template void Function::AppendEqual<Function::Append, double>();
extern template void Function::AppendEqual<Function::Consume, int8_t>();
extern template void Function::AppendEqual<Function::Consume, int16_t>();
extern template void Function::AppendEqual<Function::Consume, int32_t>();
extern template void Function::AppendEqual<Function::Consume, int64_t>();
extern template void Function::AppendEqual<Function::Consume, uint8_t>();
extern template void Function::AppendEqual<Function::Consume, uint16_t>();
extern template void Function::AppendEqual<Function::Consume, uint32_t>();
extern template void Function::AppendEqual<Function::Consume, uint64_t>();
extern template void Function::AppendEqual<Function::Consume, float>();
extern template void Function::AppendEqual<Function::Consume, double>();

}  // namespace vm

#endif  // ICARUS_VM_FUNCTION_H
