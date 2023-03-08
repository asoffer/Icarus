#include "vm/function.h"

#include "vm/implementation.h"

namespace vm {

Function::Function(uint8_t parameter_count, uint8_t return_count) {
  new (data_) internal::IrFunction(parameter_count, return_count);
}

Function::Function(Function const &f) {
  new (data_) internal::IrFunction(internal::Impl(f.data_));
}

Function::Function(Function &&f) {
  new (data_) internal::IrFunction(std::move(internal::Impl(f.data_)));
}

Function &Function::operator=(Function const &f) {
  internal::Impl(data_) = internal::Impl(f.data_);
  return *this;
}

Function &Function::operator=(Function &&f) {
  internal::Impl(data_) = std::move(internal::Impl(f.data_));
  return *this;
}

Function::~Function() {
  using IrFunction = internal::IrFunction;
  internal::Impl(data_).~IrFunction();
}

uint8_t Function::parameter_count() const {
  return internal::Impl(data_).parameter_count();
}
uint8_t Function::return_count() const {
  return internal::Impl(data_).return_count();
}

std::span<jasmin::Value const> Function::raw_instructions() const {
  return internal::Impl(data_).raw_instructions();
}
std::span<jasmin::Value> Function::raw_instructions() {
  return internal::Impl(data_).raw_instructions();
}

void Function::set_value(jasmin::OpCodeRange range, size_t index,
                         jasmin::Value value) {
  internal::Impl(data_).set_value(range, index, value);
}

void Function::AppendCall() { internal::Impl(data_).append<jasmin::Call>(); }
void Function::AppendReturn() {
  internal::Impl(data_).append<jasmin::Return>();
}
void Function::AppendDrop(size_t n) {
  if (n != 0) { internal::Impl(data_).append<jasmin::Drop>(n); }
}
void Function::AppendSwap() { internal::Impl(data_).append<jasmin::Swap>(); }
void Function::AppendPush(jasmin::Value v) {
  internal::Impl(data_).append<jasmin::Push>(v);
}
void Function::AppendPushFunction(jasmin::Value v) {
  internal::Impl(data_).append<PushFunction>(v);
}
void Function::AppendPushStringLiteral(std::string_view s) {
  internal::Impl(data_).append<PushStringLiteral>(s.data(), s.size());
}

void Function::AppendDuplicate() {
  internal::Impl(data_).append<jasmin::Duplicate>();
}
void Function::AppendDuplicateAt(size_t n) {
  internal::Impl(data_).append<jasmin::DuplicateAt>(n);
}

void Function::AppendXor() { internal::Impl(data_).append<jasmin::Xor>(); }
void Function::AppendNot() { internal::Impl(data_).append<jasmin::Not>(); }

template <char Op, typename T>
void Function::AppendBinary() {
  if constexpr (Op == '+') {
    internal::Impl(data_).append<jasmin::Add<T>>();
  } else if constexpr (Op == '-') {
    internal::Impl(data_).append<jasmin::Subtract<T>>();
  } else if constexpr (Op == '*') {
    internal::Impl(data_).append<jasmin::Multiply<T>>();
  } else if constexpr (Op == '/') {
    internal::Impl(data_).append<jasmin::Divide<T>>();
  } else if constexpr (Op == '%') {
    internal::Impl(data_).append<jasmin::Mod<T>>();
  }
}

template void Function::AppendBinary<'+', int8_t>();
template void Function::AppendBinary<'+', int16_t>();
template void Function::AppendBinary<'+', int32_t>();
template void Function::AppendBinary<'+', int64_t>();
template void Function::AppendBinary<'+', uint8_t>();
template void Function::AppendBinary<'+', uint16_t>();
template void Function::AppendBinary<'+', uint32_t>();
template void Function::AppendBinary<'+', uint64_t>();
template void Function::AppendBinary<'+', float>();
template void Function::AppendBinary<'+', double>();
template void Function::AppendBinary<'-', int8_t>();
template void Function::AppendBinary<'-', int16_t>();
template void Function::AppendBinary<'-', int32_t>();
template void Function::AppendBinary<'-', int64_t>();
template void Function::AppendBinary<'-', uint8_t>();
template void Function::AppendBinary<'-', uint16_t>();
template void Function::AppendBinary<'-', uint32_t>();
template void Function::AppendBinary<'-', uint64_t>();
template void Function::AppendBinary<'-', float>();
template void Function::AppendBinary<'-', double>();
template void Function::AppendBinary<'*', int8_t>();
template void Function::AppendBinary<'*', int16_t>();
template void Function::AppendBinary<'*', int32_t>();
template void Function::AppendBinary<'*', int64_t>();
template void Function::AppendBinary<'*', uint8_t>();
template void Function::AppendBinary<'*', uint16_t>();
template void Function::AppendBinary<'*', uint32_t>();
template void Function::AppendBinary<'*', uint64_t>();
template void Function::AppendBinary<'*', float>();
template void Function::AppendBinary<'*', double>();
template void Function::AppendBinary<'/', int8_t>();
template void Function::AppendBinary<'/', int16_t>();
template void Function::AppendBinary<'/', int32_t>();
template void Function::AppendBinary<'/', int64_t>();
template void Function::AppendBinary<'/', uint8_t>();
template void Function::AppendBinary<'/', uint16_t>();
template void Function::AppendBinary<'/', uint32_t>();
template void Function::AppendBinary<'/', uint64_t>();
template void Function::AppendBinary<'/', float>();
template void Function::AppendBinary<'/', double>();
template void Function::AppendBinary<'%', int8_t>();
template void Function::AppendBinary<'%', int16_t>();
template void Function::AppendBinary<'%', int32_t>();
template void Function::AppendBinary<'%', int64_t>();
template void Function::AppendBinary<'%', uint8_t>();
template void Function::AppendBinary<'%', uint16_t>();
template void Function::AppendBinary<'%', uint32_t>();
template void Function::AppendBinary<'%', uint64_t>();

template <typename T>
void Function::AppendNegate() {
  if constexpr (nth::type<T> == nth::type<data_types::IntegerHandle>) {
    internal::Impl(data_).append<data_types::IntegerHandle::Negate>();
  } else {
    internal::Impl(data_).append<jasmin::Negate<T>>();
  }
}

template void Function::AppendNegate<int8_t>();
template void Function::AppendNegate<int16_t>();
template void Function::AppendNegate<int32_t>();
template void Function::AppendNegate<int64_t>();
template void Function::AppendNegate<float>();
template void Function::AppendNegate<double>();
template void Function::AppendNegate<data_types::IntegerHandle>();

template <typename T>
void Function::AppendConstruct(std::type_identity_t<T> value) {
  internal::Impl(data_).append<Construct<T>>(value);
}

template void Function::AppendConstruct<bool>(bool value);
template void Function::AppendConstruct<data_types::Char>(data_types::Char value);
template void Function::AppendConstruct<int8_t>(int8_t value);
template void Function::AppendConstruct<int16_t>(int16_t value);
template void Function::AppendConstruct<int32_t>(int32_t value);
template void Function::AppendConstruct<int64_t>(int64_t value);
template void Function::AppendConstruct<uint8_t>(uint8_t value);
template void Function::AppendConstruct<uint16_t>(uint16_t value);
template void Function::AppendConstruct<uint32_t>(uint32_t value);
template void Function::AppendConstruct<uint64_t>(uint64_t value);
template void Function::AppendConstruct<float>(float value);
template void Function::AppendConstruct<double>(double value);

void Function::AppendLoad(uint8_t bytes) {
  internal::Impl(data_).append<jasmin::Load>(bytes);
}

void Function::AppendStore(uint8_t bytes) {
  internal::Impl(data_).append<jasmin::Store>(bytes);
}

void Function::AppendStackAllocate(size_t bytes) {
  internal::Impl(data_).append<jasmin::StackAllocate>(bytes);
}

void Function::AppendStackOffset(size_t bytes) {
  internal::Impl(data_).append<jasmin::StackOffset>(bytes);
}

void Function::AppendMakeSliceType() {
  internal::Impl(data_)
      .append<
          semantic_analysis::TypeSystem::Make<semantic_analysis::SliceType>>();
}
void Function::AppendMakePointerType() {
  internal::Impl(data_)
      .append<semantic_analysis::TypeSystem::Make<core::PointerType>>();
}
void Function::AppendMakeBufferPointerType() {
  internal::Impl(data_)
      .append<semantic_analysis::TypeSystem::Make<
          semantic_analysis::BufferPointerType>>();
}
void Function::AppendBeginParameterType() {
  internal::Impl(data_).append<core::ParameterType::Begin>();
}
void Function::AppendNamedParameter() {
  internal::Impl(data_).append<core::ParameterType::AppendNamed>();
}
void Function::AppendAnonymousParameter() {
  internal::Impl(data_).append<core::ParameterType::Append>();
}
void Function::AppendEndParameterType(semantic_analysis::TypeSystem *ts) {
  internal::Impl(data_)
      .append<core::ParameterType::End<semantic_analysis::TypeSystem>>(ts);
}
void Function::AppendEndFunctionType(semantic_analysis::TypeSystem *ts,
                                     size_t count) {
  internal::Impl(data_)
      .append<core::FunctionType::End<semantic_analysis::TypeSystem>>(ts,
                                                                      count);
}

void Function::AppendIncrementPointer(size_t amount) {
  internal::Impl(data_).append<IncrementPointer>(amount);
}

void Function::AppendBuiltinForeignFunction(
    core::Type t, void *raw_table,
    serialization::ForeignSymbolMap *foreign_symbol_map,
    semantic_analysis::TypeSystem *ts) {
  internal::Impl(data_).append<BuiltinForeignFunction>(
      t, raw_table, foreign_symbol_map, ts);
}

void Function::AppendBuiltinForeignPointer(core::Type t) {
  internal::Impl(data_).append<BuiltinForeignPointer>(t);
}

void Function::AppendInvokeForeignFunction(void (*fn_ptr)(),
                                           core::FunctionType t) {
  auto const &parameters = t.parameters();
  std::span returns      = t.returns();
  internal::Impl(data_).append<InvokeForeignFunction>(
      fn_ptr, parameters.data(), parameters.size(),
      returns.empty() ? nullptr : returns.data());
}

template <typename Behavior, typename T>
void Function::AppendLessThan() {
  if constexpr (nth::type<Behavior> == nth::type<Function::Append>) {
    internal::Impl(data_).append<jasmin::AppendLessThan<T>>();
  } else {
    internal::Impl(data_).append<jasmin::LessThan<T>>();
  }
}

template void Function::AppendLessThan<Function::Append, int8_t>();
template void Function::AppendLessThan<Function::Append, int16_t>();
template void Function::AppendLessThan<Function::Append, int32_t>();
template void Function::AppendLessThan<Function::Append, int64_t>();
template void Function::AppendLessThan<Function::Append, uint8_t>();
template void Function::AppendLessThan<Function::Append, uint16_t>();
template void Function::AppendLessThan<Function::Append, uint32_t>();
template void Function::AppendLessThan<Function::Append, uint64_t>();
template void Function::AppendLessThan<Function::Append, float>();
template void Function::AppendLessThan<Function::Append, double>();
template void Function::AppendLessThan<Function::Consume, int8_t>();
template void Function::AppendLessThan<Function::Consume, int16_t>();
template void Function::AppendLessThan<Function::Consume, int32_t>();
template void Function::AppendLessThan<Function::Consume, int64_t>();
template void Function::AppendLessThan<Function::Consume, uint8_t>();
template void Function::AppendLessThan<Function::Consume, uint16_t>();
template void Function::AppendLessThan<Function::Consume, uint32_t>();
template void Function::AppendLessThan<Function::Consume, uint64_t>();
template void Function::AppendLessThan<Function::Consume, float>();
template void Function::AppendLessThan<Function::Consume, double>();

template <typename Behavior, typename T>
void Function::AppendEqual() {
  if constexpr (nth::type<Behavior> == nth::type<Function::Append>) {
    internal::Impl(data_).append<jasmin::AppendEqual<T>>();
  } else {
    internal::Impl(data_).append<jasmin::Equal<T>>();
  }
}

template void Function::AppendEqual<Function::Append, int8_t>();
template void Function::AppendEqual<Function::Append, int16_t>();
template void Function::AppendEqual<Function::Append, int32_t>();
template void Function::AppendEqual<Function::Append, int64_t>();
template void Function::AppendEqual<Function::Append, uint8_t>();
template void Function::AppendEqual<Function::Append, uint16_t>();
template void Function::AppendEqual<Function::Append, uint32_t>();
template void Function::AppendEqual<Function::Append, uint64_t>();
template void Function::AppendEqual<Function::Append, float>();
template void Function::AppendEqual<Function::Append, double>();
template void Function::AppendEqual<Function::Consume, int8_t>();
template void Function::AppendEqual<Function::Consume, int16_t>();
template void Function::AppendEqual<Function::Consume, int32_t>();
template void Function::AppendEqual<Function::Consume, int64_t>();
template void Function::AppendEqual<Function::Consume, uint8_t>();
template void Function::AppendEqual<Function::Consume, uint16_t>();
template void Function::AppendEqual<Function::Consume, uint32_t>();
template void Function::AppendEqual<Function::Consume, uint64_t>();
template void Function::AppendEqual<Function::Consume, float>();
template void Function::AppendEqual<Function::Consume, double>();

void Function::AppendZeroExtendSignedSigned(
    ZeroExtendOptions options) {
  internal::Impl(data_).append<ZeroExtend<true, true>>(options);
}
void Function::AppendZeroExtendUnsignedSigned(
    ZeroExtendOptions options) {
  internal::Impl(data_).append<ZeroExtend<false, true>>(options);
}
void Function::AppendZeroExtendUnsignedUnsigned(ZeroExtendOptions options) {
  internal::Impl(data_).append<ZeroExtend<false, false>>(
      options);
}

jasmin::OpCodeRange Function::AppendJumpWithPlaceholders() {
  return internal::Impl(data_).append_with_placeholders<jasmin::Jump>();
}
jasmin::OpCodeRange Function::AppendJumpIfWithPlaceholders() {
  return internal::Impl(data_).append_with_placeholders<jasmin::JumpIf>();
}

}  // namespace vm
