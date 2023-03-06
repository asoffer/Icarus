#include "semantic_analysis/instruction_set.h"

#include <ffi.h>

#include "absl/container/inlined_vector.h"
#include "data_types/fn.h"
#include "jasmin/value.h"
#include "serialization/function_table.h"

namespace semantic_analysis {
namespace {

ffi_type* FfiType(core::Type t) {
  if (t == Char) {
    return std::is_signed_v<char> ? &ffi_type_schar : &ffi_type_uchar;
  }
  if (t == I(8)) { return &ffi_type_sint8; }
  if (t == I(16)) { return &ffi_type_sint16; }
  if (t == I(32)) { return &ffi_type_sint32; }
  if (t == I(64)) { return &ffi_type_sint64; }
  if (t == U(8)) { return &ffi_type_uint8; }
  if (t == U(16)) { return &ffi_type_uint16; }
  if (t == U(32)) { return &ffi_type_uint32; }
  if (t == U(64)) { return &ffi_type_uint64; }
  if (t == F32) { return &ffi_type_float; }
  if (t == F64) { return &ffi_type_double; }
  return &ffi_type_pointer;
}

template <jasmin::SmallTrivialValue T>
T Read(ffi_arg const& value) {
  T result;
  std::memcpy(&result, &value, sizeof(T));
  return result;
}

}  // namespace

void BuiltinForeignFunction::execute(
    jasmin::ValueStack& value_stack, core::Type t, void* raw_table,
    serialization::ForeignSymbolMap* foreign_symbol_map, TypeSystem* ts) {
  auto fn_type     = t.get<core::FunctionType>(*ts);
  size_t length    = value_stack.pop<size_t>();
  char const* data = value_stack.pop<char const*>();
  std::string name(data, length);

  auto& table = *static_cast<
      serialization::FunctionTable<jasmin::Function<InstructionSet>>*>(
      raw_table);

  auto [index, inserted] = foreign_symbol_map->insert({
      .type = fn_type,
      .name = std::string(data, length),
  });

  auto const& parameters = fn_type.parameters();
  std::span returns      = fn_type.returns();
  auto [fn_index, f]     = table.emplace(parameters.size(), returns.size());
  ASSERT(returns.size() <= 1);
  f->append<InvokeForeignFunction>(foreign_symbol_map->function(index),
                                   parameters.data(), parameters.size(),
                                   returns.empty() ? nullptr : returns.data());
  f->append<jasmin::Return>();
  value_stack.push(f);
}

void BuiltinForeignPointer::execute(
    jasmin::ValueStack& value_stack,
    BuiltinForeignPointer::JasminExecutionState& foreign_symbol_map,
    core::Type type) {
  size_t length          = value_stack.pop<size_t>();
  char const* data       = value_stack.pop<char const*>();
  auto [index, inserted] = foreign_symbol_map.insert({
      .type = type,
      .name = std::string(data, length),
  });

  value_stack.push(foreign_symbol_map.pointer(index));
}

void InvokeForeignFunction::execute(
    jasmin::ValueStack& value_stack, void (*fn_ptr)(),
    core::Parameter<core::Type> const* parameters, size_t parameter_count,
    core::Type const* maybe_return_type) {
  ffi_cif call_interface;

  ffi_type* return_type =
      maybe_return_type ? FfiType(*maybe_return_type) : &ffi_type_void;

  absl::InlinedVector<ffi_type*, 4> argument_types;
  absl::InlinedVector<void*, 4> argument_values;
  argument_types.reserve(parameter_count);
  argument_values.reserve(parameter_count);

  for (auto iter = value_stack.end() - parameter_count;
       iter != value_stack.end(); ++iter, ++parameters) {
    argument_types.push_back(FfiType(parameters->value));
    argument_values.push_back(const_cast<void*>(iter->address()));
  }

  ffi_status status =
      ffi_prep_cif(&call_interface, FFI_DEFAULT_ABI, argument_types.size(),
                   return_type, argument_types.data());
  ASSERT(status == FFI_OK);

  ffi_arg return_value;
  ffi_call(&call_interface, fn_ptr, &return_value, argument_values.data());

  for (size_t i = 0; i < parameter_count; ++i) { value_stack.pop_value(); }

  if (maybe_return_type) {
    core::Type t = *maybe_return_type;
    if (t == Char) {
      value_stack.push(Read<char>(return_value));
    } else if (t == I(8)) {
      value_stack.push(Read<int8_t>(return_value));
    } else if (t == I(16)) {
      value_stack.push(Read<int16_t>(return_value));
    } else if (t == I(32)) {
      value_stack.push(Read<int32_t>(return_value));
    } else if (t == I(64)) {
      value_stack.push(Read<int64_t>(return_value));
    } else if (t == U(8)) {
      value_stack.push(Read<uint8_t>(return_value));
    } else if (t == U(16)) {
      value_stack.push(Read<uint16_t>(return_value));
    } else if (t == U(32)) {
      value_stack.push(Read<uint32_t>(return_value));
    } else if (t == U(64)) {
      value_stack.push(Read<uint64_t>(return_value));
    } else if (t == F32) {
      value_stack.push(Read<float>(return_value));
    } else if (t == F64) {
      value_stack.push(Read<double>(return_value));
    } else {
      value_stack.push(Read<char const*>(return_value));
    }
  }
}

// 0: void (*fn_ptr)()
// 1: core::Parameter<core::Type> const* parameters
// 2: size_t parameter_count
// 3: core::Type const* maybe_return_type
//
//
// ForeignSymbol{type, name}, index, void(*)()
//
// void(*)() -> index
// type -> index
//
void InvokeForeignFunction::serialize(jasmin::Serializer& serializer,
                                      std::span<jasmin::Value const> values,
                                      serialization_state& state) {
  auto& ts = state.get<TypeSystem>();
  core::Parameters<core::Type> params;
  auto const* p   = values[1].as<core::Parameter<core::Type> const*>();
  auto const* end = p + values[2].as<size_t>();
  for (; p != end; ++p) { params.append(*p); }
  std::vector<core::Type> rets;
  if (auto const* maybe_return_type = values[3].as<core::Type const*>()) {
    rets.push_back(*maybe_return_type);
  }
  core::Type t = core::FunctionType(
      ts, core::ParameterType(ts, std::move(params)), std::move(rets));
  ASSERT(values.size() == 4);
  serializer(jasmin::Value(state.index(t, values[0].as<void (*)()>())));
}

bool InvokeForeignFunction::deserialize(jasmin::Deserializer& deserializer,
                                        std::span<jasmin::Value> values,
                                        serialization_state& state) {
  ASSERT(values.size() == 4);
  jasmin::Value index = jasmin::Value::Uninitialized();
  if (not deserializer(index)) { return false; }

  auto& type_system        = state.get<TypeSystem>();
  auto const& [type, name] = state.symbol(index.as<uint32_t>());
  auto function_type       = type.get<core::FunctionType>(type_system);

  values[0] = state.function(index.as<uint32_t>());
  values[1] = function_type.parameters().data();
  values[2] = function_type.parameters().size();
  values[3] =
      function_type.returns().empty() ? nullptr : &function_type.returns()[0];
  return true;
}

void PushStringLiteral::serialize(jasmin::Serializer& serializer,
                                  std::span<jasmin::Value const> values,
                                  serialization_state& state) {
  ASSERT(values.size() == 2);
  // TODO: Change this to string_view once nth::flyweight_set supports
  // heterogeneous lookup.
  auto [index, inserted] = state.insert(
      std::string(values[0].as<char const*>(), values[1].as<size_t>()));
  serializer(index);
}

bool PushStringLiteral::deserialize(jasmin::Deserializer& deserializer,
                                    std::span<jasmin::Value> values,
                                    serialization_state& state) {
  ASSERT(values.size() == 2);
  size_t index;
  if (not deserializer(index)) { return false; }
  std::string_view s = state.string(index);
  values[0]          = s.data();
  values[1]          = s.size();
  return true;
}

void PushFunction::execute(jasmin::ValueStack& value_stack,
                           jasmin::Value value) {
  value_stack.push(value);
}

void PushFunction::serialize(jasmin::Serializer& serializer,
                             std::span<jasmin::Value const> values,
                             serialization_state& state) {
  auto& fn_map = std::get<2>(state);
  ASSERT(values.size() == 1);
  // TODO: Make this void* more well-typed.
  auto* ir_fn                   = values[0].as<void*>();
  auto [module_index, fn_index] = fn_map.find(ir_fn);
  ASSERT(module_index != serialization::ModuleIndex::Invalid());
  ASSERT(fn_index != serialization::FunctionIndex::Invalid());
  serializer(module_index.value());
  serializer(fn_index.value());
}

bool PushFunction::deserialize(jasmin::Deserializer& deserializer,
                               std::span<jasmin::Value> values,
                               serialization_state& state) {
  auto& [current_index, module_map, fn_map] = state;
  ASSERT(values.size() == 1);
  serialization::ModuleIndex::underlying_type module_index;
  serialization::FunctionIndex::underlying_type function_index;
  if (not deserializer(module_index)) { return false; }
  if (not deserializer(function_index)) { return false; }
  auto index =
      module_map.read(current_index, serialization::ModuleIndex(module_index));
  auto* f = ASSERT_NOT_NULL(
      fn_map.find(index, serialization::FunctionIndex(function_index)));
  values[0] = f;
  return true;
}

void TranslateFunctionArguments::execute(
    jasmin::ValueStack& value_stack,
    core::Parameters<core::Type> const* parameters,
    serialization::ModuleIndex index) {
  std::vector<jasmin::Value> values;
  values.reserve(parameters->size());
  for (auto iter = parameters->rbegin(); iter != parameters->rend(); ++iter) {
    values.push_back(value_stack.pop_value());
    if (iter->value == Type) { NOT_YET(); }
  }

  for (auto iter = values.rbegin(); iter != values.rend(); ++iter) {
    value_stack.push(*iter);
  }
}

}  // namespace semantic_analysis
