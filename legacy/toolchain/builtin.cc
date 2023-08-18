#include <cstdlib>
#include <fstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "core/type_system/type.h"
#include "module/global_function_map.h"
#include "module/type_system.h"
#include "nth/commandline/commandline.h"
#include "nth/io/file_path.h"
#include "semantic_analysis/type_system.h"
#include "serialization/module.pb.h"
#include "vm/function_table.h"
#include "vm/serialization.h"

namespace toolchain {

void SerializeType(core::Type t, serialization::Type &proto,
                   bool inline_storage) {
  proto.set_category(t.category());
  if (inline_storage) {
    proto.set_value(t.index());
  } else {
    proto.set_index(t.index());
  }
}

serialization::Module BuiltinModule() {
  semantic_analysis::TypeSystem ts;
  module::GlobalFunctionMap fn_map;
  serialization::ForeignSymbolMap foreign_map(&ts);
  serialization::ReadOnlyData rodata;

  vm::FunctionTable table(fn_map);

  serialization::Module module;
  auto &exported       = *module.mutable_exported();
  auto &function_table = *module.mutable_function_table();
  {
    auto &symbol = *exported["ascii_decode"].add_symbols();
    core::FunctionType t(
        ts,
        core::ParameterType(
            ts,
            core::Parameters<core::Type>{{.value = semantic_analysis::Char}}),
        {semantic_analysis::U(8)});
    SerializeType(t, *symbol.mutable_symbol_type(), false);

    auto [index, fn] = table.emplace(1, 1, module::UniqueId::Builtin());
    symbol.mutable_function()->set_index(index.value());
    fn->AppendBuiltinAsciiDecode();
    fn->AppendReturn();
  }

  {
    auto &symbol = *exported["ascii_encode"].add_symbols();
    core::FunctionType t(
        ts,
        core::ParameterType(
            ts,
            core::Parameters<core::Type>{{.value = semantic_analysis::U(8)}}),
        {semantic_analysis::Char});
    SerializeType(t, *symbol.mutable_symbol_type(), false);

    auto [index, fn] = table.emplace(1, 1, module::UniqueId::Builtin());
    symbol.mutable_function()->set_index(index.value());
    fn->AppendBuiltinAsciiEncode();
    fn->AppendReturn();
  }

  {
    auto &symbol = *exported["arguments"].add_symbols();
    core::FunctionType t(
        ts, core::ParameterType(ts, {}),
        {semantic_analysis::SliceType(
            ts, semantic_analysis::SliceType(ts, semantic_analysis::Char))});

    SerializeType(t, *symbol.mutable_symbol_type(), false);

    auto [index, fn] = table.emplace(0, 2, module::UniqueId::Builtin());
    symbol.mutable_function()->set_index(index.value());
    fn->AppendBuiltinArguments();
    fn->AppendReturn();
  }

  {
    auto &symbol = *exported["opaque"].add_symbols();
    core::FunctionType t(
        ts, core::ParameterType(ts, core::Parameters<core::Type>{}),
        {semantic_analysis::Type});
    SerializeType(t, *symbol.mutable_symbol_type(), false);

    auto [index, fn] = table.emplace(0, 1, module::UniqueId::Builtin());
    symbol.mutable_function()->set_index(index.value());
    fn->AppendBuiltinOpaque();
    fn->AppendReturn();
  }

  {
    auto &symbol = *exported["slice"].add_symbols();
    core::FunctionType t(
        ts,
        core::ParameterType(ts,
                            core::Parameters<core::Type>{
                                {.value = semantic_analysis::BufferPointerType(
                                     ts, semantic_analysis::Char)},
                                {.value = semantic_analysis::U(64)}}),
        {semantic_analysis::SliceType(ts, semantic_analysis::Char)});
    SerializeType(t, *symbol.mutable_symbol_type(), false);

    // From the perspective of Jasmin this function has two inputs and two
    // returns, though the function itself only has one return in Icarus: A
    // slice.
    auto [index, fn] = table.emplace(2, 2, module::UniqueId::Builtin());
    symbol.mutable_function()->set_index(index.value());
    fn->AppendReturn();
  }

  serialization::UniqueTypeTable unique_type_table;

  vm::SerializationState state(rodata, foreign_map, module::UniqueId::Self(),
                               fn_map);
  vm::Serialize(table, *module.mutable_function_table(), state);
  module::SerializeTypeSystem(ts, unique_type_table,
                              *module.mutable_type_system());

  vm::Function initializer(0, 0);
  initializer.AppendReturn();
  vm::Serialize(initializer, *module.mutable_initializer(), state);

  return module;
}

nth::exit_code Main(nth::FlagValueSet flags, nth::file_path const &output) {
  std::ofstream output_stream(output.path(), std::ofstream::out);
  return BuiltinModule().SerializeToOstream(&output_stream)
             ? nth::exit_code::success
             : nth::exit_code::generic_error;
}

}  // namespace toolchain

nth::Usage const nth::program_usage = {
    .description = "Icarus Builtin Module Compiler",
    .execute     = toolchain::Main,
};
