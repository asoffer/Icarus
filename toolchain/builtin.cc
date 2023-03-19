#include <cstdlib>
#include <fstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "core/type_system/type.h"
#include "module/global_function_map.h"
#include "module/type_system.h"
#include "semantic_analysis/type_system.h"
#include "serialization/module.pb.h"
#include "toolchain/flags.h"
#include "vm/function_table.h"
#include "vm/serialization.h"

ABSL_FLAG(std::string, output, "",
          "The location at which to write the output .icm file.");

namespace toolchain {

bool HelpFilter(std::string_view module) { return true; }

void ValidateOutputPath(std::string_view output) {
  if (output.empty()) {
    std::cerr << "--output must not be empty.";
    std::exit(1);
  }
}

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
  module::GlobalModuleMap mod_map;
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

    auto [index, fn] = table.emplace(1, 1);
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

    auto [index, fn] = table.emplace(1, 1);
    symbol.mutable_function()->set_index(index.value());
    fn->AppendBuiltinAsciiEncode();
    fn->AppendReturn();
  }

  {
    auto &symbol = *exported["opaque"].add_symbols();
    core::FunctionType t(
        ts, core::ParameterType(ts, core::Parameters<core::Type>{}),
        {semantic_analysis::Type});
    SerializeType(t, *symbol.mutable_symbol_type(), false);

    auto [index, fn] = table.emplace(0, 1);
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
    auto [index, fn] = table.emplace(2, 2);
    symbol.mutable_function()->set_index(index.value());
    fn->AppendReturn();
  }

  serialization::UniqueTypeTable unique_type_table;

  vm::SerializationState state(rodata, foreign_map,
                               serialization::ModuleIndex::Self(), mod_map,
                               fn_map);
  vm::Serialize(table, *module.mutable_function_table(), state);
  module::SerializeTypeSystem(ts, unique_type_table,
                              *module.mutable_type_system());
  return module;
}

}  // namespace toolchain

int main(int argc, char *argv[]) {
  toolchain::InitializeFlags("Icarus Builtin Module Compiler",
                             toolchain::HelpFilter);
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);

  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  std::string output = absl::GetFlag(FLAGS_output);
  toolchain::ValidateOutputPath(output);

  auto builtin = toolchain::BuiltinModule();
  std::ofstream output_stream(output.c_str(), std::ofstream::out);
  return builtin.SerializeToOstream(&output_stream) ? 0 : 1;
}
