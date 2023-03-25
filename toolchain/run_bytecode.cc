#include <fstream>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "module/resources.h"
#include "toolchain/bazel.h"
#include "toolchain/flags.h"
#include "vm/argument_slice.h"
#include "vm/execute.h"

ABSL_FLAG(std::string, diagnostics, "console",
          "Indicates how diagnostics should be emitted. Options: console "
          "(default), or json.");
ABSL_FLAG(std::string, module_identifier, "",
          "Identifier to be used to uniquely identify this module amongst all "
          "modules being linked together, and must not begin with a tilde (~) "
          "character.");
ABSL_FLAG(std::string, module_map_file, "",
          "The path to the .icmodmap file describing the module map.");
ABSL_FLAG(std::string, input, "", "The path to the .icm file to be executed.");

namespace toolchain {

bool HelpFilter(std::string_view module) { return true; }

bool Execute(serialization::UniqueModuleId module_id,
             std::string const &input_file, std::string const &module_map_file,
             std::span<std::string_view const> arguments) {
  auto specification = BazelModuleMap(module_map_file);
  if (not specification) {
    // TODO log an error
    std::cerr << "invalid spec.\n";
    return false;
  }
  auto name_resolver = BazelNameResolver(std::move(specification->names));
  ASSERT(name_resolver != nullptr);

  frontend::SourceIndexer source_indexer;
  auto diagnostic_consumer =
      toolchain::DiagnosticConsumerFromFlag(FLAGS_diagnostics, source_indexer);
  if (not diagnostic_consumer.ok()) {
    std::cerr << diagnostic_consumer.status().message();
    return false;
  }

  module::Resources resources(std::move(module_id), std::move(name_resolver),
                              std::move(*diagnostic_consumer));

  std::vector<std::pair<serialization::Module, module::Module *>> modules;
  for (auto const &[id, path] : specification->paths) {
    std::ifstream stream(path);

    if (not stream.is_open()) {
      std::cerr << "failed to open " << id.value() << " (" << path << ").\n";
      return false;
    }

    auto &[proto, mptr] = modules.emplace_back();
    if (not proto.ParseFromIstream(&stream)) {
      std::cerr << "failed to parse " << id.value() << " (" << path << ").\n";
      return false;
    }
    auto index = serialization::ModuleIndex(resources.imported_modules());
    mptr = &resources.AllocateModule(id);

    resources.module_map().insert(serialization::ModuleIndex::Self(), index,
                                  id);
    if (not module::GlobalModuleMap::Deserialize(index, proto.module_map(),
                                                 resources.module_map())) {
      // TODO Log an error.
      std::cerr << "failed to load module " << id.value() << " (" << path
                << ").\n";
      return false;
    }
  }

  size_t i = 0;
  for (auto const &[proto, mptr] : modules) {
    serialization::ModuleIndex index(i);
    if (not module::Module::DeserializeInto(
            proto, resources.modules(), index, resources.module(index),
            resources.primary_module().type_system(),
            resources.unique_type_table(), resources.module_map(),
            resources.function_map(), resources.opaque_map())) {
      // TODO Log an error.
      std::cerr << "failed to load module.";
      return false;
    }
    ++i;
  }

  std::ifstream stream(input_file);
  if (not stream.is_open()) {
    std::cerr << "Failed to open '" << input_file << "'.\n";
    return false;
  }

  serialization::Module proto;
  if (not proto.ParseFromIstream(&stream)) {
    std::cerr << "Invalid module.\n";
    return false;
  }

  if (not module::Module::DeserializeInto(
          proto, resources.modules(), serialization::ModuleIndex::Self(),
          resources.primary_module(), resources.primary_module().type_system(),
          resources.unique_type_table(), resources.module_map(),
          resources.function_map(), resources.opaque_map())) {
    // TODO Log an error.
    std::cerr << "failed to load module.";
    return false;
  }

  vm::ArgumentSlice argument_slice(
      const_cast<std::string_view *>(arguments.data()), arguments.size());

  for (auto const *module : resources.modules()) {
    jasmin::ValueStack value_stack;
    data_types::IntegerTable table;
    vm::Execute(
        module->initializer(),
        vm::ExecutionState{table, resources.primary_module().type_system(),
                           argument_slice},
        value_stack);
  }

  jasmin::ValueStack value_stack;
  data_types::IntegerTable table;
  vm::Execute(
      resources.primary_module().initializer(),
      vm::ExecutionState{table, resources.primary_module().type_system(),
                         argument_slice},
      value_stack);
  return true;
}

}  // namespace toolchain

int main(int argc, char *argv[]) {
  toolchain::InitializeFlags("Icarus Bytecode Runner", toolchain::HelpFilter);
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);

  std::vector<std::string_view> arguments;
  arguments.reserve(args.size() - 1);
  for (auto iter = std::next(args.begin()); iter != args.end(); ++iter) {
    arguments.push_back(*iter);
  }

  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  bool success = toolchain::Execute(
      serialization::UniqueModuleId(absl::GetFlag(FLAGS_module_identifier)),
      absl::GetFlag(FLAGS_input), absl::GetFlag(FLAGS_module_map_file),
      arguments);
  return success ? 0 : 1;
}
