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
#include "module/bazel_name_resolver.h"
#include "module/module.h"
#include "module/resources.h"
#include "semantic_analysis/instruction_set.h"
#include "toolchain/flags.h"

ABSL_FLAG(std::string, input, "", "The path to the .icm file to be executed.");
ABSL_FLAG(std::string, module_map_file, "",
          "The path to the .icmodmap file describing the module map.");

namespace toolchain {

bool HelpFilter(std::string_view module) { return true; }

bool Execute(std::string const &input_file, std::string const &module_map_file,
             std::span<std::string_view const> arguments) {
  std::ifstream stream(input_file);
  if (not stream.is_open()) {
    std::cerr << "Failed to open '" << input_file << "'.\n";
    return false;
  }

  auto name_resolver = module::BazelNameResolver(module_map_file);
  ASSERT(name_resolver != nullptr);
  module::Resources resources(std::move(name_resolver));

  serialization::Module proto;
  if (not proto.ParseFromIstream(&stream) or
      not module::Module::DeserializeInto(proto, resources.primary_module())) {
    std::cerr << "Invalid module.\n";
    return false;
  }

  jasmin::ValueStack value_stack;
  value_stack.push(arguments.data());
  value_stack.push(arguments.size());
  data_types::IntegerTable table;
  jasmin::Execute(
      resources.primary_module().initializer(),
      jasmin::ExecutionState<semantic_analysis::InstructionSet>{table},
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

  bool success =
      toolchain::Execute(absl::GetFlag(FLAGS_input),
                         absl::GetFlag(FLAGS_module_map_file), arguments);
  return success ? 0 : 1;
}
