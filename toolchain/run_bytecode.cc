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
#include "semantic_analysis/instruction_set.h"
#include "toolchain/flags.h"

ABSL_FLAG(std::string, input, "", "The path to the .icm file to be executed.");

namespace toolchain {

bool HelpFilter(std::string_view module) { return true; }

bool Execute(std::string const &input_file,
             std::span<std::string_view const> arguments) {
  std::ifstream stream(input_file);
  if (not stream.is_open()) {
    std::cerr << "Failed to open '" << input_file << "'.\n";
    return false;
  }
  std::optional module = module::Module::Deserialize(stream);
  if (not module) {
    std::cerr << "Invalid module.\n";
    return false;
  }

  jasmin::ValueStack value_stack;
  value_stack.push(arguments.data());
  value_stack.push(arguments.size());
  data_types::IntegerTable table;
  jasmin::Execute(
      module->initializer(),
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

  bool success = toolchain::Execute(absl::GetFlag(FLAGS_input), arguments);
  return success ? 0 : 1;
}
