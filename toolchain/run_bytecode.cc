#include <fstream>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include "diagnostic/consumer/streaming.h"
#include "frontend/source_indexer.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "module/module_map.h"
#include "module/resources.h"
#include "module/serialize.h"
#include "nth/commandline/commandline.h"
#include "nth/process/exit_code.h"
#include "vm/argument_slice.h"
#include "vm/execute.h"

namespace toolchain {

nth::exit_code Execute(nth::FlagValueSet flags,
                       std::span<std::string_view const> arguments) {
  auto const *input           = flags.get<nth::file_path>("input");
  auto const *module_map_file = flags.get<nth::file_path>("module-map");
  auto const *id = flags.get<module::UniqueId>("module-identifier");

  if (not input) { return nth::exit_code::usage; }
  if (not module_map_file) { return nth::exit_code::usage; }
  if (not id) { return nth::exit_code::usage; }

  std::optional module_map = module::ModuleMap::Load(*module_map_file);
  if (not module_map) {
    // TODO: log an error
    return nth::exit_code::generic_error;
  }

  frontend::SourceIndexer source_indexer;
  diagnostic::StreamingConsumer diagnostic_consumer(stderr, &source_indexer);

  module::Resources resources(std::move(*id), *module_map, diagnostic_consumer);

  std::ifstream stream(input->path());
  if (not stream.is_open()) {
    std::cerr << "Failed to open '" << input->path() << "'.\n";
    return nth::exit_code::generic_error;
  }

  serialization::Module proto;
  if (not proto.ParseFromIstream(&stream)) {
    std::cerr << "Invalid module.\n";
    return nth::exit_code::generic_error;
  }

  if (not module::DeserializeModuleInto(
          proto, resources.modules(), module::UniqueId::Self(),
          resources.primary_module(), resources.primary_module().type_system(),
          resources.unique_type_table(), *module_map,
          resources.function_map())) {
    // TODO Log an error.
    std::cerr << "failed to load module.";
    return nth::exit_code::generic_error;
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
  return nth::exit_code::success;
}

}  // namespace toolchain

nth::Usage const nth::program_usage = {
    .description = "Icarus Bytecode Runner",
    .flags =
        {
            {
                .name = {"module-identifier"},
                .type = nth::type<module::UniqueId>,
                .description =
                    "The path to the .icmodmap file describing the module map.",

            },
            {
                .name = {"module-map"},
                .type = nth::type<nth::file_path>,
                .description =
                    "The path to the .icmodmap file describing the module map.",

            },
        },
    .execute = toolchain::Execute,
};
