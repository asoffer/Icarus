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
#include "nth/debug/log/log.h"
#include "nth/debug/log/stderr_log_sink.h"
#include "nth/io/file.h"
#include "nth/process/exit_code.h"
#include "vm/argument_slice.h"
#include "vm/execute.h"

namespace toolchain {

nth::exit_code Execute(nth::FlagValueSet flags,
                       std::span<std::string_view const> arguments) {
  nth::RegisterLogSink(nth::stderr_log_sink);
  auto const &input           = flags.get<nth::file_path>("input");
  auto const &module_map_file = flags.get<nth::file_path>("module-map");
  auto id = flags.get<module::UniqueId>("module-identifier");

  std::optional module_map = module::ModuleMap::Load(module_map_file);
  if (not module_map) {
    NTH_LOG((v.always), "Failed to load module map from \"{}\"") <<=
        {module_map_file};
    return nth::exit_code::generic_error;
  }

  frontend::SourceIndexer source_indexer;
  diagnostic::StreamingConsumer diagnostic_consumer(nth::file::err(),
                                                    source_indexer);

  module::Resources resources(id, *module_map, diagnostic_consumer);

  std::ifstream stream(input.path());
  if (not stream.is_open()) {
    std::cerr << "Failed to open '" << input.path() << "'.\n";
    return nth::exit_code::generic_error;
  }

  serialization::Module proto;
  if (not proto.ParseFromIstream(&stream)) {
    std::cerr << "Invalid module.\n";
    return nth::exit_code::generic_error;
  }

  if (not module::DeserializeModuleInto(
          proto, module_map->imported_modules(), module::UniqueId::Self(),
          resources.primary_module(), GlobalTypeSystem,
          resources.unique_type_table(), *module_map,
          resources.function_map())) {
    // TODO Log an error.
    std::cerr << "failed to load module.";
    return nth::exit_code::generic_error;
  }

  vm::ArgumentSlice argument_slice(
      const_cast<std::string_view *>(arguments.data()), arguments.size());

  for (auto const *module : module_map->imported_modules()) {
    jasmin::ValueStack value_stack;
    vm::Execute(module->initializer(),
                vm::ExecutionState{GlobalTypeSystem, argument_slice},
                value_stack);
  }

  jasmin::ValueStack value_stack;
  vm::Execute(resources.primary_module().initializer(),
              vm::ExecutionState{GlobalTypeSystem, argument_slice},
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