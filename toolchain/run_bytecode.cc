#include <fstream>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include "diagnostic/consumer/streaming.h"
#include "frontend/source_indexer.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "module/resources.h"
#include "nth/commandline/commandline.h"
#include "nth/process/exit_code.h"
#include "toolchain/bazel.h"
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

  auto specification = BazelModuleMap(*module_map_file);
  if (not specification) {
    // TODO log an error
    std::cerr << "invalid spec.\n";
    return nth::exit_code::generic_error;
  }
  auto name_resolver = BazelNameResolver(std::move(specification->names));
  NTH_ASSERT(name_resolver != nullptr);

  frontend::SourceIndexer source_indexer;
  diagnostic::StreamingConsumer diagnostic_consumer(stderr, &source_indexer);

  module::Resources resources(*id, std::move(name_resolver),
                              diagnostic_consumer);

  std::vector<std::pair<serialization::Module, module::Module *>> modules;
  for (auto const &[id, path] : specification->paths) {
    std::ifstream stream(path);

    if (not stream.is_open()) {
      std::cerr << "failed to open " << id.value() << " (" << path << ").\n";
      return nth::exit_code::generic_error;
    }

    auto &[proto, mptr] = modules.emplace_back();
    if (not proto.ParseFromIstream(&stream)) {
      std::cerr << "failed to parse " << id.value() << " (" << path << ").\n";
      return nth::exit_code::generic_error;
    }
    auto index = serialization::ModuleIndex(resources.imported_modules());
    mptr       = &resources.AllocateModule(id);

    resources.module_map().insert(serialization::ModuleIndex::Self(), index,
                                  id);
    if (not module::GlobalModuleMap::Deserialize(index, proto.module_map(),
                                                 resources.module_map())) {
      // TODO Log an error.
      std::cerr << "failed to load module " << id.value() << " (" << path
                << ").\n";
      return nth::exit_code::generic_error;
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
      return nth::exit_code::generic_error;
    }
    ++i;
  }

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

  if (not module::Module::DeserializeInto(
          proto, resources.modules(), serialization::ModuleIndex::Self(),
          resources.primary_module(), resources.primary_module().type_system(),
          resources.unique_type_table(), resources.module_map(),
          resources.function_map(), resources.opaque_map())) {
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
