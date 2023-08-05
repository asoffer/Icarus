#include <cstdlib>
#include <fstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "base/file.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "module/module.h"
#include "module/resources.h"
#include "nth/commandline/commandline.h"
#include "nth/io/file_path.h"
#include "nth/process/exit_code.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "toolchain/bazel.h"

namespace toolchain {

nth::exit_code Compile(nth::FlagValueSet flags) {
  auto const *source          = flags.get<nth::file_path>("source");
  auto const *output_path     = flags.get<nth::file_path>("output");
  auto const *module_map_file = flags.get<nth::file_path>("module-map");
  auto const *id = flags.get<module::UniqueId>("module-identifier");

  // TODO: Validate these as required.
  if (not source) { return nth::exit_code::usage; }
  if (not output_path) { return nth::exit_code::usage; }
  if (not module_map_file) { return nth::exit_code::usage; }
  if (not id) { return nth::exit_code::usage; }

  frontend::SourceIndexer source_indexer;
  diagnostic::StreamingConsumer diagnostic_consumer(stderr, &source_indexer);

  std::optional content = base::ReadFileToString(*source);
  if (not content) {
    // TODO Log an error with the diagnostics consumer.
    NTH_LOG((v.always), "Failed to load the content from {}") <<= {source};
    return nth::exit_code::generic_error;
  }

  std::string_view file_content = source_indexer.insert(
      serialization::ModuleIndex::Self(), *std::move(content));

  auto parsed_nodes = frontend::Parse(file_content, diagnostic_consumer);
  if (diagnostic_consumer.num_consumed() != 0) {
    return nth::exit_code::success;
  }

  ast::Module ast_module;
  ast_module.insert(parsed_nodes.begin(), parsed_nodes.end());

  auto specification = BazelModuleMap(*module_map_file);
  if (not specification) {
    // TODO log an error with the diagnostics consumer.
    std::cerr << "invalid spec.\n";
    NTH_LOG((v.always), "Invalid module map specification.");
    return nth::exit_code::generic_error;
  }
  auto name_resolver = BazelNameResolver(std::move(specification->names));
  NTH_ASSERT(name_resolver != nullptr);

  module::Resources resources(std::move(*id), std::move(name_resolver),
                              diagnostic_consumer);

  std::vector<std::pair<serialization::Module, module::Module *>> modules;

  // Allocate all imported modules, and populate their module maps.
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

  semantic_analysis::Context context;

  semantic_analysis::TypeVerifier tv(resources, context);
  tv.schedule(&ast_module);
  tv.complete();

  if (diagnostic_consumer.num_consumed() != 0) {
    return nth::exit_code::generic_error;
  }

  semantic_analysis::EmitByteCodeForModule(ast_module, context, resources);

  std::ofstream output(output_path->path());
  return resources.primary_module().Serialize(
             output, resources.unique_type_table(), resources.module_map(),
             resources.function_map())
             ? nth::exit_code::success
             : nth::exit_code::generic_error;
}

}  // namespace toolchain

nth::Usage const nth::program_usage = {
    .description = "Icarus Compiler",
    .flags =
        {
            {
                .name        = {"source"},
                .type        = nth::type<nth::file_path>,
                .description = "The path to the source file to be compiled.",
            },
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
            {
                .name = {"output"},
                .type = nth::type<nth::file_path>,
                .description =
                    "The location at which to write the output .icm file.",

            },
        },
    .execute = toolchain::Compile,
};
