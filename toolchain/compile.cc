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
#include "module/module_map.h"
#include "module/resources.h"
#include "module/serialize.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/debug/log/stderr_log_sink.h"
#include "nth/io/file_path.h"
#include "nth/process/exit_code.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_verification/verify.h"

namespace toolchain {

nth::exit_code Compile(nth::FlagValueSet flags) {
  nth::RegisterLogSink(nth::stderr_log_sink);
  auto const &source          = flags.get<nth::file_path>("source");
  auto const &output_path     = flags.get<nth::file_path>("output");
  auto const &module_map_file = flags.get<nth::file_path>("module-map");
  auto id = flags.get<module::UniqueId>("module-identifier");

  frontend::SourceIndexer source_indexer;
  diagnostic::StreamingConsumer diagnostic_consumer(stderr, &source_indexer);

  std::optional content = base::ReadFileToString(source);
  if (not content) {
    // TODO Log an error with the diagnostics consumer.
    NTH_LOG((v.always), "Failed to load the content from \"{}\"") <<= {source};
    return nth::exit_code::generic_error;
  }

  std::optional module_map = module::ModuleMap::Load(module_map_file);
  if (not module_map) {
    NTH_LOG((v.always), "Failed to load module map from \"{}\"") <<=
        {module_map_file};
    return nth::exit_code::generic_error;
  }

  std::string_view file_content =
      source_indexer.insert(module::UniqueId::Self(), *std::move(content));

  auto parsed_nodes = frontend::Parse(file_content, diagnostic_consumer);
  if (diagnostic_consumer.num_consumed() != 0) {
    return nth::exit_code::success;
  }

  ast::Module ast_module;
  ast_module.insert(parsed_nodes.begin(), parsed_nodes.end());

  module::Resources resources(id, *module_map, diagnostic_consumer);

  semantic_analysis::Context context;
  semantic_analysis::TypeVerifier tv(resources, context);
  tv.schedule(&ast_module);
  tv.complete();

  if (diagnostic_consumer.num_consumed() != 0) {
    return nth::exit_code::generic_error;
  }

  semantic_analysis::EmitByteCodeForModule(ast_module, context, resources);

  std::ofstream output(output_path.path());
  return module::SerializeModule(resources.primary_module(), output,
                                 resources.unique_type_table(), *module_map,
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
