#include <cstdio>
#include <optional>
#include <string>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "common/debug.h"
#include "common/string.h"
#include "common/to_bytes.h"
#include "diagnostics/consumer/streaming.h"
#include "diagnostics/message.h"
#include "ir/dependent_modules.h"
#include "ir/deserialize.h"
#include "ir/module.h"
#include "ir/program_arguments.h"
#include "jasmin/core/function.h"
#include "jasmin/core/value.h"
#include "nth/commandline/commandline.h"
#include "nth/container/stack.h"
#include "nth/debug/log/log.h"
#include "nth/debug/log/stderr_log_sink.h"
#include "nth/io/file_path.h"
#include "nth/io/reader/file.h"
#include "nth/io/reader/string.h"
#include "nth/process/exit_code.h"
#include "toolchain/module_map.h"

namespace ic {
namespace {

nth::exit_code Run(nth::FlagValueSet flags, std::span<std::string_view const> arguments) {
  absl::InitializeSymbolizer("");
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  auto const& input           = flags.get<nth::file_path>("input");
  auto const& module_map_path = flags.get<nth::file_path>("module-map");
  auto const* debug_run       = flags.try_get<bool>("debug-run");
  if (debug_run) { ic::debug::run = *debug_run; }

  SetProgramArguments(
      std::vector<std::string>(arguments.begin(), arguments.end()));

  diag::StreamingConsumer consumer;

  std::optional dependent_modules = PopulateModuleMap(module_map_path, shared_context);
  if (not dependent_modules) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(InterpolateString<
                   "Failed to load the content from the module map file {}.">(
            module_map_path)),
    });
    return nth::exit_code::generic_error;
  }

  std::optional reader = nth::io::file_reader::try_open(input);
  if (not reader) { return nth::exit_code::generic_error; }
  std::string serialized_content(reader->size(), '\0');
  if (not reader->read(ToBytes(serialized_content))) {
    return nth::exit_code::generic_error;
  }

  ModuleDeserializer<nth::io::string_reader> deserializer(serialized_content,
                                                          shared_context);
  Module module;
  if (not nth::io::deserialize(deserializer, module)) {
    return nth::exit_code::generic_error;
  }

  nth::stack<jasmin::Value> value_stack;
  module.initializer().invoke(value_stack);
  return nth::exit_code::success;
}

}  // namespace
}  // namespace ic

nth::Usage const nth::program_usage = {
    .description = "Icarus Bytecode Interpreter",
    .flags =
        {
            {.name        = {"input"},
             .type        = nth::type<nth::file_path>,
             .description = "The from which to read the input .icm file."},
            {.name        = {"module-map"},
             .type        = nth::type<nth::file_path>,
             .description = "The location of the .icmod file defining the "
                            "module mapping."},
            {.name        = {"debug-run"},
             .type        = nth::type<bool>,
             .description = "Dumps serialized byte code before executing."},

        },

    .execute = ic::Run,
};
