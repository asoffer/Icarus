#include <cstdio>
#include <fstream>
#include <optional>
#include <string>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "common/debug.h"
#include "common/string.h"
#include "diagnostics/consumer/streaming.h"
#include "diagnostics/message.h"
#include "ir/dependent_modules.h"
#include "ir/deserialize.h"
#include "ir/module.h"
#include "ir/module.pb.h"
#include "jasmin/execute.h"
#include "lexer/token_buffer.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/debug/log/stderr_log_sink.h"
#include "nth/io/file.h"
#include "nth/io/file_path.h"
#include "nth/process/exit_code.h"
#include "toolchain/module_map.h"

namespace ic {
namespace {

nth::exit_code Run(nth::FlagValueSet flags, std::span<std::string_view const>) {
  absl::InitializeSymbolizer("");
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  auto const& input           = flags.get<nth::file_path>("input");
  auto const& module_map_path = flags.get<nth::file_path>("module-map");
  auto const* debug_run       = flags.try_get<bool>("debug-run");
  if (debug_run) { ic::debug::run = *debug_run; }

  std::ifstream in(input.path());

  diag::StreamingConsumer consumer;

  if (not in.is_open()) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Failed to load the content from {}.">(input)),
    });
    return nth::exit_code::generic_error;
  }

  ModuleProto proto;
  Module module;

  TokenBuffer token_buffer;

  std::optional dependent_module_protos = PopulateModuleMap(module_map_path);
  if (not dependent_module_protos) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(InterpolateString<
                   "Failed to load the content from the module map file {}.">(
            module_map_path)),
    });

    return nth::exit_code::generic_error;
  }

  DependentModules dependencies;
  Deserializer d;
  if (not d.DeserializeDependentModules(*dependent_module_protos,
                                        dependencies)) {
    consumer.Consume({diag::Header(diag::MessageKind::Error),
                      diag::Text("Failed to deserialize dependent modules.")});
    return nth::exit_code::generic_error;
  }

  if (not proto.ParseFromIstream(&in) or not d.Deserialize(proto, module)) {
    NTH_LOG((v.debug), "{}") <<= {proto.DebugString()};
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Failed to parse the module content from {}.">(
                input)),
    });
    return nth::exit_code::generic_error;
  }
  NTH_LOG((v.when(debug::run)), "{}") <<= {proto.DebugString()};

  jasmin::ValueStack value_stack;
  jasmin::Execute(module.initializer(), value_stack);
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
