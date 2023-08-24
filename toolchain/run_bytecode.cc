#include <cstdio>
#include <fstream>
#include <optional>
#include <string>

#include "common/string.h"
#include "diagnostics/consumer/streaming.h"
#include "diagnostics/message.h"
#include "ir/deserialize.h"
#include "ir/module.h"
#include "ir/module.pb.h"
#include "jasmin/execute.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/debug/log/stderr_log_sink.h"
#include "nth/io/file.h"
#include "nth/io/file_path.h"
#include "nth/process/exit_code.h"

namespace ic {
namespace {

nth::exit_code Run(nth::FlagValueSet flags, std::span<std::string_view const>) {
  auto const& input = flags.get<nth::file_path>("input");
  std::ifstream in(input.path());

  diag::StreamingConsumer consumer("");

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
  if (not proto.ParseFromIstream(&in) or not Deserialize(proto, module)) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Failed to parse the moudle content from {}.">(
                input)),
    });
    return nth::exit_code::generic_error;
  }

  jasmin::ValueStack value_stack;

  jasmin::Execute(module.initializer, value_stack);
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
        },
    .execute = ic::Run,
};
