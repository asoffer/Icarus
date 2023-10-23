#include <cstdio>
#include <fstream>
#include <optional>
#include <string>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/strings/str_split.h"
#include "common/debug.h"
#include "common/errno.h"
#include "common/resources.h"
#include "common/string.h"
#include "diagnostics/consumer/streaming.h"
#include "diagnostics/message.h"
#include "ir/dependent_modules.h"
#include "ir/deserialize.h"
#include "ir/emit.h"
#include "ir/ir.h"
#include "ir/module.pb.h"
#include "ir/serialize.h"
#include "lexer/lexer.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/io/file.h"
#include "nth/io/file_path.h"
#include "nth/process/exit_code.h"
#include "parser/parser.h"

namespace ic {
namespace {

std::optional<std::string> ReadFileToString(nth::file_path const& file_name) {
  errno_resetter e;
  std::optional<std::string> result = std::nullopt;
  std::optional<nth::file> file = nth::file::read_only(file_name);
  if (file) {
    std::fseek(file->get(), 0, SEEK_END);
    size_t file_size = file->tell();
    file->rewind();

    result.emplace();
    result->resize(file_size, '\0');
    (void)file->read_into(*result);
    NTH_REQUIRE(file->close());
  }
  return result;
}

std::optional<std::vector<ModuleProto>> PopulateModuleMap(
    nth::file_path const& module_map_file) {
  std::optional content = ReadFileToString(module_map_file);
  if (not content) { return std::nullopt; }
  std::vector<ModuleProto> dependent_module_protos;
  for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
    if (line.empty()) { continue; }
    size_t count = 0;
    std::string_view name, location;
    for (std::string_view chunk : absl::StrSplit(line, absl::ByChar('\t'))) {
      switch (count++) {
        case 0: name = chunk; break;
        case 1: location = chunk; break;
        default: return std::nullopt;
      }
    }
    auto id = resources.module_map.add(name);
    NTH_REQUIRE(dependent_module_protos.size() + 1 == id.value());

    std::ifstream in{std::string(location)};
    if (not in.is_open()) { NTH_UNIMPLEMENTED("{}") <<= {location}; }
    dependent_module_protos.emplace_back().ParseFromIstream(&in);
  }

  return dependent_module_protos;
}

nth::exit_code Compile(nth::FlagValueSet flags, nth::file_path const& source) {
  absl::InitializeSymbolizer("");
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  auto const& output_path     = flags.get<nth::file_path>("output");
  auto const& module_map_path = flags.get<nth::file_path>("module-map");

  auto const* debug_parser     = flags.try_get<bool>("debug-parser");
  auto const* debug_type_check = flags.try_get<bool>("debug-type-check");
  auto const* debug_emit       = flags.try_get<bool>("debug-emit");
  if (debug_parser) { ic::debug::parser = *debug_parser; }
  if (debug_type_check) { ic::debug::type_check = *debug_type_check; }
  if (debug_emit) { ic::debug::emit = *debug_emit; }

  diag::StreamingConsumer consumer;

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
  std::optional content = ReadFileToString(source);
  if (not content) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Failed to load the content from {}.">(source)),
    });
    return nth::exit_code::generic_error;
  }

  consumer.set_source(*content);

  TokenBuffer token_buffer = lex::Lex(*content, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  ParseTree parse_tree = Parse(token_buffer, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  consumer.set_parse_tree(parse_tree);

  DependentModules dependencies;
  Deserializer d;
  if (not d.DeserializeDependentModules(*dependent_module_protos,
                                        dependencies)) {
    consumer.Consume({diag::Header(diag::MessageKind::Error),
                      diag::Text("Failed to deserialize dependent modules.")});
    return nth::exit_code::generic_error;
  }

  Module module;
  EmitContext emit_context(parse_tree, dependencies, module);
  ProcessIr(emit_context, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  emit_context.queue.push({.range = parse_tree.node_range()});
  EmitIr(emit_context);
  SetExported(emit_context);

  ModuleProto module_proto;
  Serializer s;
  s.Serialize(module, module_proto);

  std::ofstream out(output_path.path());
  return module_proto.SerializeToOstream(&out) ? nth::exit_code::success
                                               : nth::exit_code::generic_error;
}

}  // namespace
}  // namespace ic

nth::Usage const nth::program_usage = {
    .description = "Icarus Compiler",
    .flags =
        {
            {
                .name = {"debug-emit"},
                .type = nth::type<bool>,
                .description =
                    "Turns on debug information for byte-code emission.",
            },
            {
                .name        = {"debug-parser"},
                .type        = nth::type<bool>,
                .description = "Turns on debug information for the parser.",
            },
            {
                .name = {"debug-type-check"},
                .type = nth::type<bool>,
                .description =
                    "Turns on debug information for the type-checker.",
            },
            {
                .name = {"output"},
                .type = nth::type<nth::file_path>,
                .description =
                    "The location at which to write the output .icm file.",
            },
            {
                .name        = {"module-map"},
                .type        = nth::type<nth::file_path>,
                .description = "The location of the .icmod file defining the "
                               "module mapping.",
            },
        },
    .execute = ic::Compile,
};
