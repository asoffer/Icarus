#include <cstdio>
#include <optional>
#include <string>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "common/debug.h"
#include "common/errno.h"
#include "common/resources.h"
#include "common/string.h"
#include "diagnostics/consumer/streaming.h"
#include "diagnostics/message.h"
#include "ir/declaration.h"
#include "ir/dependent_modules.h"
#include "ir/deserialize.h"
#include "ir/emit.h"
#include "ir/ir.h"
#include "ir/serialize.h"
#include "lexer/lexer.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/io/reader/file.h"
#include "nth/io/file_path.h"
#include "nth/io/serialize/serialize.h"
#include "nth/io/writer/string.h"
#include "nth/process/exit_code.h"
#include "parse/parser.h"
#include "toolchain/module_map.h"

namespace ic {
namespace {

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

  std::optional dependencies = PopulateModuleMap(module_map_path);
  if (not dependencies) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(InterpolateString<
                   "Failed to load the content from the module map file {}.">(
            module_map_path)),
    });

    return nth::exit_code::generic_error;
  }

  StringLiteral::CompleteGeneration();
  ForeignFunction::CompleteGeneration();

  std::optional reader = nth::io::file_reader::try_open(source);
  if (not reader) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Failed to load the content from {}.">(source)),
    });
    return nth::exit_code::generic_error;
  }
  std::string content(reader->size(), '\0');
  if (not reader->read(std::span<std::byte>(
          reinterpret_cast<std::byte*>(content.data()), content.size()))) {
    consumer.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Failed to load the content from {}.">(source)),
    });
    return nth::exit_code::generic_error;
  }

  consumer.set_source(content);

  TokenBuffer token_buffer = lex::Lex(content, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  auto [parse_tree, scope_tree] = Parse(token_buffer, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  if (not AssignDeclarationsToIdentifiers(parse_tree, consumer)) {
    return nth::exit_code::generic_error;
  }
  consumer.set_parse_tree(parse_tree);

  Module module;
  EmitContext emit_context(parse_tree, *dependencies, scope_tree, module);
  ProcessIr(emit_context, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  EmitContext::WorkItem item{
      .range = parse_tree.node_range(),
  };
  item.push_function(module.insert_initializer(), LexicalScope::Index::Root());
  emit_context.queue.push(std::move(item));
  EmitIr(emit_context);
  SetExported(emit_context);

  std::string serialized_content;
  ModuleSerializer<nth::io::string_writer> serializer(serialized_content);
  if (not nth::io::serialize(serializer, module)) {
    consumer.Consume(
        {diag::Header(diag::MessageKind::Error),
         diag::Text("Failed to serialize module. This is a compiler bug.")});
    return nth::exit_code::generic_error;
  }

  std::FILE* file = std::fopen(output_path.path().c_str(), "w");
  if (not file) {
    consumer.Consume({diag::Header(diag::MessageKind::Error),
                      diag::Text("Failed to open output file for writing. This "
                                 "is a compiler bug.")});
    return nth::exit_code::generic_error;
  }

  if (std::fwrite(serialized_content.c_str(), 1, serialized_content.size(),
                  file) != serialized_content.size()) {
    consumer.Consume({diag::Header(diag::MessageKind::Error),
                      diag::Text("Failed to write entire serialized output. "
                                 "This is a compiler bug.")});
    return nth::exit_code::generic_error;
  }

  if (std::fclose(file) != 0) {
    consumer.Consume({diag::Header(diag::MessageKind::Error),
                      diag::Text("Failed to close serialized output file. This "
                                 "is a compiler bug.")});
    return nth::exit_code::generic_error;
  }

  return nth::exit_code::success;
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
