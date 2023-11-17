#include <cstdio>
#include <fstream>
#include <optional>
#include <string>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "common/debug.h"
#include "common/errno.h"
#include "common/file.h"
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
  auto [parse_tree, scope_tree] = Parse(token_buffer, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  consumer.set_parse_tree(parse_tree);

  DependentModules dependencies;
  Deserializer d(dependencies);
  if (not d.DeserializeDependentModules(*dependent_module_protos,
                                        dependencies)) {
    consumer.Consume({diag::Header(diag::MessageKind::Error),
                      diag::Text("Failed to deserialize dependent modules.")});
    return nth::exit_code::generic_error;
  }

  Module module;
  EmitContext emit_context(parse_tree, dependencies, scope_tree, module);
  ProcessIr(emit_context, consumer);
  if (consumer.count() != 0) { return nth::exit_code::generic_error; }
  EmitContext::WorkItem item{
      .range = parse_tree.node_range(),
  };
  item.push_function(module.initializer(), LexicalScope::Index::Root());
  emit_context.queue.push(std::move(item));
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
