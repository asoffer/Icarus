#include <cstdio>
#include <optional>
#include <string>

#include "ir/ir.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/debug/log/stderr_log_sink.h"
#include "nth/io/file.h"
#include "nth/io/file_path.h"
#include "nth/process/exit_code.h"

namespace ic {
namespace {

std::optional<std::string> ReadFileToString(nth::file_path const& file_name) {
  std::optional<std::string> result = std::nullopt;
  std::optional<nth::file> file = nth::file::read_only(file_name);
  if (file) {
    std::fseek(file->get(), 0, SEEK_END);
    size_t file_size = file->tell();
    file->rewind();

    result.emplace();
    result->resize(file_size, '\0');
    (void)file->read_into(*result);
    NTH_ASSERT(file->close());
  }
  return result;
}

}  // namespace

nth::exit_code Compile(nth::FlagValueSet, nth::file_path const& source) {
  std::optional content = ReadFileToString(source);
  if (not content) {
    // TODO Log an error with the diagnostics consumer.
    NTH_LOG((v.always), "Failed to load the content from \"{}\"") <<= {source};
    return nth::exit_code::generic_error;
  }

  std::string_view file_content = *content;
  DiagnosticConsumer diagnostic_consumer;
  TokenBuffer token_buffer = Lex(*content, diagnostic_consumer);
  ParseTree parse_tree     = Parse(token_buffer, diagnostic_consumer);
  IrContext context;
  ProcessIr(parse_tree.nodes(), context);

  return nth::exit_code::success;
}

}  // namespace ic

nth::Usage const nth::program_usage = {
    .description = "Icarus Compiler",
    .execute = ic::Compile,
};
