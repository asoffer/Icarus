#include <string>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "absl/strings/match.h"
#include "absl/strings/string_view.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "base/macros.h"
#include "base/no_destructor.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file.h"
#include "match/match_expr.h"

ABSL_FLAG(std::string, expr, "",
          "The file holding the expression to be matched.");

namespace match {
int MatchParse(frontend::FileName const &expr_file,
               frontend::FileName const &file) {
  auto canonical_expr_file = frontend::CanonicalFileName::Make(expr_file);
  auto expr_src            = frontend::FileSource::Make(canonical_expr_file);
  if (!expr_src.ok()) return 1;
  diagnostic::StreamingConsumer diag(stderr, &*expr_src);
  auto expr_stmts = frontend::Parse(expr_src->buffer(), diag);
  if (expr_stmts.size() != 1) { return 2; }
  auto *expr = expr_stmts[0]->if_as<ast::Expression>();
  if (not expr) { return 2; }

  auto canonical_file = frontend::CanonicalFileName::Make(file);
  auto src            = frontend::FileSource::Make(canonical_file);
  if (!src.ok()) return 1;
  auto stmts = frontend::Parse(src->buffer(), diag);

  match::Match visitor;
  // TODO How do you want to match multiple lines?
  visitor.MatchAll(stmts[0].get(), expr);

  return 0;
}
}  // namespace match

bool HelpFilter(absl::string_view module) {
  return absl::EndsWith(module, "/main.cc");
}

int main(int argc, char *argv[]) {
  absl::FlagsUsageConfig flag_config;
  flag_config.contains_helpshort_flags = &HelpFilter;
  flag_config.contains_help_flags      = &HelpFilter;
  absl::SetFlagsUsageConfig(flag_config);
  absl::SetProgramUsageMessage("the Icarus expression matcher.");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  if (absl::GetFlag(FLAGS_expr).empty()) {
    std::cerr << "Missing required argument --expr" << std::endl;
    return 1;
  }
  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file"
              << std::endl;
    return 1;
  }
  if (args.size() > 2) {
    std::cerr << "Too many positional arguments." << std::endl;
    return 1;
  }
  return match::MatchParse(frontend::FileName(absl::GetFlag(FLAGS_expr)),
                           frontend::FileName(args[1]));
}
