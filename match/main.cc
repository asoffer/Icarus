#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "base/macros.h"
#include "frontend/parse.h"
#include "frontend/source/file.h"
#include "init/cli.h"
#include "match/match_expr.h"

namespace match {
int MatchParse(frontend::FileName const &expr_file,
               frontend::FileName const &file) {
  ASSIGN_OR(return 1,  //
                   auto canonical_expr_file,
                   frontend::CanonicalFileName::Make(expr_file));
  ASSIGN_OR(return 1,  //
                   frontend::FileSource expr_src,
                   frontend::FileSource::Make(canonical_expr_file));
  auto expr_stmts = frontend::Parse(&expr_src);
  if (expr_stmts.size() != 1) { return 2; }
  auto *expr = expr_stmts[0]->if_as<ast::Expression>();
  if (not expr) { return 2; }

  ASSIGN_OR(return 1,  //
                   auto canonical_file,
                   frontend::CanonicalFileName::Make(file));
  ASSIGN_OR(return 1,  //
                   frontend::FileSource src,
                   frontend::FileSource::Make(canonical_file));
  auto stmts = frontend::Parse(&src);

  match::Match visitor;
  // TODO How do you want to match multiple lines?
  visitor.MatchAll(stmts[0].get(), expr);

  return 0;
}
}  // namespace match

void cli::Usage() {
  Flag("help") << "Show usage information." << [] { execute = cli::ShowUsage; };

  static char const *expr_file;
  Flag("expr") << "The file holding the expression to be matched."
               << [](char const *e) { expr_file = e; };

  // TODO error-out if more than one file is provided
  static char const *file;
  HandleOther = [](char const *arg) { file = arg; };
  execute     = [] {
    return match::MatchParse(frontend::FileName(expr_file),
                             frontend::FileName(file));
  };
}

int main(int argc, char *argv[]) {
  absl::InitializeSymbolizer(argv[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);
  return cli::ParseAndRun(argc, argv);
}
