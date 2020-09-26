#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "base/macros.h"
#include "base/no_destructor.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file.h"
#include "init/cli.h"
#include "match/match_expr.h"

namespace match {
int MatchParse(frontend::FileName const &expr_file,
               frontend::FileName const &file) {
  auto canonical_expr_file = frontend::CanonicalFileName::Make(expr_file);
  ASSIGN_OR(return 1,  //
                   frontend::FileSource expr_src,
                   frontend::FileSource::Make(canonical_expr_file));
  diagnostic::StreamingConsumer diag(stderr, &expr_src);
  auto expr_stmts = frontend::Parse(expr_src, diag);
  if (expr_stmts.size() != 1) { return 2; }
  auto *expr = expr_stmts[0]->if_as<ast::Expression>();
  if (not expr) { return 2; }

  auto canonical_file = frontend::CanonicalFileName::Make(file);
  ASSIGN_OR(return 1,  //
                   frontend::FileSource src,
                   frontend::FileSource::Make(canonical_file));
  auto stmts = frontend::Parse(src, diag);

  match::Match visitor;
  // TODO How do you want to match multiple lines?
  visitor.MatchAll(stmts[0].get(), expr);

  return 0;
}
}  // namespace match

void cli::Usage() {
  Flag("help") << "Show usage information." << [] { execute = cli::ShowUsage; };

  static base::NoDestructor<frontend::FileName> expr_file;
  Flag("expr") << "The file holding the expression to be matched."
               << [](char const *e) { *expr_file = frontend::FileName(e); };

  // TODO error-out if more than one file is provided
  static base::NoDestructor<frontend::FileName> file;
  HandleOther = [](char const *arg) { *file = frontend::FileName(arg); };
  execute     = [] { return match::MatchParse(*expr_file, *file); };
}

int main(int argc, char *argv[]) {
  absl::InitializeSymbolizer(argv[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);
  return cli::ParseAndRun(argc, argv);
}
