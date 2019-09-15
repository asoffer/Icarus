#include <filesystem>
#include <vector>

#include "ast/node.h"
#include "ast/expression.h"
#include "frontend/source.h"
#include "init/cli.h"
#include "init/signal.h"
#include "misc/module.h"
#include "visitor/match.h"

namespace frontend {
std::vector<std::unique_ptr<ast::Node>> Parse(Src *src, ::Module *mod);
}  // namespace frontend

namespace match {
int MatchParse(std::filesystem::path const &expr_file,
              std::filesystem::path const &file) {
  Module expr_mod;
  ASSIGN_OR(return 1, frontend::FileSrc expr_src,
                   frontend::FileSrc::Make(file));
  auto expr_stmts = frontend::Parse(&expr_src, &expr_mod);
  if (expr_stmts.size() != 1) { return 2; }
  auto *expr = expr_stmts[0]->if_as<ast::Expression>();
  if (!expr) { return 2; }

  Module mod;
  ASSIGN_OR(return 1, frontend::FileSrc src, frontend::FileSrc::Make(file));
  auto stmts = frontend::Parse(&src, &mod);

  visitor::Match visitor;
  // TODO How do you want to match multiple lines?
  visitor.MatchAll(stmts[0].get(), expr);

  return 0;
}
}  // namespace match

void cli::Usage() {
  Flag("help") << "Show usage information."
               << [] { execute = cli::ShowUsage; };

  static char const *expr_file;
  Flag("expr") << "The file holding the expression to be matched."
               << [](char const * e) { expr_file = e; };

  // TODO error-out if more than one file is provided
  static char const *file;
  HandleOther = [](char const *arg) { file = arg; };
  execute     = [] {
    return match::MatchParse(std::filesystem::path{expr_file},
                             std::filesystem::path{file});
  };
}

int main(int argc, char *argv[]) {
  init::InstallSignalHandlers();
  return cli::ParseAndRun(argc, argv);
}
