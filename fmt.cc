#include <filesystem>
#include <vector>

#include "frontend/source.h"
#include "init/cli.h"
#include "init/signal.h"
#include "misc/module.h"
#include "visitor/format.h"

namespace frontend {
std::unique_ptr<ast::Statements> Parse(Src *src, ::Module *mod);
}  // namespace frontend

namespace format {
int FormatFile(std::filesystem::path const &file) {
  Module mod;
  ASSIGN_OR(return 1, frontend::FileSrc src,
                   frontend::FileSrc::Make(file));
  auto stmts = frontend::Parse(&src, &mod);
  if (!stmts) { return 2; }
  visitor::Format visitor;
  stmts->format(&visitor);
  return 0;
}
}  // namespace format

void cli::Usage() {
  Flag("help") << "Show usage information."
               << [] { execute = cli::ShowUsage; };

  // TODO error-out if more than one file is provided
  static char const *file;
  HandleOther = [](char const *arg) { file = arg; };
  execute     = [] { return format::FormatFile(std::filesystem::path{file}); };
}

int main(int argc, char *argv[]) {
  init::InstallSignalHandlers();
  return cli::ParseAndRun(argc, argv);
}
