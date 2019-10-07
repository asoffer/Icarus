#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <memory>
#include <string_view>
#include <vector>

#include "base/ptr_span.h"
#include "core/scope.h"
#include "error/log.h"
#include "frontend/source/source.h"
#include "module/pending.h"

namespace module {
struct Module {
  Module();
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) noexcept = delete;
  Module &operator=(Module &&) noexcept = delete;

  void AppendStatements(std::vector<std::unique_ptr<ast::Node>> stmts);
  void Append(std::unique_ptr<ast::Node> node);

  ast::Declaration *GetDecl(std::string_view name) const;

  base::PtrSpan<ast::Node> unprocessed();
  base::PtrSpan<ast::Node const> unprocessed() const;

  // TODO This is poorly named. It's just a simple way to mark that everything
  // so far has been processed, but there are no invariants enforced yet.
  // Ideally we'd fix this by wrapping some functor that does the processing.
  void process();

 private:
  core::ModuleScope scope_;
  std::vector<std::unique_ptr<ast::Node>> processed_;
  std::vector<std::unique_ptr<ast::Node>> unprocessed_;

 public:
  error::Log error_log_;
};
}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
