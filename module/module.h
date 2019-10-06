#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <memory>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "core/scope.h"
#include "error/log.h"
#include "frontend/source/source.h"
#include "module/pending.h"

namespace module {
struct Module {
  explicit Module(std::vector<std::unique_ptr<ast::Node>> stmts = {});
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) noexcept = delete;
  Module &operator=(Module &&) noexcept = delete;

  void AppendStatements(std::vector<std::unique_ptr<ast::Node>> stmts);

  ast::Declaration *GetDecl(std::string_view name) const;

  core::ModuleScope scope_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  std::vector<std::unique_ptr<ast::Node>> statements_;
  frontend::Source *src_ = nullptr;
  error::Log error_log_;
};
}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
