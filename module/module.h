#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/ptr_span.h"
#include "core/scope.h"

namespace module {
template <typename T>
struct ExtendedModule;

struct BasicModule {
  BasicModule();
  ~BasicModule();

  // We take pointers to the module, so it cannot be moved.
  BasicModule(BasicModule &&) noexcept = delete;
  BasicModule &operator=(BasicModule &&) noexcept = delete;

  void AppendStatements(std::vector<std::unique_ptr<ast::Node>> stmts);
  void Append(std::unique_ptr<ast::Node> node);

  absl::Span<ast::Declaration const *const> declarations(
      std::string_view name) const;

 private:
  template <typename T>
  friend struct ExtendedModule;

  void InitializeNodes(base::PtrSpan<ast::Node > nodes);

  core::ModuleScope scope_;
  absl::flat_hash_map<std::string_view, std::vector<ast::Declaration const *>>
      top_level_decls_;
  std::vector<std::unique_ptr<ast::Node>> nodes_;
};

template <typename Extension = void>
struct ExtendedModule : BasicModule {
 public:
  explicit ExtendedModule(
      std::function<void(base::PtrSpan<ast::Node const>)> fn)
      : process_(std::move(fn)) {}

  void Process(std::unique_ptr<ast::Node> node) {
    InitializeNodes(base::PtrSpan<ast::Node>(&node, 1));
    process_(base::PtrSpan<ast::Node const>(&node, 1));
    nodes_.push_back(std::move(node));
  }

  void Process(std::vector<std::unique_ptr<ast::Node>> nodes) {
    InitializeNodes(nodes);
    process_(nodes);
    nodes_.insert(nodes_.end(), std::make_move_iterator(nodes.begin()),
                  std::make_move_iterator(nodes.end()));
  }

 private:
  std::function<void(base::PtrSpan<ast::Node const>)> process_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
