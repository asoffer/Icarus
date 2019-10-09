#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <memory>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/ptr_span.h"
#include "core/scope.h"

namespace module {
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

  template <typename Fn>
  void process(Fn &&fn) {
    base::PtrSpan<ast::Node const> nodes(unprocessed_.begin(),
                                         unprocessed_.end());

    IndexDeclarations(nodes);
    std::forward<Fn>(fn)(nodes);

    processed_.insert(processed_.end(),
                      std::make_move_iterator(unprocessed_.begin()),
                      std::make_move_iterator(unprocessed_.end()));
    unprocessed_.clear();
  }

 private:
  void IndexDeclarations(base::PtrSpan<ast::Node const> nodes);

  core::ModuleScope scope_;
  absl::flat_hash_map<std::string_view, std::vector<ast::Declaration const *>>
      top_level_decls_;
  std::vector<std::unique_ptr<ast::Node>> processed_;
  std::vector<std::unique_ptr<ast::Node>> unprocessed_;
};

template <typename Extension = void>
struct ExtendedModule : BasicModule {
 private:
  Extension ext_;
};

template <>
struct ExtendedModule<void> : BasicModule {};

using Module = ExtendedModule<void>;
}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
