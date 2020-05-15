#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <memory>
#include <string_view>
#include <utility>
#include <vector>
#include <forward_list>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "absl/synchronization/notification.h"
#include "ast/scope/module.h"
#include "base/cast.h"
#include "base/expected.h"
#include "base/guarded.h"
#include "base/macros.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/consumer/streaming.h"
#include "diagnostic/errors.h"
#include "frontend/source/file.h"
#include "frontend/source/file_name.h"
#include "frontend/source/shared.h"
#include "frontend/source/source.h"
#include "ir/value/module_id.h"
#include "module/module.h"

namespace module {
template <typename T>
struct ExtendedModule;

// BasicModule:
//
// Represents a unit of compilation, beyond which all intercommunication must be
// explicit.
struct BasicModule : base::Cast<BasicModule> {
  BasicModule();
  virtual ~BasicModule();

  // Pointers to modules are passed around, so moving a module is not safe.
  BasicModule(BasicModule &&) noexcept = delete;
  BasicModule &operator=(BasicModule &&) noexcept = delete;

  // Copying a module is implicitly disallowed as modules hold move-only types.
  // We explicitly delete them to improve error messages, and because even if
  // they were not implicitly deleted, we would not want modules to be copyable
  // anyway for reasons similar to those explaining why we disallow moves.
  BasicModule(BasicModule const &) = delete;
  BasicModule &operator=(BasicModule const &) = delete;

  void AppendNode(std::unique_ptr<ast::Node> node,
                  diagnostic::DiagnosticConsumer &diag);
  void AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes,
                   diagnostic::DiagnosticConsumer &diag);

  absl::Span<ast::Declaration const *const> ExportedDeclarations(
      std::string_view name) const;

  constexpr ast::ModuleScope const *scope() const { return &scope_; }

  void ProcessFromSource(frontend::Source *src,
                         diagnostic::DiagnosticConsumer &diag);

  void Wait() const { complete_.WaitForNotification(); }

 protected:
  virtual void ProcessNodes(base::PtrSpan<ast::Node const>,
                            diagnostic::DiagnosticConsumer &) = 0;

 private:
  void InitializeNodes(base::PtrSpan<ast::Node> nodes);

  void Complete() const;

  ast::ModuleScope scope_;
  absl::flat_hash_map<std::string_view, std::vector<ast::Declaration const *>>
      exported_declarations_;
  std::vector<std::unique_ptr<ast::Node>> nodes_;

  // Notifies when exports are ready to be consumed.
  mutable absl::Notification exports_complete_;
  mutable absl::Notification complete_;
};

// Returns a container of all declarations in this scope and in parent scopes
// with the given identifier.
std::vector<ast::Declaration const *> AllDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id);

// Calls `fn` on each declaration in this scope and in parent scopes with the
// given identifier.
template <typename Fn>
bool ForEachDeclTowardsRoot(ast::Scope const *starting_scope,
                            std::string_view id, Fn fn) {
  for (auto scope_ptr = starting_scope; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) {
        if (not fn(decl)) { return false; }
      }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      // TODO use the right bound constants? or kill bound constants?
      for (auto *decl : mod->ExportedDeclarations(id)) {
        // TODO what about transitivity for embedded modules?
        // New context will lookup with no constants.
        if (not fn(decl)) { return false; }
      }
    }
  }
  return true;
}

// Returns a container of all declaration with the given identifier that are in
// a scope directly related to this one (i.e., one of the scopes is an ancestor
// of the other, or is the root scope of an embedded module).
std::vector<ast::Declaration const *> AllAccessibleDecls(
    ast::Scope const *starting_scope, std::string_view id);


namespace internal {

inline base::guarded<
    absl::flat_hash_map<ir::ModuleId, std::unique_ptr<BasicModule>>>
    modules;

}  // namespace internal

// Returns a pointer to a module of type given by the template parameter, by
// loading the module from the filesystem denoted by the file named `file_name`.
// When the module is returned it may not be ready for consumption yet as the
// processing is started in a separate thread. Each module implementation has
// its own criteria for which parts are available when and how to access them.
// BasicModule provides no such guarantees.
template <typename ModType>
ModType *ImportModule(frontend::CanonicalFileName const &file_name) {
  auto id = ir::ModuleId::FromFile(file_name);

  ModType *result;
  {
    auto handle = internal::modules.lock();

    // TODO Need to add dependencies even if the node was already scheduled
    // (hence the "already scheduled" check is done after this).
    //
    // TODO detect dependency cycles.

    auto [iter, inserted] = handle->try_emplace(id);
    auto &mod             = iter->second;

    if (not inserted) { return &mod->as<ModType>(); }

    auto derived_mod = std::make_unique<ModType>();
    result           = derived_mod.get();
    mod              = std::move(derived_mod);

    if (auto maybe_file_src = frontend::FileSource::Make(id.filename())) {
      std::thread t([mod      = &mod,
                     file_src = std::move(*maybe_file_src)]() mutable {
        auto *src =
            frontend::Source::Make<frontend::FileSource>(std::move(file_src));
        diagnostic::StreamingConsumer diag(stderr, src);
        (*mod)->ProcessFromSource(src, diag);
      });
      t.detach();
    } else {
      diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
      diag.Consume(diagnostic::MissingModule{
          .source    = id.filename(),
          .requestor = "",
      });
      return nullptr;
    }
  }

  return result;
}

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
