#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <forward_list>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "ast/module.h"
#include "ast/scope.h"
#include "base/cast.h"
#include "base/guarded.h"
#include "base/macros.h"
#include "base/ptr_span.h"
#include "frontend/source/buffer.h"

namespace module {

enum class Linkage { Internal, External };

// BasicModule:
//
// Represents a unit of compilation, beyond which all intercommunication must be
// explicit.
struct BasicModule : base::Cast<BasicModule> {
  explicit BasicModule(frontend::SourceBuffer const *buffer)
      : module_(this), buffer_(buffer) {}
  virtual ~BasicModule() {}

  // Pointers to modules are passed around, so moving a module is not safe.
  BasicModule(BasicModule &&) noexcept = delete;
  BasicModule &operator=(BasicModule &&) noexcept = delete;

  // Copying a module is implicitly disallowed as modules hold move-only types.
  // We explicitly delete them to improve error messages, and because even if
  // they were not implicitly deleted, we would not want modules to be copyable
  // anyway for reasons similar to those explaining why we disallow moves.
  BasicModule(BasicModule const &) = delete;
  BasicModule &operator=(BasicModule const &) = delete;

  ast::ModuleScope const &scope() const { return module_.body_scope(); }
  ast::ModuleScope &scope() { return module_.body_scope(); }

  frontend::SourceBuffer const &buffer() const {
    return *ASSERT_NOT_NULL(buffer_);
  }

  template <std::input_iterator Iter>
  base::PtrSpan<ast::Node const> insert(Iter b, Iter e) {
    return module_.insert(b, e);
  }

  bool has_error_in_dependent_module() const {
    return depends_on_module_with_errors_;
  }
  void set_dependent_module_with_errors() {
    depends_on_module_with_errors_ = true;
  }

 private:
  ast::Module module_;
  frontend::SourceBuffer const *buffer_;

  // This flag should be set to true if this module is ever found to depend on
  // another which has errors, even if those errors do not effect
  // code-generation in this module.
  //
  // TODO: As we move towards separate compilation in separate processes, this
  // will become irrelevant.
  bool depends_on_module_with_errors_ = false;
};

// Returns a container of all visible declarations in this scope  with the given
// identifier. This means any declarations in the path to the ancestor
// function/jump, and any constant declarations above that.
std::vector<ast::Declaration::Id const *> AllVisibleDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id);

// Returns a container of all declaration ids with the given identifier that are
// in a scope directly related to this one (i.e., one of the scopes is an
// ancestor of the other, or is the root scope of an embedded module).
std::vector<ast::Declaration::Id const *> AllAccessibleDeclIds(
    ast::Scope const *starting_scope, std::string_view id);

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
