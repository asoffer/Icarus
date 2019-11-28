#include "ast/scope/exec.h"

#include "ast/scope/fn.h"

namespace ast {

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  // This is not just a convencience, it's necessary for correctness because on
  // FnScope construction, the ExecScope constructor will be run before any
  // FnScope member is initialized.
  if (auto *containing_fn_scope = parent->Containing<FnScope>()) {
    containing_fn_scope->insert_descendant(this);
  }
}

}  // namespace ast
