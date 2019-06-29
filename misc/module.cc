#include "misc/module.h"

#include "ast/ast.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
#include "base/guarded.h"
#include "frontend/source.h"
#ifdef ICARUS_VISITOR_EMIT_IR
#include "ir/compiled_fn.h"
#endif  // ICARUS_VISITOR_EMIT_IR
#include "type/function.h"
#include "type/jump.h"

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
Module::Module() : scope_(this) { dep_data_.emplace_back(); }

Module::~Module() = default;

// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
ir::CompiledFn *Module::AddFunc(
    type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  return fns_
      .emplace_back(
          std::make_unique<ir::CompiledFn>(this, fn_type, std::move(params)))
      .get();
}

ir::CompiledFn *Module::AddJump(
    type::Jump const *jump_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  return fns_
      .emplace_back(std::make_unique<ir::CompiledFn>(
          this, jump_type->ToFunction(), std::move(params)))
      .get();
}

type::Type const *Module::GetType(std::string_view name) const {
  ASSIGN_OR(return nullptr, auto &decl, GetDecl(name));
  return dep_data_.front().second.verify_results_.at(&decl).type_;
}
#endif

ast::Declaration *Module::GetDecl(std::string_view name) const {
  for (auto const &stmt : statements_) {
    ASSIGN_OR(continue, auto &decl, stmt->if_as<ast::Declaration>());
    if (decl.id() != name) { continue; }
    auto &hashtags = decl.hashtags_;
    bool exported =
        std::any_of(hashtags.begin(), hashtags.end(), [](ast::Hashtag h) {
          return h.kind_ == ast::Hashtag::Builtin::Export;
        });
    if (!exported) { continue; }
    return &decl;
  }
  return nullptr;
}

void Module::CompleteAllDeferredWork() {
  while (!deferred_work_.empty()) {
    auto &work = deferred_work_.front();
    if (work == nullptr) { continue; }
    work();
    deferred_work_.pop();
  }
}
