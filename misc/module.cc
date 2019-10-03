#include "misc/module.h"

#include "ast/ast.h"
#include "ast/expression.h"
#include "base/guarded.h"
#include "frontend/source/source.h"
#include "type/function.h"
#include "type/jump.h"

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
Module::Module() : scope_(this) {
#ifdef ICARUS_VISITOR_EMIT_IR
  dep_data_.emplace_back();
#endif  // ICARUS_VISITOR_EMIT_IR
}

Module::~Module() = default;

// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR

type::Type const *Module::GetType(std::string_view name) const {
  ASSIGN_OR(return nullptr, auto &decl, GetDecl(name));
  return dep_data_.front().second.verify_results_.at(&decl).type_;
}

std::pair<ConstantBinding, DependentData> *Module::insert_constants(
    ConstantBinding const &constant_binding) {
  for (auto iter = dep_data_.begin(); iter != dep_data_.end(); ++iter) {
    auto &[key, val] = *iter;
    if (key == constant_binding) { return &*iter; }
  }
  auto &pair = dep_data_.emplace_back(constant_binding, DependentData{});
  pair.second.constants_ = pair.first;
  return &pair;
}
#endif  //  ICARUS_VISITOR_EMIT_IR

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
