#include "ast/dispatch.h"

#include "type/variant.h"

namespace AST {
void DispatchTable::insert(FnArgs<const type::Type *> call_arg_types,
                           Binding binding, size_t expanded_size) {
  if (expanded_size == std::numeric_limits<size_t>::max()) {
    expanded_size = 1;
    call_arg_types.Apply([&expanded_size](const type::Type *t) {
      if (t->is<type::Variant>()) {
        expanded_size *= t->as<type::Variant>().size();
      }
    });
  }

  total_size_ += expanded_size;
  bindings_.emplace(std::move(call_arg_types), std::move(binding));
}

std::optional<Binding> Binding::MakeUntyped(
    AST::Expression *fn_expr, const FnArgs<Expression *> &args,
    const std::unordered_map<std::string, size_t> &index_lookup) {
  Binding result(fn_expr, index_lookup.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    result.exprs_[i] = std::pair(nullptr, args.pos_[i]);
  }

  // Match the named arguments
  for (const auto & [ name, expr ] : args.named_) {
    // TODO emit an error explaining why we couldn't use this one if there
    // was a missing named argument.
    auto iter = index_lookup.find(name);
    if (iter == index_lookup.end()) { return std::nullopt; }
    result.exprs_[iter->second] = std::pair(nullptr, expr);
  }
  return result;
}
}  // namespace AST
