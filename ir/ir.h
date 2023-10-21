#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include <optional>
#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "diagnostics/consumer/consumer.h"
#include "ir/emit.h"
#include "jasmin/value_stack.h"
#include "nth/debug/debug.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct IrContext {
  ParseTree::Node const& Node(ParseTree::Node::Index index) {
    return emit.tree[index];
  }

  auto ChildIndices(ParseTree::Node::Index index) {
    return emit.tree.child_indices(index);
  }
  auto Children(ParseTree::Node::Index index) {
    return emit.tree.children(index);
  }

  void ProcessIr(diag::DiagnosticConsumer& diag);

  template <typename T>
  std::optional<T> EvaluateAs(ParseTree::Node::Index subtree_root_index) {
    T result;
    nth::interval range = emit.tree.subtree_range(subtree_root_index);
    jasmin::ValueStack value_stack;
    emit.Evaluate(range, value_stack, {FromConstant<T>()});
    if constexpr (nth::type<T> == nth::type<std::string_view>) {
      size_t length   = value_stack.pop<size_t>();
      char const* ptr = value_stack.pop<char const*>();
      return std::string_view(ptr, length);
    } else if (IcarusDeserializeValue(
                   std::span(value_stack.begin(), value_stack.end()), result)) {
      return result;
    } else {
      return std::nullopt;
    }
  }

  std::vector<DeclarationInfo> declaration_stack;
  std::vector<type::QualifiedType> type_stack;
  std::vector<Token::Kind> operator_stack;
  EmitContext emit;

 private:
  template <typename T>
  static type::Type FromConstant() {
    auto t = nth::type<T>;
    if constexpr (t == nth::type<type::Type>) {
      return type::Type_;
    } else if constexpr (t == nth::type<ModuleId>) {
      return type::Module;
    } else if constexpr (t == nth::type<std::string_view>) {
      return type::Slice(type::Char);
    } else {
      NTH_UNREACHABLE("No specified `ic::type::Type` associated with `{}`") <<=
          {t};
    }
  }
};

}  // namespace ic

#endif  // ICARUS_IR_IR_H
