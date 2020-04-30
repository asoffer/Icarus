#include "compiler/constant/binding_tree.h"

#include "absl/strings/str_cat.h"
#include "base/stringify.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "type/util.h"

#include <string_view>
#include <vector>

namespace compiler {

void DebugStringImpl(ConstantBindingTree::Node const* node,
                     std::vector<std::string>& str_vec) {
  if (auto const* p = node->parent()) { DebugStringImpl(p, str_vec); }
  std::string_view sep = "";

  std::string s = absl::StrCat("\n|- with ",
#if defined(ICARUS_DEBUG)
                               node->num_children_,
#else
                               -1,
#endif
                               " children [");
  node->binding().ForEach([&](ast::Declaration const* decl, auto binding) {
    std::string val_as_str;
    type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                     uint32_t, uint64_t>(binding.type, [&](auto tag) {
      using T = typename decltype(tag)::type;
      using base::stringify;
      val_as_str = stringify(binding.buffer.template get<T>(0));
    });
    absl::StrAppend(&s, sep, decl->id(), ": ", binding.type->to_string(), " = ",
                    val_as_str);
    sep = ", ";
  });
  absl::StrAppend(&s, "]");
  str_vec.push_back(std::move(s));
}

std::string ConstantBindingTree::Node::DebugString() const {
  std::vector<std::string> str_vec;
  DebugStringImpl(this, str_vec);
  return absl::StrJoin(str_vec, "");
}

base::untyped_buffer_view ConstantBindingTree::Node::find_constant(
    ast::Declaration const* decl) {
  auto result = binding().get_constant(decl);
  if (not result.empty()) { return result; }
  if (auto* p = parent()) { return parent()->find_constant(decl); }
  return base::untyped_buffer_view{};
}

ConstantBindingTree::Node* ConstantBindingTree::AddChildTo(Node* parent) {
  nodes_.push_back(std::make_unique<Node>(parent));
  return nodes_.back().get();
}

}  // namespace compiler
