#ifndef ICARUS_MISC_CONSTANT_BINDING_H
#define ICARUS_MISC_CONSTANT_BINDING_H

#include <utility>
#include <variant>

#include "absl/container/flat_hash_map.h"
#include "base/untyped_buffer.h"
#include "core/bytes.h"

namespace ast {
struct Declaration;
}  // namespace ast

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct Results;
}  // namespace ir

namespace compiler {
struct Compiler;
}  // namespace compiler

struct ConstantBinding {
 public:
  size_t size() const { return keys_.size(); }

  bool contains(ast::Declaration const* decl) const {
    return keys_.contains(decl);
  }

  type::Type const* type_of(ast::Declaration const* decl) const {
    if (auto iter = keys_.find(decl); iter != keys_.end()) {
      return iter->second.type_;
    }
    return nullptr;
  }

  std::variant<ir::Results, std::pair<size_t, core::Bytes>> reserve_slot(
      ast::Declaration const* decl, type::Type const* t);
  ir::Results set_slot(size_t offset, void const* data, core::Bytes bytes);

  ir::Results get_constant(ast::Declaration const* decl) const;

 private:
  friend bool operator==(ConstantBinding const& lhs,
                         ConstantBinding const& rhs);
  friend struct compiler::Compiler;  // TODO remove me.

  struct Binding {
    type::Type const * type_;
    size_t offset_;
  };

  absl::flat_hash_map<ast::Declaration const*, Binding> keys_;
  base::untyped_buffer buf_;
};

inline bool operator!=(ConstantBinding const& lhs, ConstantBinding const& rhs) {
  return !(lhs == rhs);
}

#endif  // ICARUS_MISC_CONSTANT_BINDING_H
