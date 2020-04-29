#ifndef ICARUS_COMPILER_CONSTANT_BINDING_H
#define ICARUS_COMPILER_CONSTANT_BINDING_H

#include <utility>
#include <variant>

#include "absl/container/flat_hash_map.h"
#include "ast/ast_fwd.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "type/type.h"

namespace compiler {

struct ConstantBinding {
 public:
  size_t size() const { return bindings_.size(); }

  type::Type const* type_of(ast::Declaration const* decl) const {
    if (auto iter = bindings_.find(decl); iter != bindings_.end()) {
      return iter->second.type_;
    }
    return nullptr;
  }

  base::untyped_buffer_view reserve_slot(ast::Declaration const* decl,
                                         type::Type const* t);
  void set_slot(ast::Declaration const* decl, base::untyped_buffer_view buf);

  base::untyped_buffer_view get_constant(ast::Declaration const* decl) const;

 private:
  struct Binding {
    type::Type const* type_;
    base::untyped_buffer buf_;
  };

  // Note: While the `Binding` itself may move around on rehash, the underlying
  // untyped_buffer storage will not move.
  absl::flat_hash_map<ast::Declaration const*, Binding> bindings_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_CONSTANT_BINDING_H
