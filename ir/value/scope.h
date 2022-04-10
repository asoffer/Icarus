#ifndef ICARUS_IR_VALUE_SCOPE_H
#define ICARUS_IR_VALUE_SCOPE_H

#include <string>
#include <vector>

#include "absl/types/span.h"
#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/serialize.h"
#include "ir/subroutine.h"
#include "ir/value/block.h"
#include "ir/value/reg.h"
#include "type/scope.h"

namespace ast {
struct ScopeLiteral;
}  // namespace ast

namespace ir {

struct Scope : base::Extend<Scope, 1>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "Scope(%p)";

  struct Data {
    ast::ScopeLiteral const *literal;
    Subroutine *scope;
    type::Scope const *type;
    absl::flat_hash_map<Block, std::vector<Reg>> parameters;
    ByteCode *byte_code;
  };

  constexpr Scope() : data_(nullptr) {}
  explicit constexpr Scope(Scope::Data *data) : data_(ASSERT_NOT_NULL(data)) {}

  Subroutine *operator->() { return get().scope; }
  Subroutine &operator*() { return *get().scope; }

  explicit operator bool() { return data_; }

  void add_parameters(Block b, Reg r) {
    return data_->parameters[b].push_back(r);
  }
  absl::Span<Reg const> parameters(Block b) { return data_->parameters[b]; }

  type::Scope const *type() const { return ASSERT_NOT_NULL(get().type); }

  ByteCode const &byte_code() const { return *data_->byte_code; }

 private:
  friend Subroutine;
  friend base::EnableExtensions;

  Data const &get() const { return *ASSERT_NOT_NULL(data_); }

  Data *data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_H
