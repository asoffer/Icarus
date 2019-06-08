#ifndef ICARUS_IR_ARGUMENTS_H
#define ICARUS_IR_ARGUMENTS_H

#include <string>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/untyped_buffer.h"
#include "ir/results.h"

namespace type {
struct Type;
struct Callable;
}  // namespace type

namespace ir {
struct Arguments {
  Arguments(type::Callable const *c) : type_(c) {}
  Arguments(type::Callable const *c, Results results)
      : type_(c), results_(std::move(results)) {}

  std::string to_string() const;
  base::untyped_buffer PrepareCallBuffer(
      absl::flat_hash_map<Reg, size_t> const &reg_to_offset,
      base::untyped_buffer const &regs);

  std::vector<type::Type const *> const &input_types() const;
  Results const &results() const { return results_; }

 // private:
  type::Callable const *type_ = nullptr;
  Results results_;
};
}  // namespace ir

#endif  // ICARUS_IR_ARGUMENTS_H
