#ifndef ICARUS_IR_VALUE_SLICE_H
#define ICARUS_IR_VALUE_SLICE_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/compare.h"
#include "ir/value/addr.h"

namespace ir {

struct Slice : base::Extend<Slice, 2>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension,
                                            base::TotalOrderExtension> {
  static constexpr std::string_view kAbslFormatString = "Slice(%p, %u)";

  Slice() = default;
  explicit Slice(ir::addr_t data, uint64_t length)
      : data_(data), length_(length) {}

  addr_t data() const { return data_; }
  uint64_t length() const { return length_; }

  operator std::string_view() const {
    return std::string_view(reinterpret_cast<char const *>(data_), length_);
  }

 private:
  friend base::EnableExtensions;

  ir::addr_t data_;
  uint64_t length_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SLICE_H
