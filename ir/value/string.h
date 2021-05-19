#ifndef ICARUS_IR_VALUE_STRING_H
#define ICARUS_IR_VALUE_STRING_H

#include <iostream>
#include <string>
#include <string_view>

#include "ir/value/addr.h"
#include "ir/value/slice.h"

namespace ir {

struct String {
  // TODO: Should we allow this at all? Seems not exactly right.
  String() : String("") {}
  explicit String(std::string_view str);
  explicit String(char const* str);
  explicit String(std::string const& str);

  addr_t addr() const { return addr_; }
  Slice slice() const;

  std::string get() const;

  friend bool operator==(String lhs, String rhs) {
    return lhs.addr() == rhs.addr() or lhs.get() == rhs.get();
  }

  template <typename H>
  friend H AbslHashValue(H h, String s) {
    return H::combine(std::move(h), s.get());
  }

  friend std::ostream& operator<<(std::ostream& os, String s);

 private:
  ir::addr_t addr_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_STRING_H
