#ifndef ICARUS_IR_VALUE_STRING_H
#define ICARUS_IR_VALUE_STRING_H

#include <string>
#include <string_view>

#include "ir/value/addr.h"

namespace ir {

struct String {
  explicit String(std::string_view str = "");
  explicit String(char const * str);
  explicit String(std::string const &str);

  Addr addr() const { return addr_; }

  std::string get() const;

  friend bool operator==(String lhs, String rhs) {
    return lhs.addr() == rhs.addr() or lhs.get() == rhs.get();
  }

  friend std::ostream& operator<<(std::ostream& os, String s);

 private:
  ir::Addr addr_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_STRING_H
