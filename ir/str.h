#ifndef ICARUS_IR_STR_H
#define ICARUS_IR_STR_H

// TODO rename this... to what?
#include <string_view>
#include "ir/value/addr.h"

namespace ir {
std::string_view SaveStringGlobally(std::string const &str);
Addr GetString(std::string const &str);
Addr GetString(std::string_view str);
}  // namespace ir

#endif  // ICARUS_IR_STR_H
