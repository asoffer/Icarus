#include "ir/value/string.h"

#include "ir/read_only_data.h"
#include "ir/str.h"

namespace ir {

String::String(char const* str) : String(std::string(str)) {}
String::String(std::string_view str) : String(std::string(str)) {}
String::String(std::string const& str)
    : addr_(GetString(SaveStringGlobally(str))) {}

std::string String::get() const { return ReadOnlyData.raw(addr_.rodata()); }

std::ostream& operator<<(std::ostream& os, String s) { return os << s.get(); }

}  // namespace ir
