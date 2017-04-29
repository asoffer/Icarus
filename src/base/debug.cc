#include "debug.h"

namespace debug {
std::string to_string(const char *s) { return std::string(s); }
std::string to_string(std::string s) { return std::move(s); }
}
