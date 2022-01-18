#include "diagnostic/message.h"

#include "absl/strings/str_cat.h"

namespace diagnostic::internal_text {

std::string Transform(type::Type t) {
  return absl::StrCat("\033[97;1m", t.to_string(), "\033[0m");
}

}  // namespace diagnostic::internal_text
