#include "frontend/source.h"

#include <system_error>

namespace frontend {
base::expected<FileSrc> FileSrc::Make(std::filesystem::path path) {
  std::error_code ec;
  auto canonical_path = std::filesystem::canonical(path, ec);
  if (ec) {
    return base ::unexpected(std::string("Unable to open file \"") +
                             path.string() + "\"");
  }
  FILE *f = std::fopen(path.c_str(), "r");

  if (!f) {
    return base ::unexpected(std::string("Unable to open file \"") +
                             canonical_path.string() + "\"");
  }
  FileSrc src(std::move(path), f);
  return src;
}
}  // namespace frontend
