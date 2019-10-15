#include "frontend/source/file.h"

#include <system_error>

#include "absl/strings/str_format.h"

namespace frontend {
base::expected<FileSource> FileSource::Make(std::filesystem::path path) {
  std::error_code ec;
  auto canonical_path = std::filesystem::canonical(path, ec);
  if (ec) {
    return base::unexpected(
        absl::StrFormat(R"(Unable to open file "%s")", path.string()));
  }
  FILE *f = std::fopen(path.c_str(), "r");

  if (not f) {
    return base::unexpected(absl::StrFormat(R"(Unable to open file "%s")",
                                            canonical_path.string()));
  }
  return FileSource(std::move(path), f);
}
}  // namespace frontend
