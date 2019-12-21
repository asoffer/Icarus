#include "frontend/source/file_name.h"

#include <filesystem>
#include <system_error>

#include "absl/strings/str_format.h"

namespace frontend {

base::expected<CanonicalFileName> CanonicalFileName::Make(FileName name) {
  std::error_code ec;
  std::filesystem::path p(std::move(name).value);
  auto canonical_path = std::filesystem::canonical(p, ec);

  if (ec) {
    return base::unexpected(
        absl::StrFormat(R"(Unable to open file "%s")", p.string()));
  }

  return CanonicalFileName{canonical_path.string()};
}

}  // namespace frontend
