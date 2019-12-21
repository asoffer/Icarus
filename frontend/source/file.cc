#include "frontend/source/file.h"

#include "absl/strings/str_format.h"

namespace frontend {

base::expected<FileSource> FileSource::Make(CanonicalFileName file_name) {
  auto f = file_name.OpenReadOnly();
  if (not f) {
    return base::unexpected(
        absl::StrFormat(R"(Unable to open file "%s")", file_name.name()));
  }
  return FileSource(std::move(file_name), std::move(f));
}

}  // namespace frontend
