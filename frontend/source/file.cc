#include "frontend/source/file.h"

#include <cerrno>
#include <cstring>

#include "absl/strings/str_format.h"

namespace frontend {

base::expected<FileSource> FileSource::Make(CanonicalFileName file_name) {
  auto f = file_name.OpenReadOnly();
  if (not f) {
    return base::unexpected(absl::StrFormat(R"(Failed to open file "%s": %s)",
                                            file_name.name(),
                                            std::strerror(errno)));
  }
  return FileSource(std::move(file_name), std::move(f));
}

}  // namespace frontend
