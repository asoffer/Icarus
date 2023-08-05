#include "base/file.h"

#include <cstdlib>

#include "absl/cleanup/cleanup.h"
#include "nth/debug/debug.h"
#include "nth/io/file.h"

namespace base {

std::optional<std::string> ReadFileToString(nth::file_path const& file_name) {
  std::optional<std::string> result = std::nullopt;

  auto save_errno              = std::exchange(errno, 0);
  absl::Cleanup errno_replacer = [&] { errno = save_errno; };

  std::optional<nth::file> file = nth::file::read_only(file_name);
  if (not file) { return result; }
  absl::Cleanup closer = [&] { NTH_ASSERT(file->close()); };

  std::fseek(file->get(), 0, SEEK_END);
  size_t file_size = file->tell();
  file->rewind();

  result.emplace();
  result->resize(file_size, '\0');
  (void)file->read_into(*result);
  return result;
}

}  // namespace base
