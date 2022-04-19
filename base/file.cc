#include "base/file.h"

#include <cstdlib>

#include "absl/cleanup/cleanup.h"

namespace base {

std::optional<std::string> ReadFileToString(std::string const& file_name) {
  std::optional<std::string> result = std::nullopt;
  auto save_errno                   = std::exchange(errno, 0);
  std::FILE* file                   = std::fopen(file_name.c_str(), "r");
  absl::Cleanup errno_replacer      = [&] { errno = save_errno; };

  if (not file) { return std::nullopt; }
  absl::Cleanup closer = [&] { std::fclose(file); };

  std::fseek(file, 0, SEEK_END);
  size_t file_size = std::ftell(file);
  std::rewind(file);

  result.emplace();
#if defined(__cpp_lib_string_resize_and_overwrite)
  result->resize_and_overwrite(file_size, [&](char* buffer, size_t size) {
    std::fread(buffer, sizeof(char), file_size, file);
  });
#else
  result->resize(file_size, '\0');
  std::fread(result->data(), sizeof(char), file_size, file);
#endif
  return result;
}

}  // namespace base
