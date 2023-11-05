#include "common/file.h"

#include <cstdio>
#include <optional>
#include <string>

#include "common/errno.h"
#include "nth/debug/debug.h"
#include "nth/io/file.h"
#include "nth/io/file_path.h"

namespace ic {

std::optional<std::string> ReadFileToString(nth::file_path const& file_name) {
  errno_resetter e;
  std::optional<std::string> result = std::nullopt;
  std::optional<nth::file> file = nth::file::read_only(file_name);
  if (file) {
    std::fseek(file->get(), 0, SEEK_END);
    size_t file_size = file->tell();
    file->rewind();

    result.emplace();
    result->resize(file_size, '\0');
    (void)file->read_into(*result);
    NTH_REQUIRE((v.always), file->close());
  }
  return result;
}

}  // namespace ic
