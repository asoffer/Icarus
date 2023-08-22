#ifndef ICARUS_BASE_FILE_H
#define ICARUS_BASE_FILE_H

#include <optional>
#include <string>

#include "nth/io/file_path.h"

namespace base {

// Reads a file from the filesystem with the name `file_name`. Returns its
// contents as a string if it exists, and `std::nullopt` otherwise.
std::optional<std::string> ReadFileToString(nth::file_path const& file_name);

}  // namespace base

#endif  // ICARUS_BASE_FILE_H