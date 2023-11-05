#ifndef ICARUS_COMMON_FILE_H
#define ICARUS_COMMON_FILE_H

#include <optional>
#include <string>

#include "nth/io/file_path.h"

namespace ic {

std::optional<std::string> ReadFileToString(nth::file_path const& file_name);

}  // namespace ic

#endif // ICARUS_COMMON_FILE_H
