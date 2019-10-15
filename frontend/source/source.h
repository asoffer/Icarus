#ifndef ICARUS_FRONTEND_SOURCE_H
#define ICARUS_FRONTEND_SOURCE_H

#include <string>
#include <string_view>
#include <vector>

#include "base/cast.h"
#include "base/expected.h"

namespace frontend {

struct SourceChunk {
  std::string_view view = "";
  bool more_to_read     = true;
};

struct Source : base::Cast<Source> {
  virtual ~Source(){};

  // Reads data from the source until the delimeter is found, dropping the
  // delimeter Sources may have a maximum number of characters they will read.
  // Typically 1k.
  //
  // Returned data contains a string_view which is guaranteed to be valid until
  // the next call to ReadUntil.
  virtual SourceChunk ReadUntil(char delim) = 0;

  virtual std::vector<std::string> LoadLines() = 0;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_H
