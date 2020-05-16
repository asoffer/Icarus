#ifndef ICARUS_FRONTEND_SOURCE_H
#define ICARUS_FRONTEND_SOURCE_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "base/cast.h"
#include "base/expected.h"

namespace frontend {

struct SourceChunk {
  std::string_view view = "";
  bool more_to_read     = true;
};

struct Source : base::Cast<Source> {
  virtual ~Source(){};

  template <typename T, typename... Args>
  static T* Make(Args&&... args) {
    // TODO Don't keep these around forever.
    return new T(std::forward<Args>(args)...);
  }

  // Reads data from the source until the delimeter is found, dropping the
  // delimeter Sources may have a maximum number of characters they will read.
  // Typically 1k.
  //
  // Returned data contains a string_view which is guaranteed to be valid until
  // the next call to ReadUntil.
  virtual SourceChunk ReadUntil(char delim) = 0;

  virtual std::vector<std::string> LoadLines() const = 0;

 private:
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_H
