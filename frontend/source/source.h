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
    auto src  = std::make_unique<T>(std::forward<Args>(args)...);
    auto* ptr = src.get();
    sources_.insert(std::move(src));
    return ptr;
  }

  // Reads data from the source until the delimeter is found, dropping the
  // delimeter Sources may have a maximum number of characters they will read.
  // Typically 1k.
  //
  // Returned data contains a string_view which is guaranteed to be valid until
  // the next call to ReadUntil.
  virtual SourceChunk ReadUntil(char delim) = 0;

  virtual std::vector<std::string> LoadLines() = 0;

 private:
  // TODO Just keeping all the sources around forever is a bad idea. For sources
  // that own file handles, we should close them. If a repl is open long enough,
  // lines should be written out to memory. If we need to emit diagnostics,
  // reopening and skipping forward to the approripate point in the file is
  // probably preferable.
  static absl::flat_hash_set<std::unique_ptr<Source>> sources_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_H
