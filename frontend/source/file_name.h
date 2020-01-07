#ifndef ICARUS_FRONTEND_SOURCE_FILE_NAME_H
#define ICARUS_FRONTEND_SOURCE_FILE_NAME_H

#include <cstdio>
#include <memory>
#include <string>
#include <string_view>

#include "base/expected.h"
#include "base/strong_types.h"

namespace frontend {

ICARUS_BASE_DEFINE_STRONG_TYPE(FileName, std::string{});

using file_handle_t = std::unique_ptr<std::FILE, void (*)(std::FILE*)>;

// CanonicalFileName
//
// Represents a file name that has already been canonicalized and therefore can
// be safely compared for equality/hashed.
struct CanonicalFileName {
  static base::expected<CanonicalFileName> Make(FileName name);

  template <typename H>
  friend H AbslHashValue(H h, CanonicalFileName const& name) {
    return H::combine(std::move(h), name.name_);
  }

  std::string_view name() const { return name_; }

  file_handle_t OpenReadOnly() const {
    std::FILE* file = std::fopen(name_.c_str(), "r");
    auto deleter    = +[](std::FILE* f) {
      if (f) { std::fclose(f); }
    };
    return std::unique_ptr<std::FILE, void (*)(std::FILE*)>(file, deleter);
  }

  friend inline bool operator==(CanonicalFileName const& lhs,
                                CanonicalFileName const& rhs) {
    return lhs.name_ == rhs.name_;
  }
  friend inline bool operator!=(CanonicalFileName const& lhs,
                                CanonicalFileName const& rhs) {
    return !(lhs == rhs);
  }

 private:
  explicit CanonicalFileName(std::string name) : name_(std::move(name)) {}

  std::string name_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_FILE_NAME_H
