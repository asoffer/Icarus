#ifndef ICARUS_TYPE_CALLABLE_H
#define ICARUS_TYPE_CALLABLE_H

#include <vector>

#include "core/parameters.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace type {

struct Callable : LegacyType {
  core::Parameters<type::QualType> const& params() const { return parameters_; }

 protected:
  explicit Callable(int8_t which, LegacyType::Flags flags,
                    core::Parameters<type::QualType> parameters)
      : LegacyType(which, flags), parameters_(std::move(parameters)) {
#if defined(ICARUS_DEBUG)
    for (auto const& p : parameters_) { ASSERT(p.value != QualType::Error()); }
#endif  // defined(ICARUS_DEBUG)
  }

 private:
  core::Parameters<type::QualType> parameters_;
};

struct ReturningType : Callable {
  absl::Span<Type const> return_types() const { return return_types_; }
  bool eager() const { return eager_; }

 protected:
  explicit ReturningType(int8_t which, LegacyType::Flags flags,
                         core::Parameters<type::QualType> parameters,
                         std::vector<Type> return_types, bool eager)
      : Callable(which, flags, std::move(parameters)),
        return_types_(std::move(return_types)),
        eager_(eager) {
#if defined(ICARUS_DEBUG)
    for (Type t : return_types_) { ASSERT(t.valid() == true); }
#endif  // defined(ICARUS_DEBUG)
  }

 private:
  std::vector<Type> return_types_;
  bool eager_ = false;
};

}  // namespace type

#endif  // ICARUS_TYPE_CALLABLE_H
