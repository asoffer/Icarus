#ifndef ICARUS_TYPE_CALLABLE_H
#define ICARUS_TYPE_CALLABLE_H

#include <vector>

#include "core/params.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace type {

struct Callable : LegacyType {
  core::Params<type::QualType> const& params() const { return parameters_; }

 protected:
  explicit Callable(LegacyType::Flags flags,
                    core::Params<type::QualType> parameters)
      : LegacyType(flags), parameters_(std::move(parameters)) {
#if defined(ICARUS_DEBUG)
    for (auto const& p : parameters_) { ASSERT(p.value != QualType::Error()); }
#endif  // defined(ICARUS_DEBUG)
  }

 private:
  core::Params<type::QualType> parameters_;
};

struct ReturningType : Callable {
  absl::Span<Type const> return_types() const { return return_types_; }

 protected:
  explicit ReturningType(LegacyType::Flags flags,
                         core::Params<type::QualType> parameters,
                         std::vector<Type> return_types)
      : Callable(flags, std::move(parameters)),
        return_types_(std::move(return_types)) {
#if defined(ICARUS_DEBUG)
    for (Type t : return_types_) { ASSERT(t.valid() == true); }
#endif  // defined(ICARUS_DEBUG)
  }

 private:
  std::vector<Type> return_types_;
};

}  // namespace type

#endif  // ICARUS_TYPE_CALLABLE_H
