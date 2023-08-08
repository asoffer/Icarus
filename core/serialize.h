#ifndef ICARUS_CORE_SERIALIZE_H
#define ICARUS_CORE_SERIALIZE_H

#include "nth/base/attributes.h"

namespace core {

template <typename T>
struct Deserializer {
  explicit Deserializer(NTH_ATTRIBUTE(lifetimebound)
                            typename T::icarus_serialization_type const& input)
      : input_(input) {}
  auto const& input() const { return input_; }

 private:
  typename T::icarus_serialization_type const& input_;
};

template <typename T>
struct Serializer {
  explicit Serializer(typename T::icarus_serialization_type& output)
      : output_(output) {}
  auto& output() { return output_; }

 private:
  typename T::icarus_serialization_type& output_;
};

}  // namespace core

#endif  // ICARUS_CORE_SERIALIZE_H
