#ifndef ICARUS_BASE_OPTIONAL_H
#define ICARUS_BASE_OPTIONAL_H

#include <utility>

namespace base {
struct optional_t {};
extern optional_t none;

template <typename T> struct optional {
public:
  optional(optional_t) : present_(false) {}
  optional(const T &t) : value_(t), present_(true) {}
  optional(T &&t) : value_(std::move(t)), present_(true) {}

  const T &operator*() const { return value_; }
  T &operator*() { return value_; }

  operator bool () const { return present_; }

private:
  T value_;
  bool present_ = false;
};

} // namespace base
#endif // ICARUS_BASE_OPTIONAL_H
