#ifndef ICARUS_BASE_EXTEND_H
#define ICARUS_BASE_EXTEND_H

#include <tuple>
#include <type_traits>

#include "base/meta.h"

namespace base {

namespace internal {

struct ConvertibleToAnything {
  constexpr ConvertibleToAnything() {}
  template <typename T>
  operator T &() const;
  template <typename T>
  operator T &&() const;
};

template <typename T>
struct ConvertibleToAnythingBut {
  ConvertibleToAnythingBut();
  template <typename U,
            std::enable_if_t<base::meta<T> != base::meta<U>, int> = 0>
  operator U &() const;
  template <typename U,
            std::enable_if_t<base::meta<T> != base::meta<U>, int> = 0>
  operator U &&() const;
};

struct AcceptAnything {
  AcceptAnything(AcceptAnything const &);
  AcceptAnything(...);
};

std::false_type IsBraceInitializableWith(void *, AcceptAnything);

struct NotDefaultConstructible {
  NotDefaultConstructible() = delete;
  explicit NotDefaultConstructible(int);
};

template <typename T>
struct TestInitializer {
  T value;
  NotDefaultConstructible not_default_constructible;
};

template <typename T>
std::true_type IsBraceInitializableWith(T*, TestInitializer<T>);

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-braces"
template <typename T, typename... Args>
constexpr int NumInitializers(Args... args) {
  if constexpr (decltype(IsBraceInitializableWith(
                    static_cast<T *>(nullptr),
                    {ConvertibleToAnythingBut<T>(), Args{}...}))::value) {
    return sizeof...(Args);
  } else {
    return NumInitializers<T>(args..., ConvertibleToAnything());
  }
}
#pragma clang diagnostic pop

// TODO: Support reference and cv-qualified fields and R-value structs.
template <typename T, int NumBases>
auto GetFields(T &t) {
  constexpr int kNumFields = NumInitializers<T>() - NumBases;
  if constexpr (kNumFields == 0) {
    return std::tie();
  } else if constexpr (kNumFields == 1) {
    auto &[field0] = t;
    return std::tie(field0);
  } else if constexpr (kNumFields == 2) {
    auto &[field0, field1] = t;
    return std::tie(field0, field1);
  } else if constexpr (kNumFields == 3) {
    auto &[field0, field1, field2] = t;
    return std::tie(field0, field1, field2);
  } else if constexpr (kNumFields == 4) {
    auto &[field0, field1, field2, field3] = t;
    return std::tie(field0, field1, field2, field3);
  }
}

template <typename T, int NumBases>
auto GetFields(T const &t) {
  constexpr int kNumFields = NumInitializers<T>() - NumBases;
  if constexpr (kNumFields == 0) {
    return std::tie();
  } else if constexpr (kNumFields == 1) {
    auto &[field0] = t;
    return std::tie(field0);
  } else if constexpr (kNumFields == 2) {
    auto &[field0, field1] = t;
    return std::tie(field0, field1);
  } else if constexpr (kNumFields == 3) {
    auto &[field0, field1, field2] = t;
    return std::tie(field0, field1, field2);
  } else if constexpr (kNumFields == 4) {
    auto &[field0, field1, field2, field3] = t;
    return std::tie(field0, field1, field2, field3);
  }
}


}  // namespace internal

template <typename T>
struct Extend final {
  template <template <typename> typename... Extensions>
  struct With : Extensions<T>... {
    auto field_refs() & {
      return internal::GetFields<T, 1>(static_cast<T &>(*this));
    }
    auto field_refs() const & {
      return internal::GetFields<T, 1>(static_cast<T const &>(*this));
    }
  };
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_H
