#ifndef ICARUS_BASE_EXTEND_H
#define ICARUS_BASE_EXTEND_H

#include <iostream>
#include <tuple>
#include <type_traits>

#include "base/meta.h"

namespace base {

namespace internal_extend {

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
  template <typename U, std::enable_if_t<nth::type<T> != nth::type<U>, int> = 0>
  operator U &() const;
  template <typename U, std::enable_if_t<nth::type<T> != nth::type<U>, int> = 0>
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
std::true_type IsBraceInitializableWith(T *, TestInitializer<T>);

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

}  // namespace internal_extend

struct EnableExtensions {
  template <typename T, int NumBases, int NumFields>
  static auto field_refs(T &t) {
    constexpr int kNumFields = [] {
      if constexpr (NumFields == -1) {
        return internal_extend::NumInitializers<T>() - NumBases;
      } else {
        return NumFields;
      }
    }();
    if constexpr (kNumFields == 0) {
      return std::tie();
    } else if constexpr (kNumFields == 1) {
      auto &[f0] = t;
      return std::tie(f0);
    } else if constexpr (kNumFields == 2) {
      auto &[f0, f1] = t;
      return std::tie(f0, f1);
    } else if constexpr (kNumFields == 3) {
      auto &[f0, f1, f2] = t;
      return std::tie(f0, f1, f2);
    } else if constexpr (kNumFields == 4) {
      auto &[f0, f1, f2, f3] = t;
      return std::tie(f0, f1, f2, f3);
    } else if constexpr (kNumFields == 5) {
      auto &[f0, f1, f2, f3, f4] = t;
      return std::tie(f0, f1, f2, f3, f4);
    } else if constexpr (kNumFields == 6) {
      auto &[f0, f1, f2, f3, f4, f5] = t;
      return std::tie(f0, f1, f2, f3, f4, f5);
    } else if constexpr (kNumFields == 7) {
      auto &[f0, f1, f2, f3, f4, f5, f6] = t;
      return std::tie(f0, f1, f2, f3, f4, f5, f6);
    } else if constexpr (kNumFields == 8) {
      auto &[f0, f1, f2, f3, f4, f5, f6, f7] = t;
      return std::tie(f0, f1, f2, f3, f4, f5, f6, f7);
    }
  }

  template <typename T, int NumBases, int NumFields>
  static auto field_refs(T const &t) {
    constexpr int kNumFields = [] {
      if constexpr (NumFields == -1) {
        return internal_extend::NumInitializers<T>() - NumBases;
      } else {
        return NumFields;
      }
    }();
    if constexpr (kNumFields == 0) {
      return std::tie();
    } else if constexpr (kNumFields == 1) {
      auto &[f0] = t;
      return std::tie(f0);
    } else if constexpr (kNumFields == 2) {
      auto &[f0, f1] = t;
      return std::tie(f0, f1);
    } else if constexpr (kNumFields == 3) {
      auto &[f0, f1, f2] = t;
      return std::tie(f0, f1, f2);
    } else if constexpr (kNumFields == 4) {
      auto &[f0, f1, f2, f3] = t;
      return std::tie(f0, f1, f2, f3);
    } else if constexpr (kNumFields == 5) {
      auto &[f0, f1, f2, f3, f4] = t;
      return std::tie(f0, f1, f2, f3, f4);
    } else if constexpr (kNumFields == 6) {
      auto &[f0, f1, f2, f3, f4, f5] = t;
      return std::tie(f0, f1, f2, f3, f4, f5);
    } else if constexpr (kNumFields == 7) {
      auto &[f0, f1, f2, f3, f4, f5, f6] = t;
      return std::tie(f0, f1, f2, f3, f4, f5, f6);
    } else if constexpr (kNumFields == 8) {
      auto &[f0, f1, f2, f3, f4, f5, f6, f7] = t;
      return std::tie(f0, f1, f2, f3, f4, f5, f6, f7);
    }
  }
};

namespace internal_extend {

template <typename T>
auto GetDependencies(T *) -> typename T::dependencies;
auto GetDependencies(void *) -> type_list<>;

template <typename... Processed>
auto DependenciesImpl(type_list<>, type_list<Processed...>) {
  return type_list<Processed...>{};
}

template <typename T, typename... Ts, typename... Processed>
auto DependenciesImpl(type_list<T, Ts...>, type_list<Processed...>) {
  if constexpr (((nth::type<T> == nth::type<Processed>) || ...)) {
    return DependenciesImpl(type_list<Ts...>{}, type_list<Processed...>{});
  } else {
    using deps = decltype(GetDependencies(static_cast<T *>(nullptr)));
    if constexpr (nth::type<deps> == nth::type<type_list<>>) {
      return DependenciesImpl(type_list<Ts...>{}, type_list<T, Processed...>{});
    } else {
      return DependenciesImpl(
          FromSeq<ToSeq(deps{}) + nth::type_sequence<Ts...>>{},
          type_list<T, Processed...>{});
    }
  }
}

template <typename ExtensionsTypeList>
struct ExtensionSet;
template <typename... Extensions>
struct ExtensionSet<type_list<Extensions...>> : Extensions... {};

template <typename... Deps>
using AllDependencies = decltype(internal_extend::DependenciesImpl(
    type_list<Deps...>{}, type_list<>{}));

}  // namespace internal_extend

template <typename T, int NumFields = -1>
struct Extend final {
  template <template <typename> typename... Extensions>
  struct With : internal_extend::ExtensionSet<
                    internal_extend::AllDependencies<Extensions<T>...>> {
    auto field_refs() & {
      return EnableExtensions::field_refs<T, 1, NumFields>(
          static_cast<T &>(*this));
    }
    auto field_refs() const & {
      return EnableExtensions::field_refs<T, 1, NumFields>(
          static_cast<T const &>(*this));
    }
  };
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_H
