#ifndef ICARUS_BASE_EXTEND_H
#define ICARUS_BASE_EXTEND_H

#include <iostream>
#include <tuple>
#include <type_traits>

#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

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
  template <typename U>
  requires(nth::type<T> != nth::type<U>) operator U &() const;
  template <typename U>
  requires(nth::type<T> != nth::type<U>) operator U &&() const;
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

constexpr auto DependenciesImpl(nth::Sequence auto unprocessed,
                                nth::Sequence auto processed) {
  if constexpr (unprocessed.empty()) {
    return processed;
  } else {
    constexpr auto head = unprocessed.head();
    constexpr auto tail = unprocessed.tail();
    if constexpr (processed.template contains<head>()) {
      return DependenciesImpl(tail, processed);
    } else if constexpr (requires {
                           nth::type_t<decltype(head){}>::dependencies;
                         }) {
      return DependenciesImpl(
          decltype(tail){} + nth::type_t<head>::dependencies,
          nth::sequence<head> + processed);
    } else {
      return DependenciesImpl(decltype(tail){},
                              nth::sequence<head> + processed);
    }
  }
}

template <typename ExtensionsTypeList>
struct ExtensionSet;
template <typename... Extensions>
struct ExtensionSet<void(Extensions...)> : Extensions... {};

template <typename... Deps>
inline constexpr auto AllDependencies = internal_extend::DependenciesImpl(
    nth::type_sequence<Deps...>, nth::sequence<>);

}  // namespace internal_extend

template <typename T, int NumFields = -1>
struct Extend final {
  template <template <typename> typename... Extensions>
  struct With : internal_extend::ExtensionSet<nth::type_t<
                    internal_extend::AllDependencies<Extensions<T>...>.reduce(
                        [](auto... ts) {
                          return nth::type<void(nth::type_t<ts>...)>;
                        })>> {
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
