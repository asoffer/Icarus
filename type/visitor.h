#ifndef ICARUS_TYPE_VISITOR_H
#define ICARUS_TYPE_VISITOR_H

#include <tuple>
#include <type_traits>
#include <utility>

#include "type/type.h"
#include "type/visitor_base.h"

namespace type {

template <typename...>
struct Visitor;

template <typename Tag, typename Ret, typename... Args>
struct Visitor<Tag, Ret(Args...)> : VisitorBase {
#define ICARUS_TYPE_TYPE_X(ty)                                                 \
  void ErasedVisit(ty const *t, void *erased_ret, void *erased_args) final {   \
    auto args_ptr =                                                            \
        static_cast<std::tuple<std::decay_t<Args>...> *>(erased_args);         \
                                                                               \
    if constexpr (base::meta<Ret> == base::meta<void>) {                       \
      static_cast<void>(erased_ret);                                           \
      std::apply(                                                              \
          [&](auto &... args) { this->Visit(Tag{}, t, std::move(args)...); },  \
          *args_ptr);                                                          \
    } else {                                                                   \
      new (static_cast<Ret *>(erased_ret)) Ret(std::apply(                     \
          [&](auto &... args) {                                                \
            return this->Visit(Tag{}, t, std::move(args)...);                  \
          },                                                                   \
          *args_ptr));                                                         \
    }                                                                          \
  }                                                                            \
                                                                               \
  virtual Ret Visit(Tag, ty const *t, Args... args) {                          \
    UNREACHABLE(#ty, typeid(Tag).name());                                      \
  }
  ICARUS_TYPE_TYPE_X(Generic<Struct>)
  ICARUS_TYPE_TYPE_X(Generic<Function>)
#include "type/type.xmacro.h"
#undef ICARUS_TYPE_TYPE_X

  Ret Visit(LegacyType const *t, Args... args) {
    if constexpr (base::meta<Ret> == base::meta<void>) {
      if constexpr (sizeof...(Args) == 0) {
        t->Accept(this, nullptr, nullptr);
      } else if constexpr (sizeof...(Args) == 1) {
        t->Accept(this, nullptr, &args...);
      } else {
        auto arg_tuple = std::make_tuple(std::move(args)...);
        t->Accept(this, nullptr, &arg_tuple);
      }
    } else {
      alignas(alignof(Ret)) char r[sizeof(Ret)];
      if constexpr (sizeof...(Args) == 0) {
        t->Accept(this, r, nullptr);
      } else if constexpr (sizeof...(Args) == 1) {
        t->Accept(this, r, &args...);
      } else {
        auto arg_tuple = std::make_tuple(std::move(args)...);
        t->Accept(this, r, &arg_tuple);
      }

      return *reinterpret_cast<Ret *>(r);
    }
  }
};

}  // namespace type

#endif  // ICARUS_TYPE_VISITOR_H
