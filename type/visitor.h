#ifndef ICARUS_TYPE_VISITOR_H
#define ICARUS_TYPE_VISITOR_H

#include <tuple>
#include <type_traits>
#include <utility>

#include "type/type.h"
#include "type/visitor_base.h"

namespace type {

template <typename>
struct SingleVisitor;

template <typename Ret, typename... Args>
struct SingleVisitor<Ret(Args...)> : VisitorBase {
  ~SingleVisitor() override {}

#define ICARUS_TYPE_TYPE_X(subtype)                                            \
  void ErasedVisit(subtype const *type, void *erased_ret,                      \
                   void *erased_arg_tuple) final {                             \
    static_cast<void>(erased_ret);                                             \
    auto *tup_ptr = static_cast<std::tuple<Args...> *>(erased_arg_tuple);      \
    if constexpr (std::is_void_v<Ret>) {                                       \
      std::apply(                                                              \
          [this, type](auto &&... call_args) {                                 \
            this->Visit(type,                                                  \
                        std::forward<decltype(call_args)>(call_args)...);      \
          },                                                                   \
          std::move(*tup_ptr));                                                \
    } else {                                                                   \
      auto *ret_ptr = static_cast<Ret *>(erased_ret);                          \
      *ret_ptr      = std::apply(                                              \
          [this, type](auto &&... call_args) {                            \
            return this->Visit(                                           \
                type, std::forward<decltype(call_args)>(call_args)...);   \
          },                                                              \
          std::move(*tup_ptr));                                           \
    }                                                                          \
  }                                                                            \
                                                                               \
  virtual Ret Visit(subtype const *type, Args... args) {                       \
    UNREACHABLE(#subtype);                                                     \
  }
#include "type/type.xmacro.h"
#undef ICARUS_TYPE_TYPE_X

  Ret Visit(Type const *type, Args... args) {
    if constexpr (std::is_void_v<Ret>) {
      std::tuple<Args...> arg_tup(std::forward<Args>(args)...);
      type->Accept(this, nullptr, &arg_tup);
    } else {
      Ret r;
      std::tuple<Args...> arg_tup(std::forward<Args>(args)...);
      type->Accept(this, &r, &arg_tup);
      return r;
    }
  }
};

template <typename... Signatures>
struct Visitor : SingleVisitor<Signatures>... {};

}  // namespace type

#endif  // ICARUS_TYPE_VISITOR_H
