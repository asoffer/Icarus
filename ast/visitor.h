#ifndef ICARUS_AST_VISITOR_H
#define ICARUS_AST_VISITOR_H

#include <utility>

#include "ast/ast.h"
#include "ast/visitor_base.h"
#include "base/meta.h"

namespace ast {

template <typename...>
struct Visitor;

template <typename Tag, typename Ret, typename... Args>
struct Visitor<Tag, Ret(Args...)> : VisitorBase {
#define ICARUS_AST_NODE_X(node_type)                                           \
  void ErasedVisit(node_type const *node, void *erased_ret, void *erased_args) \
      final {                                                                  \
    if constexpr (base::meta<Ret> == base::meta<void>) {                       \
      static_cast<void>(erased_ret);                                           \
      if constexpr (sizeof...(Args) == 0) {                                    \
        this->Visit(Tag{}, node);                                              \
      } else if constexpr (sizeof...(Args) == 1) {                             \
        this->Visit(Tag{}, node,                                               \
                    std::forward<Args>(*static_cast<Args *>(erased_args))...); \
      } else {                                                                 \
        static_assert(base::always_false<Tag>(), "Currently unsupported");     \
      }                                                                        \
    } else {                                                                   \
      if constexpr (sizeof...(Args) == 0) {                                    \
        new (static_cast<Ret *>(erased_ret)) Ret(this->Visit(Tag{}, node));    \
      } else if constexpr (sizeof...(Args) == 1) {                             \
        this->Visit(Tag{}, node, std::forward<Args...>(erased_args));          \
      } else {                                                                 \
        static_assert(base::always_false<Tag>(), "Currently unsupported");     \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  virtual Ret Visit(Tag, node_type const *node, Args... args) {                \
    UNREACHABLE(#node_type);                                                   \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  Ret Visit(Node const *node, Args... args) {
    if constexpr (base::meta<Ret> == base::meta<void>) {
      if constexpr (sizeof...(Args) == 0) {
        node->Accept(this, nullptr, nullptr);
      } else if constexpr (sizeof...(Args) == 1) {
        node->Accept(this, nullptr, &args...);
      } else {
        static_assert(base::always_false<Tag>(), "Currently unsupported");
      }
    } else {
      alignas(alignof(Ret)) char r[sizeof(Ret)];
      if constexpr (sizeof...(Args) == 0) {
        node->Accept(this, r, nullptr);
      } else if constexpr (sizeof...(Args) == 1) {
        node->Accept(this, r, &args...);
      } else {
        static_assert(base::always_false<Tag>(), "Currently unsupported");
      }

      return *reinterpret_cast<Ret *>(r);
    }
  }
};

template <typename Ret, typename... Args>
struct Visitor<Ret(Args...)> : VisitorBase {
#define ICARUS_AST_NODE_X(node_type)                                           \
  void ErasedVisit(node_type const *node, void *erased_ret, void *erased_args) \
      final {                                                                  \
    if constexpr (base::meta<Ret> == base::meta<void>) {                       \
      static_cast<void>(erased_ret);                                           \
      if constexpr (sizeof...(Args) == 0) {                                    \
        this->Visit(node);                                                     \
      } else if constexpr (sizeof...(Args) == 1) {                             \
        this->Visit(node,                                                      \
                    std::forward<Args>(*static_cast<Args *>(erased_args))...); \
      } else {                                                                 \
        static_assert(base::always_false<Ret>(), "Currently unsupported");     \
      }                                                                        \
    } else {                                                                   \
      if constexpr (sizeof...(Args) == 0) {                                    \
        new (static_cast<Ret *>(erased_ret)) Ret(this->Visit(node));           \
      } else if constexpr (sizeof...(Args) == 1) {                             \
        this->Visit(node, std::forward<Args...>(erased_args));                 \
      } else {                                                                 \
        static_assert(base::always_false<Ret>(), "Currently unsupported");     \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  virtual Ret Visit(node_type const *node, Args... args) {                     \
    UNREACHABLE(#node_type);                                                   \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  Ret Visit(Node const *node, Args... args) {
    if constexpr (base::meta<Ret> == base::meta<void>) {
      if constexpr (sizeof...(Args) == 0) {
        node->Accept(this, nullptr, nullptr);
      } else if constexpr (sizeof...(Args) == 1) {
        node->Accept(this, nullptr, &args...);
      } else {
        static_assert(base::always_false<Ret>(), "Currently unsupported");
      }
    } else {
      alignas(alignof(Ret)) char r[sizeof(Ret)];
      if constexpr (sizeof...(Args) == 0) {
        node->Accept(this, r, nullptr);
      } else if constexpr (sizeof...(Args) == 1) {
        node->Accept(this, r, &args...);
      } else {
        static_assert(base::always_false<Ret>(), "Currently unsupported");
      }

      return *reinterpret_cast<Ret *>(r);
    }
  }

};

}  // namespace ast

#endif  // ICARUS_AST_VISITOR_H
