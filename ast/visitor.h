#ifndef ICARUS_AST_VISITOR_H
#define ICARUS_AST_VISITOR_H

#include <tuple>
#include <type_traits>
#include <utility>

#include "ast/ast.h"
#include "ast/visitor_base.h"

namespace ast {

template <typename>
struct SingleVisitor;

template <typename Ret, typename... Args>
struct SingleVisitor<Ret(Args...)> : VisitorBase {
  ~SingleVisitor() override {}

#define ICARUS_AST_NODE_X(node_type)                                           \
  void ErasedVisit(node_type const *node, void *erased_ret,                    \
                   void *erased_arg_tuple) final {                             \
    auto *tup_ptr = static_cast<std::tuple<Args...> *>(erased_arg_tuple);      \
    if constexpr (std::is_void_v<Ret>) {                                       \
      std::apply(                                                              \
          [this, node](auto &&... call_args) {                                 \
            Visit(node, std::forward<std::decay_t<decltype(call_args)>>(       \
                            call_args)...);                                    \
          },                                                                   \
          std::move(*tup_ptr));                                                \
    } else {                                                                   \
      auto *ret_ptr = static_cast<Ret *>(erased_ret);                          \
      *ret_ptr      = std::apply(                                              \
          [this, node](auto &&... call_args) {                            \
            return Visit(node,                                            \
                         std::forward<std::decay_t<decltype(call_args)>>( \
                             call_args)...);                              \
          },                                                              \
          std::move(*tup_ptr));                                           \
    }                                                                          \
  }                                                                            \
                                                                               \
  virtual Ret Visit(node_type const *node, Args... args) {                     \
    UNREACHABLE(#node_type);                                                   \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  Ret Visit(Node const *node, Args... args) {
    if constexpr (std::is_void_v<Ret>) {
      std::tuple<Args...> arg_tup(std::forward<Args>(args)...);
      node->Accept(this, nullptr, &arg_tup);
    } else {
      Ret r;
      std::tuple<Args...> arg_tup(std::forward<Args>(args)...);
      node->Accept(this, &r, &arg_tup);
      return r;
    }
  }
};

template <typename... Signatures>
struct Visitor : SingleVisitor<Signatures>... {};

}  // namespace ast

#endif  // ICARUS_AST_VISITOR_H
