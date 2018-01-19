#ifndef ICARUS_AST_DISPATCH_H
#define ICARUS_AST_DISPATCH_H

#include <map>
#include <string>

struct Type;

namespace AST {
struct Declaration;
struct Expression;

struct CallArgTypes {
  auto find(const std::string &name) -> decltype(auto) {
    auto iter = named_.begin();
    for (; iter != named_.end(); ++iter) {
      if (iter->first == name) { return iter; }
    }
    return iter;
  }
  auto find(const std::string &name) const -> decltype(auto) {
    auto iter = named_.begin();
    for (; iter != named_.end(); ++iter) {
      if (iter->first == name) { return iter; }
    }
    return iter;
  }

  std::vector<Type *> pos_;

  // TODO implement flat map for real
  std::vector<std::pair<std::string, Type *>> named_;

  // TODO this map data structure is getting too complicated
  std::string to_string() const;
};
bool operator<(const CallArgTypes &lhs, const CallArgTypes &rhs);

// Represents a particular call resolution.
struct Binding {
  bool defaulted(size_t i) const { return exprs_[i].second == nullptr; }

  Expression *fn_expr_;
  std::vector<std::pair<Type *, Expression *>> exprs_;
};

struct DispatchTable {
  // TODO come up with a good internal representaion.
  // * Can/should this be balanced to find the right type-check sequence in a
  //   streaming manner?
  // * Add weights for PGO optimizations?

  void insert(CallArgTypes call_arg_types, Binding binding);

  std::map<CallArgTypes, Binding> bindings_;
  size_t total_size_ = 0;
};
} // namespace AST

#endif // ICARUS_AST_DISPATCH_H
