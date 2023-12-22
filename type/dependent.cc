#include "type/dependent.h"

#include <queue>

#include "ir/function.h"
#include "jasmin/core/execute.h"
#include "nth/container/stack.h"

namespace ic::type::internal_dependent {

Term Term::DeBruijnIndex(uint16_t index) {
  Term term;
  term.nodes_.push_back({
      .kind         = Node::Kind::DeBruijnIndex,
      .index        = index,
      .subtree_size = 1,
  });
  return term;
}

Term Term::Value(TypeErasedValue const &value) {
  Term term;
  term.nodes_.push_back({
      .kind  = Node::Kind::Value,
      .index = static_cast<uint16_t>(
          term.values_.index(term.values_.insert(value).first)),
      .subtree_size = 1,
  });
  return term;
}

Term Term::Call(Term const &value, Term f) {
  for (auto const &n : value.nodes_) {
    auto &node = f.nodes_.emplace_back(n);
    if (node.kind == Node::Kind::Value) {
      auto [iter, inserted] =
          f.values_.insert(value.values_.from_index(node.index));
      node.index = f.values_.index(iter);
    }
  }
  f.nodes_.push_back({
      .kind         = Node::Kind::FunctionCall,
      .subtree_size = static_cast<uint32_t>(f.nodes_.size() + 1),
  });
  f.PartiallyEvaluate();
  return f;
}

Term Term::Function(Term const &type, Term term) {
  for (auto const &n : type.nodes_) {
    auto &node = term.nodes_.emplace_back(n);
    if (node.kind == Node::Kind::Value) {
      auto [iter, inserted] =
          term.values_.insert(type.values_.from_index(node.index));
      node.index = term.values_.index(iter);
    }
  }
  term.nodes_.push_back({
      .kind         = Node::Kind::Function,
      .subtree_size = static_cast<uint32_t>(term.nodes_.size() + 1),
  });
  return term;
}

TypeErasedValue Term::Call(TypeErasedValue const &f, TypeErasedValue const &v) {
  auto const & fn = *f.value()[0].as<IrFunction const *>();
  nth::stack<jasmin::Value> stack;
  for (jasmin::Value value : v.value()) { stack.push(value); }
  jasmin::Execute(fn, stack);
  std::span results = stack.top_span(stack.size());
  return TypeErasedValue(f.type().AsFunction().returns()[0],
                         std::vector(results.begin(), results.end()));
}

TypeErasedValue const *Term::evaluate() const {
  if (nodes_.size() != 1) { return nullptr; }
  NTH_REQUIRE((v.harden), nodes_.back().kind == Node::Kind::Value);
  return &values_.from_index(nodes_.back().index);
}

void Term::Substitute(
    size_t index, nth::interval<std::vector<Node>::reverse_iterator> range) {
  // Replace DeBruijn indices with the value when appropriate.
  std::queue<std::pair<decltype(range), size_t>> work_queue;
  work_queue.emplace(range, 0);
  while (not work_queue.empty()) {
    auto [interval, count] = work_queue.front();
    auto [b, e]            = interval;
    work_queue.pop();
    for (auto iter = b; iter != e; ++iter) {
      switch (iter->kind) {
        case Node::Kind::Value: continue;
        case Node::Kind::FunctionCall: continue;
        case Node::Kind::Function:
          work_queue.emplace(nth::interval(iter + 1, e), count + 1);
          goto next_work_item;
        case Node::Kind::DeBruijnIndex:
          if (iter->index == count) {
            iter->kind  = Node::Kind::Value;
            iter->index = index;
          }
          continue;
      }
    }
  next_work_item:;
  }
}

void Term::PartiallyEvaluate() {
  auto write_iter = nodes_.begin();
  for (auto read_iter = nodes_.begin(); read_iter != nodes_.end();
       ++read_iter) {
    switch (read_iter->kind) {
      case Node::Kind::DeBruijnIndex:
      case Node::Kind::Function:
      case Node::Kind::Value: *write_iter++ = *read_iter; break;
      case Node::Kind::FunctionCall: {
        if ((write_iter - 1)->kind == Node::Kind::Value) {
          if ((write_iter - 2)->kind == Node::Kind::Value) {
            auto v        = *--write_iter;
            auto f        = *--write_iter;
            *write_iter++ = Node{
                .kind  = Node::Kind::Value,
                .index = static_cast<uint16_t>(
                    values_.index(values_
                                      .insert(Call(values_.from_index(f.index),
                                                   values_.from_index(v.index)))
                                      .first)),
                .subtree_size = 1,
            };
          } else if ((write_iter - 2)->kind == Node::Kind::Function) {
            if ((write_iter - 3)->kind == Node::Kind::Value) {
              auto v = *--write_iter;
              write_iter -= 2;
              auto value_index = v.index;
              Substitute(
                  value_index,
                  nth::interval(std::make_reverse_iterator(write_iter),
                                std::make_reverse_iterator(nodes_.begin())));
            }
          }
        } else {
          *write_iter++ = *read_iter;
        }
      } break;
    }
  }
  nodes_.erase(write_iter, nodes_.end());
}

void Term::specialize(TypeErasedValue const &value) {
  auto iter = nodes_.rbegin();
  NTH_REQUIRE((v.harden), iter->kind == Node::Kind::Function);
  ++iter;
  NTH_REQUIRE((v.harden), iter->kind == Node::Kind::Value);
  ++iter;
  Substitute(values_.index(values_.insert(value).first),
             nth::interval(iter, nodes_.rend()));
  nodes_.pop_back();
  nodes_.pop_back();
  PartiallyEvaluate();
}

}  // namespace ic::type::internal_dependent
