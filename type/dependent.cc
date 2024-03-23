#include "type/dependent.h"

#include <queue>

#include "jasmin/core/function.h"
#include "nth/container/stack.h"
#include "type/function.h"
#include "type/refinement.h"

namespace ic::type {

DependentTerm DependentTerm::DeBruijnIndex(uint16_t index) {
  DependentTerm term;
  term.nodes_.push_back({
      .kind         = Node::Kind::DeBruijnIndex,
      .index        = index,
      .subtree_size = 1,
  });
  return term;
}

DependentTerm DependentTerm::Value(AnyValue const &value) {
  DependentTerm term;
  term.nodes_.push_back({
      .kind  = Node::Kind::Value,
      .index = static_cast<uint16_t>(
          term.values_.index(term.values_.insert(value).first)),
      .subtree_size = 1,
  });
  return term;
}

DependentTerm DependentTerm::Call(DependentTerm const &value, DependentTerm f) {
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

DependentTerm DependentTerm::Function(DependentTerm const &type,
                                      DependentTerm term) {
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

AnyValue DependentTerm::Call(AnyValue const &f, AnyValue const &v) {
  auto const &fn = *f.value()[0].as<jasmin::Function<> const *>();
  nth::stack<jasmin::Value> stack;
  for (jasmin::Value value : v.value()) { stack.push(value); }
  fn.invoke(stack);
  std::span results = stack.top_span(stack.size());
  return AnyValue(f.type().as<type::FunctionType>().returns()[0],
                  std::vector(results.begin(), results.end()));
}

bool operator==(DependentTerm const &lhs, DependentTerm const &rhs) {
  if (lhs.nodes_.size() != rhs.nodes_.size()) { return false; }
  auto l = lhs.nodes_.begin();
  auto r = rhs.nodes_.begin();
  for (; l != lhs.nodes_.end(); ++l, ++r) {
    if (l->kind != r->kind) { return false; }
    if (l->kind == DependentTerm::Node::Kind::Value) {
      if (lhs.values_.from_index(l->index) !=
          rhs.values_.from_index(r->index)) {
        return false;
      }
    } else {
      if (l->index != r->index) { return false; }
      if (l->subtree_size != r->subtree_size) { return false; }
    }
  }
  return true;
}

AnyValue const *DependentTerm::evaluate() const {
  if (nodes_.size() != 1) { return nullptr; }
  NTH_REQUIRE((v.harden), nodes_.back().kind == Node::Kind::Value);
  return &values_.from_index(nodes_.back().index);
}

void DependentTerm::Substitute(
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
          work_queue.emplace(
              nth::interval(iter + 1 + (iter + 1)->subtree_size, e), count + 1);
          e = iter + 1 + (iter + 1)->subtree_size;
          continue;
        case Node::Kind::DeBruijnIndex:
          if (iter->index == count) {
            iter->kind  = Node::Kind::Value;
            iter->index = index;
          }
          continue;
      }
    }
  }
}

void DependentTerm::PartiallyEvaluate() {
  size_t size;
  do {
    size            = nodes_.size();
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
                  .kind         = Node::Kind::Value,
                  .index        = static_cast<uint16_t>(values_.index(
                             values_
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
            } else {
              *write_iter++ = *read_iter;
            }
          } else {
            *write_iter++ = *read_iter;
          }
        } break;
      }
    }
    nodes_.erase(write_iter, nodes_.end());
    // TODO: This is likely the most efficient way to guarantee completely
    // simplifying.
  } while (size != nodes_.size());
}

bool DependentTerm::bind(AnyValue const &value) {
  NTH_UNIMPLEMENTED();
  // auto iter = nodes_.rbegin();
  // NTH_REQUIRE((v.harden), iter->kind == Node::Kind::Function);
  // ++iter;
  // NTH_REQUIRE((v.harden), iter->kind == Node::Kind::Value);
  // NTH_REQUIRE((v.harden), values_.from_index(iter->index).type() == Type_);
  // Type parameter     = values_.from_index(iter->index).value()[0].as<Type>();
  // Type argument_type = value.type();
  // if (parameter == argument_type) {
  //   // Okay.
  // } else if (parameter.kind() == Type::Kind::Refinement and
  //            parameter.as<type::RefinementType>().underlying() == Type_ and
  //            parameter.as<type::RefinementType>()(value)) {
  //   // Okay.
  // } else {
  //   return false;
  // }

  // ++iter;
  // Substitute(values_.index(values_.insert(value).first),
  //            nth::interval(iter, nodes_.rend()));
  // nodes_.pop_back();
  // nodes_.pop_back();
  // PartiallyEvaluate();
  // return true;
}

DependentParameterMapping::Index DependentParameterMapping::Index::Type(
    uint16_t n) {
  return Index(Kind::Type, n);
}

DependentParameterMapping::Index DependentParameterMapping::Index::Value(
    uint16_t n) {
  return Index(Kind::Value, n);
}

std::pair<DependentTerm const &, DependentParameterMapping const &>
DependentFunctionType::components() const {
  NTH_UNIMPLEMENTED();
  //   auto [term_index, mapping_index] =
  //       type_system->dependent_term_mapping_pairs.from_index(data());
  //   return std::pair<DependentTerm const&, DependentParameterMapping const&>(
  //       type_system->dependent_terms.from_index(term_index),
  //       type_system->dependent_mapping.from_index(term_index));
}

// std::optional<Type> DependentFunctionType::operator()(
//     std::span<AnyValue const> values) const {
//   NTH_UNIMPLEMENTED();
//   auto [term, mapping] = components();
//   auto term_copy       = term;
//   for (auto index : mapping) {
//     switch (index.kind()) {
//       case DependentParameterMapping::Index::Kind::Type:
//         if (not term_copy.bind(AnyValue(Type_,
//         values[index.index()].type()))) {
//           return std::nullopt;
//         }
//         break;
//       case DependentParameterMapping::Index::Kind::Value:
//         if (not term_copy.bind(values[index.index()])) {
//           return std::nullopt;
//         }
//         break;
//     }
//   }
//   if (auto* v = term_copy.evaluate()) { return v->value()[0].as<Type>(); }
//   return std::nullopt;
// }

DependentFunctionType Dependent(DependentTerm const &term,
                                DependentParameterMapping const &mapping) {
  NTH_UNIMPLEMENTED();
  // size_t term_index = type_system->dependent_terms.index(
  //     type_system->dependent_terms.insert(term).first);
  // size_t mapping_index = type_system->dependent_mapping.index(
  //     type_system->dependent_mapping.insert(mapping).first);
  // size_t pair_index = type_system->dependent_term_mapping_pairs.index(
  //     type_system->dependent_term_mapping_pairs
  //         .insert({term_index, mapping_index})
  //         .first);
  // return DependentFunctionType(pair_index);
}

}  // namespace ic::type
