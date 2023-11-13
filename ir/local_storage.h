#ifndef ICARUS_IR_LOCAL_STORAGE_H
#define ICARUS_IR_LOCAL_STORAGE_H

#include "absl/container/flat_hash_map.h"
#include "ir/scope.h"
#include "nth/container/interval.h"
#include "nth/debug/debug.h"
#include "parse/node_index.h"
#include "type/byte_width.h"
#include "type/type.h"

namespace ic {

struct LocalStorage {
  void insert(ParseNodeIndex node, type::Type t) {
    auto contour = type::Contour(t);
    width_.align_forward_to(contour.alignment());
    locations_.emplace(node,
                       nth::interval(width_, width_ + contour.byte_width()));
    width_ += contour.byte_width();
  }

  std::optional<type::ByteWidth> try_offset(ParseNodeIndex node) const {
    auto iter = locations_.find(node);
    if (iter == locations_.end()) { return std::nullopt; }
    return iter->second.lower_bound();
  }

  type::ByteWidth offset(ParseNodeIndex node) const {
    return range(node).lower_bound();
  }

  nth::interval<type::ByteWidth> range(ParseNodeIndex node) const {
    auto iter = locations_.find(node);
    NTH_REQUIRE((v.debug), iter != locations_.end());
    return iter->second;
  }

  type::ByteWidth size() const { return width_; }

 private:
  // TODO: We could take into account scoping information and reduce the size
  // here.
  type::ByteWidth width_ = type::ByteWidth(0);
  absl::flat_hash_map<ParseNodeIndex, nth::interval<type::ByteWidth>>
      locations_;
};

}  // namespace ic

#endif  // ICARUS_IR_LOCAL_STORAGE_H
