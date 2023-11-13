#include "ir/type_stack.h"

#include <cstring>

#include "nth/debug/debug.h"

namespace ic {
namespace {

constexpr uint64_t CountBit = uint64_t{1} << 63;

}  // namespace

void TypeStack::push(std::initializer_list<type::QualifiedType> types) {
  ++group_count_;
  types_.insert(types_.end(), types);
  if (types.size() != 1) {
    type::QualifiedType& qt = types_.emplace_back();
    uint64_t n              = CountBit | static_cast<uint64_t>(types.size());
    std::memcpy(&qt, &n, sizeof(qt));
  }
}

void TypeStack::push(std::span<type::QualifiedType const> types) {
  ++group_count_;
  types_.insert(types_.end(), types.begin(), types.end());
  if (types.size() != 1) {
    type::QualifiedType& qt = types_.emplace_back();
    uint64_t n              = CountBit | static_cast<uint64_t>(types.size());
    std::memcpy(&qt, &n, sizeof(qt));
  }
}

std::span<type::QualifiedType const> TypeStack::top() const {
  NTH_REQUIRE((v.debug), group_count_ > 0);
  NTH_REQUIRE((v.debug), not types_.empty());
  uint64_t n;
  std::memcpy(&n, &types_.back(), sizeof(n));
  if (n & CountBit) {
    uint64_t count = (n ^ CountBit);
    return std::span(&types_.back() - count, count);
  } else {
    return std::span(&types_.back(), 1);
  }
}

void TypeStack::pop() {
  NTH_REQUIRE((v.debug), group_count_ > 0);
  NTH_REQUIRE((v.debug), not types_.empty());
  --group_count_;
  uint64_t n;
  std::memcpy(&n, &types_.back(), sizeof(n));
  if (n & CountBit) {
    uint64_t count = (n ^ CountBit);
    types_.resize(types_.size() - (count + 1));
  } else {
    types_.pop_back();
  }
}

TypeStack::const_iterator& TypeStack::const_iterator::operator++() {
  uint64_t n;
  std::memcpy(&n, &*iter_, sizeof(n));
  if (n & CountBit) {
    uint64_t count = (n ^ CountBit);
    iter_ += count + 1;
  } else {
    ++iter_;
  }
  return *this;
}

std::span<type::QualifiedType const> TypeStack::const_iterator::operator*()
    const {
  uint64_t n;
  std::memcpy(&n, &*iter_, sizeof(n));
  if (n & CountBit) {
    uint64_t count = (n ^ CountBit);
    return std::span(&*iter_ - count, count);
  } else {
    return std::span(&*iter_, 1);
  }
}

}  // namespace ic
