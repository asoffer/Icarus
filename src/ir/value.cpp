#include "value.h"

namespace IR {
bool operator<(const Value &lhs, const Value &rhs) {
  if (lhs.flag != rhs.flag) { return static_cast<int>(lhs.flag) < static_cast<int>(rhs.flag); }
  switch (lhs.flag) {
  case ValType::Val:
    return ArbitraryOrdering(lhs.as_val, rhs.as_val) == Order::Less;
  case ValType::Loc:
    return ArbitraryOrdering(lhs.as_loc, rhs.as_loc) == Order::Less;
  case ValType::CStr: return lhs.as_cstr < rhs.as_cstr;
  case ValType::Block: return lhs.as_block < rhs.as_block;
  case ValType::HeapAddr: return lhs.as_heap_addr < rhs.as_heap_addr;
  case ValType::ExtFn:  return lhs.as_ext_fn < rhs.as_ext_fn;
  case ValType::GlobalCStr: return lhs.as_global_cstr < rhs.as_global_cstr;
  case ValType::Error:
  case ValType::None: return false;
  }
}

bool operator==(const Value &lhs, const Value &rhs) {
  if (lhs.flag != rhs.flag) { return false; }
  switch (lhs.flag) {
  case ValType::Val:
    return ArbitraryOrdering(lhs.as_val, rhs.as_val) == Order::Equal;
  case ValType::Loc:
    return ArbitraryOrdering(lhs.as_loc, rhs.as_loc) == Order::Equal;
  default: return lhs.as_blah == rhs.as_blah;
  }
}
} // namespace IR
