#include "serialization/function_index.h"

namespace serialization {

std::ostream &operator<<(std::ostream &os, FunctionIndex l) {
  if (l == FunctionIndex::Invalid()) {
    return os << "invalid";
  } else if (l.foreign()) {
    return os << "foreign." << l.value();
  } else {
    return os << "local." << l.value();
  }
}

void FunctionIndex::Serialize(FunctionIndex from, proto::FunctionIndex &to) {
  to.set_foreign(from.foreign());
  to.set_index(from.value());
}

bool FunctionIndex::Deserialize(proto::FunctionIndex from, FunctionIndex &to) {
  if (from.index() > MaxLocalIndex) { return false; }
  to.foreign_ = from.foreign();
  to.index_   = from.index();
  return true;
}

}  // namespace serialization
