#include "serialization/function_index.h"

namespace serialization {

std::ostream &operator<<(std::ostream &os, FunctionIndex l) {
  if (l == FunctionIndex::Invalid()) {
    return os << "invalid";
  } else {
    return os << "local." << l.value();
  }
}

void FunctionIndex::Serialize(FunctionIndex from, proto::FunctionIndex &to) {
  to.set_index(from.value());
}

bool FunctionIndex::Deserialize(proto::FunctionIndex from, FunctionIndex &to) {
  to.index_ = from.index();
  return true;
}

}  // namespace serialization
