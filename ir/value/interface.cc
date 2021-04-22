#include "ir/value/interface.h"
#include "absl/container/node_hash_set.h"

namespace ir {
namespace {

base::Global<absl::node_hash_set<type::Type>> conversions_;

}  // namespace

Interface Interface::ConvertsTo(type::Type t) {
  return Interface(&*conversions_.lock()->insert(t).first);
}

}  // namespace ir
