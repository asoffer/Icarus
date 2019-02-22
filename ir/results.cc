#include "ir/results.h"

#include "ir/val.h"

namespace ir {

Results Results::FromVals(std::vector<Val> const& vals) {
  Results results;
  for (auto const& val : vals) {
    std::visit([&results](auto x) { results.append(x); }, val.value);
  }
  return results;
}

}  // namespace ir
