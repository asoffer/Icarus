#ifndef ICARUS_MODULE_DATA_INTEGER_TABLE_H
#define ICARUS_MODULE_DATA_INTEGER_TABLE_H

#include <iosfwd>

#include "module/data/data.pb.h"
#include "nth/container/flyweight_set.h"
#include "nth/numeric/integer.h"

namespace module {

// TODO: nth::flyweight_set doesn't yet support erasing elements, so we'll end
// up keeping and serializing values that were only temporaries.
struct IntegerTable {
  using value_type = nth::Integer;

  nth::Integer const* insert(nth::Integer const& n);

  auto begin() const { return set_.begin(); }
  auto end() const { return set_.end(); }

  void friend PrintTo(IntegerTable const& table, std::ostream* os);

 private:
  nth::flyweight_set<nth::Integer> set_;
};

void Serialize(IntegerTable const& table, data::IntegerTable& proto);
void Deserialize(data::IntegerTable const& proto, IntegerTable& table);

}  // namespace module

#endif  // ICARUS_MODULE_DATA_INTEGER_TABLE_H
