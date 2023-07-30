#ifndef ICARUS_DATA_TYPES_INTEGER_H
#define ICARUS_DATA_TYPES_INTEGER_H

#include <iosfwd>

#include "serialization/constants.pb.h"
#include "jasmin/instruction.h"
#include "nth/container/flyweight_set.h"
#include "absl/numeric/int128.h"

namespace data_types {

struct IntegerHandle;

struct IntegerTable {
  using value_type = absl::int128;

  IntegerHandle insert(absl::int128 const& n);

  auto begin() const { return set_.begin(); }
  auto end() const { return set_.end(); }

  void friend PrintTo(IntegerTable const& table, std::ostream* os);

 private:
  nth::flyweight_set<absl::int128> set_;
};

struct IntegerHandle {
  explicit IntegerHandle() = default;

  absl::int128 const& value() const { return *ptr_; }

  struct Negate : jasmin::StackMachineInstruction<Negate> {
    using execution_state = IntegerTable;
    static IntegerHandle execute(execution_state& table, IntegerHandle handle) {
      return table.insert(-handle.value());
    }
  };

 private:
  explicit IntegerHandle(absl::int128 const* ptr) : ptr_(ptr) {}
  friend IntegerTable;
  absl::int128 const* ptr_;
};

void Serialize(IntegerTable const& table, serialization::IntegerTable& proto);
void Deserialize(serialization::IntegerTable const& proto, IntegerTable& table);

}  // namespace data_types

#endif  // ICARUS_DATA_TYPES_INTEGER_H
