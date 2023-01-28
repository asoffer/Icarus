#ifndef ICARUS_MODULE_DATA_INTEGER_TABLE_H
#define ICARUS_MODULE_DATA_INTEGER_TABLE_H

#include <iosfwd>

#include "jasmin/instruction.h"
#include "module/data/data.pb.h"
#include "nth/container/flyweight_set.h"
#include "nth/numeric/integer.h"

namespace module {

struct IntegerHandle;

struct IntegerTable {
  using value_type = nth::Integer;

  IntegerHandle insert(nth::Integer const& n);

  auto begin() const { return set_.begin(); }
  auto end() const { return set_.end(); }

  void friend PrintTo(IntegerTable const& table, std::ostream* os);

 private:
  nth::flyweight_set<nth::Integer> set_;
};

struct IntegerHandle {
  explicit IntegerHandle() = default;

  nth::Integer const& value() const { return *ptr_; }

  struct Negate : jasmin::StackMachineInstruction<Negate> {
    using JasminExecutionState = module::IntegerTable;
    static IntegerHandle execute(JasminExecutionState& table,
                                 IntegerHandle handle) {
      return table.insert(-handle.value());
    }
  };

 private:
  explicit IntegerHandle(nth::Integer const* ptr) : ptr_(ptr) {}
  friend IntegerTable;
  nth::Integer const* ptr_;
};

void Serialize(IntegerTable const& table, data::IntegerTable& proto);
void Deserialize(data::IntegerTable const& proto, IntegerTable& table);

}  // namespace module

#endif  // ICARUS_MODULE_DATA_INTEGER_TABLE_H
