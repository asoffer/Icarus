#include "type/variable.h"

namespace type {
namespace {

base::flyweight_set<Variable> variables;

}  // namespace

Variable const* Var(ir::Interface intf) {
  auto [iter, inserted] = variables.insert(Variable(intf));
  return &*iter;
}

}  // namespace type
