#include "type/generic.h"

namespace type {
namespace {

base::flyweight_set<Generic> generics;

}  // namespace

Generic const *Gen(ir::Interface interface, ir::InterfaceManager *manager) {
  auto [iter, inserted] = generics.insert(Generic(interface, manager));
  return &*iter;
}

}  // namespace type

