#include "ir/value/interface.h"

#include "core/call.h"
#include "type/callable.h"

namespace ir {

bool PreciseInterface::representation_type::BindsTo(InterfaceManager const&,
                                                    type::Type t) const {
  return type_ == t;
}

bool CallableInterface::representation_type::BindsTo(InterfaceManager const& m,
                                                     type::Type t) const {
  auto const* callable = t.if_as<type::Callable>();
  if (not callable) { return false; }
  auto result = core::Callability(
      callable->parameters(), arguments_,
      [&m](Interface intf, type::QualType const& argument_qt) {
        return m.BindsTo(intf, argument_qt.type());
      });
  return result.ok();
}

PreciseInterface InterfaceManager::Precisely(type::Type t) {
  auto [iter, inserted] =
      precisely_.insert(typename PreciseInterface::representation_type(t));
  return PreciseInterface(precisely_.index(iter));
}

CallableInterface InterfaceManager::Callable(
    core::Arguments<Interface> const& arguments) {
  auto [iter, inserted] = callable_.insert(
      typename CallableInterface::representation_type(arguments));
  return CallableInterface(callable_.index(iter));
}

bool InterfaceManager::BindsTo(Interface i, type::Type t) const {
  switch (static_cast<Interface::Kind>(i.kind_)) {
    case Interface::Kind::Precise:
      return precisely_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::Callable:
      return callable_.from_index(i.index_).BindsTo(*this, t);
  }
}

}  // namespace ir
