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

bool UserDefinedInterface::representation_type::BindsTo(
    InterfaceManager const& m, type::Type t) const {
  NOT_YET();
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

UserDefinedInterface InterfaceManager::UserDefined() {
  auto [iter, inserted] = user_defined_.insert(
      typename UserDefinedInterface::representation_type());
  return UserDefinedInterface(user_defined_.index(iter));
}

bool InterfaceManager::BindsTo(Interface i, type::Type t) const {
  switch (i.kind()) {
    case Interface::Kind::Precise:
      return precisely_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::Callable:
      return callable_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::UserDefined:
      return user_defined_.from_index(i.index_).BindsTo(*this, t);
  }
}

std::string InterfaceManager::DebugString(Interface i) const {
  switch (i.kind()) {
    case Interface::Kind::Precise:
      return precisely_.from_index(i.index_).type_.to_string();
    case Interface::Kind::Callable: {
      callable_.from_index(i.index_);
      auto const& arguments = callable_.from_index(i.index_).arguments();
      std::string result;
      std::string_view separator = "callable(";
      for (auto const& p : arguments.pos()) {
        absl::StrAppend(&result, std::exchange(separator, ", "),
                        DebugString(p));
      }
      for (auto const& [name, intf] : arguments.named()) {
        absl::StrAppend(&result, std::exchange(separator, ", "), name, " = ",
                        DebugString(intf));
      }
      return result;
    }
    case Interface::Kind::UserDefined: NOT_YET();
  }
}

}  // namespace ir
