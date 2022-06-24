#include "ir/value/interface.h"

#include "core/call.h"
#include "type/callable.h"
#include "type/pointer.h"
#include "type/slice.h"

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

bool PointerInterface::representation_type::BindsTo(InterfaceManager const& im,
                                                    type::Type t) const {
  auto const* p = t.if_as<type::Pointer>();
  if (not p) { return false; }
  return im.BindsTo(pointee_, p->pointee());
}

bool BufferPointerInterface::representation_type::BindsTo(
    InterfaceManager const& im, type::Type t) const {
  auto const* p = t.if_as<type::BufferPointer>();
  if (not p) { return false; }
  return im.BindsTo(pointee_, p->pointee());
}

bool SliceInterface::representation_type::BindsTo(InterfaceManager const& im,
                                                  type::Type t) const {
  auto const* p = t.if_as<type::Slice>();
  if (not p) { return false; }
  return im.BindsTo(data_type_, p->data_type());
}

PointerInterface InterfaceManager::Pointer(Interface pointee) {
  auto [iter, inserted] =
      pointer_.insert(typename PointerInterface::representation_type(pointee));
  return PointerInterface(pointer_.index(iter));
}

BufferPointerInterface InterfaceManager::BufferPointer(Interface pointee) {
  auto [iter, inserted] = buffer_pointer_.insert(
      typename BufferPointerInterface::representation_type(pointee));
  return BufferPointerInterface(buffer_pointer_.index(iter));
}

SliceInterface InterfaceManager::Slice(Interface pointee) {
  auto [iter, inserted] =
      slice_.insert(typename SliceInterface::representation_type(pointee));
  return SliceInterface(slice_.index(iter));
}

UserDefinedInterface InterfaceManager::UserDefined(
    absl::btree_map<std::string, Subroutine> members) {
  user_defined_.push_back(
      typename UserDefinedInterface::representation_type(std::move(members)));
  return UserDefinedInterface(user_defined_.size() - 1);
}

bool InterfaceManager::BindsTo(Interface i, type::Type t) const {
  switch (i.kind()) {
    case Interface::Kind::Precise:
      return precisely_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::Callable:
      return callable_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::Pointer:
      return pointer_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::BufferPointer:
      return buffer_pointer_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::Slice:
      return slice_.from_index(i.index_).BindsTo(*this, t);
    case Interface::Kind::UserDefined:
      return user_defined_[i.index_].BindsTo(*this, t);
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
    case Interface::Kind::Pointer:
      return absl::StrCat(
          "*(", DebugString(pointer_.from_index(i.index_).pointee_), ")");
    case Interface::Kind::BufferPointer:
      return absl::StrCat(
          "[*](", DebugString(buffer_pointer_.from_index(i.index_).pointee_),
          ")");
    case Interface::Kind::Slice:
      return absl::StrCat(
          "[](", DebugString(slice_.from_index(i.index_).data_type_), ")");
    case Interface::Kind::UserDefined: NOT_YET();
  }
}

}  // namespace ir
