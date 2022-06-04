#ifndef ICARUS_IR_VALUE_INTERFACE_H
#define ICARUS_IR_VALUE_INTERFACE_H

#include <utility>

#include "absl/container/btree_map.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/flyweight_set.h"
#include "base/meta.h"
#include "core/arguments.h"
#include "ir/subroutine.h"
#include "type/type.h"

namespace ir {

struct InterfaceManager;

struct Interface {
  enum class Kind : uint8_t {
    Precise       = 0,
    Callable      = 1,
    Pointer       = 2,
    BufferPointer = 3,
    Slice         = 4,
    UserDefined   = 5,
  };

  Interface() = default;

  friend bool operator==(Interface lhs, Interface rhs) {
    return lhs.index_ == rhs.index_ and lhs.kind_ == rhs.kind_;
  }

  friend bool operator!=(Interface lhs, Interface rhs) {
    return not(lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, Interface i) {
    return H::combine(std::move(h), i.value());
  }

  friend void BaseSerialize(auto& s, Interface i) {
    base::Serialize(s, i.value());
  }
  friend bool BaseDeserialize(auto& d, Interface& i) {
    uint64_t n;
    if (not base::Deserialize(d, n)) { return false; }
    i = Interface(
        n >> kNumIndexBits,
        static_cast<Kind>(n & (uint64_t{1} << kNumIndexBits) - uint64_t{1}));
    return true;
  }

 private:
  friend struct InterfaceManager;
  template <typename, Kind>
  friend struct InterfaceKind;

  Kind kind() const { return static_cast<Kind>(kind_); }

  uint64_t value() const { return (index_ << kNumKindBits) | kind_; }

  explicit Interface(uint64_t n, Kind k)
      : index_(n), kind_(static_cast<std::underlying_type_t<Kind>>(k)) {
    ASSERT(n < (uint64_t{1} << kNumIndexBits));
  }

  static constexpr uint64_t kNumKindBits  = 4;
  static constexpr uint64_t kNumIndexBits = 64 - kNumKindBits;
  uint64_t index_ : kNumIndexBits         = 0;
  uint64_t kind_ : kNumKindBits           = 0;
};

template <typename T, Interface::Kind K>
struct InterfaceKind
    : base::Extend<T, 1>::template With<base::AbslHashExtension> {
  operator Interface() const { return Interface(index_, K); }

 protected:
  explicit InterfaceKind(uint64_t index) : index_(index) {}

 private:
  friend struct InterfaceManager;
  friend base::EnableExtensions;

  uint64_t index_;
};

struct PreciseInterface
    : InterfaceKind<PreciseInterface, Interface::Kind::Precise> {
  explicit PreciseInterface(uint64_t n)
      : InterfaceKind<PreciseInterface, Interface::Kind::Precise>(n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

   private:
    friend base::EnableExtensions;
    friend struct InterfaceManager;

    explicit representation_type(type::Type t) : type_(t) {}

    type::Type type_;
  };
};

struct UserDefinedInterface
    : InterfaceKind<UserDefinedInterface, Interface::Kind::UserDefined> {
  explicit UserDefinedInterface(uint64_t n)
      : InterfaceKind<UserDefinedInterface, Interface::Kind::UserDefined>(n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

   private:
    friend base::EnableExtensions;
    friend struct InterfaceManager;

    explicit representation_type(
        absl::btree_map<std::string, Subroutine> members)
        : members_(std::move(members)) {}

    absl::btree_map<std::string, Subroutine> members_;
  };
};

struct PointerInterface
    : InterfaceKind<PointerInterface, Interface::Kind::Pointer> {
  explicit PointerInterface(uint64_t n)
      : InterfaceKind<PointerInterface, Interface::Kind::Pointer>(n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

    Interface pointee() const { return pointee_; }

   private:
    friend base::EnableExtensions;
    friend struct InterfaceManager;

    explicit representation_type(Interface intf) : pointee_(intf) {}

    Interface pointee_;
  };
};

struct BufferPointerInterface
    : InterfaceKind<BufferPointerInterface, Interface::Kind::BufferPointer> {
  explicit BufferPointerInterface(uint64_t n)
      : InterfaceKind<BufferPointerInterface, Interface::Kind::BufferPointer>(
            n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

    Interface pointee() const { return pointee_; }

   private:
    friend base::EnableExtensions;
    friend struct InterfaceManager;

    explicit representation_type(Interface intf) : pointee_(intf) {}

    Interface pointee_;
  };
};

struct SliceInterface : InterfaceKind<SliceInterface, Interface::Kind::Slice> {
  explicit SliceInterface(uint64_t n)
      : InterfaceKind<SliceInterface, Interface::Kind::Slice>(n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

    Interface data_type() const { return data_type_; }

   private:
    friend base::EnableExtensions;
    friend struct InterfaceManager;

    explicit representation_type(Interface intf) : data_type_(intf) {}

    Interface data_type_;
  };
};

struct CallableInterface
    : InterfaceKind<CallableInterface, Interface::Kind::Callable> {
  explicit CallableInterface(uint64_t n)
      : InterfaceKind<CallableInterface, Interface::Kind::Callable>(n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

    core::Arguments<Interface> const& arguments() const { return arguments_; }

   private:
    friend base::EnableExtensions;
    friend struct InterfaceManager;

    explicit representation_type(core::Arguments<Interface> arguments)
        : arguments_(std::move(arguments)) {}

    core::Arguments<Interface> arguments_;
  };
};

struct InterfaceManager {
  PreciseInterface Precisely(type::Type);
  CallableInterface Callable(core::Arguments<Interface> const& arguments);
  PointerInterface Pointer(Interface pointee);
  BufferPointerInterface BufferPointer(Interface pointee);
  SliceInterface Slice(Interface pointee);

  UserDefinedInterface UserDefined(
      absl::btree_map<std::string, Subroutine> members);

  bool BindsTo(Interface i, type::Type t) const;

  std::string DebugString(Interface i) const;

 private:
  base::flyweight_set<typename PreciseInterface::representation_type>
      precisely_;
  base::flyweight_set<typename CallableInterface::representation_type>
      callable_;
  base::flyweight_set<typename PointerInterface::representation_type> pointer_;
  base::flyweight_set<typename BufferPointerInterface::representation_type>
      buffer_pointer_;
  base::flyweight_set<typename SliceInterface::representation_type> slice_;
  std::vector<typename UserDefinedInterface::representation_type> user_defined_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_INTERFACE_H
