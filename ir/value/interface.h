#ifndef ICARUS_IR_VALUE_INTERFACE_H
#define ICARUS_IR_VALUE_INTERFACE_H

#include <utility>

#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/flyweight_set.h"
#include "base/meta.h"
#include "core/arguments.h"
#include "type/type.h"

namespace ir {

struct InterfaceManager;

struct Interface {
  enum class Kind : uint8_t {
    Precise  = 0,
    Callable = 1,
  };

  friend bool operator==(Interface lhs, Interface rhs) {
    return lhs.index_ == rhs.index_ and lhs.kind_ == rhs.kind_;
  }

  friend bool operator!=(Interface lhs, Interface rhs) {
    return not(lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, Interface i) {
    return H::combine(std::move(h), (i.index_ << kNumKindBits) | i.kind_);
  }

 private:
  friend struct InterfaceManager;
  template <typename, Kind>
  friend struct InterfaceKind;

  explicit Interface(uint64_t n, Kind k)
      : index_(n), kind_(static_cast<std::underlying_type_t<Kind>>(k)) {
    ASSERT(n < (uint64_t{1} << kNumIndexBits));
  }

  static constexpr uint64_t kNumKindBits  = 4;
  static constexpr uint64_t kNumIndexBits = 64 - kNumKindBits;
  uint64_t index_ : kNumIndexBits;
  uint64_t kind_ : kNumKindBits;
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

struct CallableInterface
    : InterfaceKind<CallableInterface, Interface::Kind::Callable> {
  explicit CallableInterface(uint64_t n)
      : InterfaceKind<CallableInterface, Interface::Kind::Callable>(n) {}

  struct representation_type
      : base::Extend<representation_type, 1>::With<base::AbslHashExtension> {
    bool BindsTo(InterfaceManager const& m, type::Type t) const;

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

  bool BindsTo(Interface i, type::Type t) const;

 private:
  base::flyweight_set<typename PreciseInterface::representation_type>
      precisely_;
  base::flyweight_set<typename CallableInterface::representation_type>
      callable_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_INTERFACE_H
