#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "base/cast.h"
#include "base/meta.h"
#include "core/arch.h"
#include "type/visitor_base.h"

#define TYPE_FNS(name)                                                         \
  void WriteTo(std::string *buf) const override;                               \
  core::Bytes bytes(core::Arch const &arch) const override;                    \
  core::Alignment alignment(core::Arch const &arch) const override

namespace type {
// `Completeness` is an enum that describes the status of a partially
// cosntructed type. While this is primarily of interest for user-defined
// structs, it is also relevant for compound types (e.g., arrays, tuples,
// variants) of these types.
enum class Completeness {
  // Incomplete: We have not yet determined all of the fields of the struct.
  // It is disallowed to do anything with this type other than using pointer
  // to a values of this type.
  Incomplete,
  // DataComplete: All fields have been determined and any properties
  // derivable from only fields can be used (e.g., byte-size and alignment).
  // Fields may be accessed but objects of still may not be constructed.
  DataComplete,
  // Complete: All fields and all special member functions have been
  // completed. The type is complete and objects of this type may be
  // constructed.
  Complete
};

// `LegacyType` is the base class for all types in the Icarus type system. To
// construct a new category of types, create a subclass of `LegacyType`.
// Implementing the required virtual methods, and passing the correct flags to
// the `LegacyType` constructor.
struct LegacyType : base::Cast<LegacyType> {
 public:
  LegacyType() = delete;
  virtual ~LegacyType() {}
  virtual void WriteTo(std::string *buf) const                    = 0;
  virtual core::Bytes bytes(core::Arch const &arch) const         = 0;
  virtual core::Alignment alignment(core::Arch const &arch) const = 0;

  bool IsDefaultInitializable() const {
    return flags_.is_default_initializable;
  }
  bool IsCopyable() const { return flags_.is_copyable; }
  bool IsMovable() const { return flags_.is_movable; }
  bool HasDestructor() const { return flags_.has_destructor; }

  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  std::string to_string() const {
    std::string result;
    WriteTo(&result);
    return result;
  }

  virtual Completeness completeness() const = 0;

  // TODO length-0 arrays and length-1 arrays of small types should be
  // considered small too. Similarly with simple variants and tuples.
  // // TODO make this pure virtual
  virtual bool is_big() const { return false; }

  // TODO: Can we ensure structs are complete before we set these?
  struct Flags {
    uint8_t is_default_initializable : 1;
    uint8_t is_copyable : 1;
    uint8_t is_movable : 1;
    uint8_t has_destructor : 1;
  };

  constexpr Flags flags() const { return flags_; }

 protected:
  explicit constexpr LegacyType(Flags flags) : flags_(flags) {}
  Flags flags_;
};

struct Type {
  Type(LegacyType const *t = nullptr)
      : kind_(Kind::Legacy), data_(reinterpret_cast<uintptr_t>(t)) {}

  LegacyType *get() { return reinterpret_cast<LegacyType *>(data_); }
  LegacyType const *get() const {
    return reinterpret_cast<LegacyType const *>(data_);
  }

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.data_);
  }

  operator bool() const { return get(); }
  bool valid() const { return get(); }

  // Template avoids implicit conversions.
  template <typename T,
            std::enable_if_t<base::meta<T> == base::meta<Type>, int> = 0>
  friend bool operator==(T lhs, T rhs) {
    return lhs.data_ == rhs.data_;
  }
  friend bool operator!=(Type lhs, Type rhs) { return not(lhs == rhs); }

  core::Bytes bytes(core::Arch arch) const { return get()->bytes(arch); }
  core::Alignment alignment(core::Arch arch) const {
    return get()->alignment(arch);
  }

  std::string to_string() const {
    return get()->to_string();
  }

  template <typename T>
  auto *if_as() {
    return get()->template if_as<T>();
  }
  template <typename T>
  auto const *if_as() const {
    return get()->template if_as<T>();
  }
  template <typename T>
  bool is() const {
    return get()->template is<T>();
  }
  template <typename T>
  T &as() {
    return get()->template as<T>();
  }
  template <typename T>
  T const &as() const {
    return get()->template as<T>();
  }

  friend std::ostream &operator<<(std::ostream &os, Type t) {
    return os << t.get()->to_string();
  }

 private:
  enum class Kind { Legacy, Opaque, NonLegacy } kind_;
  uintptr_t data_;
};

// Intentionally leak this type.
template <typename T, typename... Args>
T *Allocate(Args &&... args) {
  return new T(std::forward<Args>(args)...);
}

}  // namespace type

#endif  // ICARUS_TYPE_TYPE_H
