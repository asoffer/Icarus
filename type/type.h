#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "base/cast.h"
#include "core/arch.h"
#include "type/visitor_base.h"

#define TYPE_FNS(name)                                                         \
  ~name() {}                                                                   \
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
  constexpr Type(LegacyType const *t = nullptr) : t_(t) {}
  constexpr operator LegacyType const *() const { return t_; }
  constexpr LegacyType const *operator->() const { return t_; }
  constexpr LegacyType const *get() const { return t_; }

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.t_);
  }

  constexpr operator bool() const { return t_; }

  friend bool operator==(Type lhs, Type rhs) { return lhs.t_ == rhs.t_; }
  friend bool operator!=(Type lhs, Type rhs) { return not(lhs == rhs); }

  friend bool operator==(LegacyType const *lhs, Type rhs) {
    return lhs == rhs.t_;
  }
  friend bool operator==(Type lhs, LegacyType const *rhs) {
    return lhs.t_ == rhs;
  }

  friend bool operator!=(LegacyType const *lhs, Type rhs) {
    return lhs != rhs.t_;
  }
  friend bool operator!=(Type lhs, LegacyType const *rhs) {
    return lhs.t_ != rhs;
  }

  friend std::ostream &operator<<(std::ostream &os, Type t) {
    return os << t->to_string();
  }

 private:
  LegacyType const *t_;
};

// Intentionally leak this type.
template <typename T, typename... Args>
T *Allocate(Args &&... args) {
  return new T(std::forward<Args>(args)...);
}

}  // namespace type

#endif  // ICARUS_TYPE_TYPE_H
