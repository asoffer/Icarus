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

// `Type` is the base class for all types in the Icarus type system. To
// construct a new category of types, create a subclass of `Type`. Implementing
// the required virtual methods, and passing the correct flags to the `Type`
// constructor.
struct Type : base::Cast<Type> {
 public:
  Type() = delete;
  virtual ~Type() {}
  virtual void WriteTo(std::string *buf) const                    = 0;
  virtual core::Bytes bytes(core::Arch const &arch) const         = 0;
  virtual core::Alignment alignment(core::Arch const &arch) const = 0;

  bool IsDefaultInitializable() const {
    return flags_.is_default_initializable;
  }
  bool IsCopyable() const { return flags_.is_copyable; }
  bool IsMovable() const { return flags_.is_movable; }
  bool HasDestructor() const { return flags_.has_destructor; }

  // TODO tests on all sub-classes
  bool DeepComplete() const {
    absl::flat_hash_set<Type const *> ts;
    return DeepCompleteImpl(ts);
  }

  virtual bool DeepCompleteImpl(absl::flat_hash_set<Type const *> &ts) const {
    return true;
  }

  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  std::string to_string() const {
    std::string result;
    WriteTo(&result);
    return result;
  }

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
  explicit constexpr Type(Flags flags) : flags_(flags) {}
  Flags flags_;
};

}  // namespace type

#endif  // ICARUS_TYPE_TYPE_H
