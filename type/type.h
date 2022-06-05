#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <sstream>
#include <string>

#include "base/cast.h"
#include "base/meta.h"
#include "base/visitable.h"
#include "core/arch.h"
#include "ir/value/result_buffer.h"

namespace type {

using AllTypeTypes =
    base::type_list<struct Array, struct Block, struct BufferPointer,
                    struct Enum, struct Flags, struct Function, struct Opaque,
                    struct Pointer, struct Primitive, struct Scope,
                    struct Slice, struct Struct, struct Variable>;

template <typename T>
constexpr int8_t IndexOf() {
  return base::Index<T>(AllTypeTypes{});
}

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
struct LegacyType : base::Visitable<LegacyType, AllTypeTypes>,
                    base::Cast<LegacyType> {
 public:
  LegacyType() = delete;
  virtual ~LegacyType() {}
  virtual void WriteTo(std::string *buf) const                    = 0;
  virtual core::Bytes bytes(core::Arch const &arch) const         = 0;
  virtual core::Alignment alignment(core::Arch const &arch) const = 0;

  using base::Visitable<LegacyType, AllTypeTypes>::which;

  bool IsDefaultInitializable() const {
    return flags_.is_default_initializable;
  }
  bool IsCopyable() const { return flags_.is_copyable; }
  bool IsMovable() const { return flags_.is_movable; }
  bool HasDestructor() const { return flags_.has_destructor; }

  std::string to_string() const {
    std::string result;
    WriteTo(&result);
    return result;
  }

  virtual Completeness completeness() const = 0;

  // TODO: length-0 arrays and length-1 arrays of small types should be
  // considered small too. Similarly with simple variants and tuples.
  virtual bool is_big() const { return false; }

  virtual size_t HashValue(ir::CompleteResultRef const &) const {
    UNREACHABLE();
  }
  virtual bool EqualsValue(ir::CompleteResultRef const &,
                           ir::CompleteResultRef const &) const {
    UNREACHABLE();
  }
  virtual void ShowValue(std::ostream &, ir::CompleteResultRef const &) const {
    UNREACHABLE();
  }

  // TODO: Can we ensure structs are complete before we set these?
  struct Flags {
    uint8_t is_default_initializable : 1;
    uint8_t is_copyable : 1;
    uint8_t is_movable : 1;
    uint8_t has_destructor : 1;
  };

  constexpr Flags flags() const { return flags_; }

 protected:
  explicit constexpr LegacyType(int8_t which, Flags flags)
      : base::Visitable<LegacyType, AllTypeTypes>(which), flags_(flags) {}
  Flags flags_;
};

struct Type;

// clang-format off
template <typename T>
concept TypeFamilyRequirements = requires(T const t) {
  { t.bytes(std::declval<core::Arch>()) }     -> std::same_as<core::Bytes>;
  { t.alignment(std::declval<core::Arch>()) } -> std::same_as<core::Alignment>;
  { t.to_string() }                           -> std::same_as<std::string>;
  { t == t }                                  -> std::same_as<bool>;
  { absl::Hash<T>{}(t) }                      -> std::same_as<size_t>;
  { t.HashValue(std::declval<ir::CompleteResultRef>()) }
                                              -> std::same_as<size_t>;
  { t.EqualsValue(std::declval<ir::CompleteResultRef>(),
                  std::declval<ir::CompleteResultRef>()) }
                                              -> std::same_as<bool>;
  { t.ShowValue(std::declval<std::ostream&>(), 
                std::declval<ir::CompleteResultRef>()) }
                                              -> std::same_as<void>;
};

// clang-format on

template <typename T>
concept TypeFamily =
    not std::is_same_v<std::decay_t<T>, Type> and
    std::is_trivially_destructible_v<T> and std::is_trivially_copyable_v<T> and
    TypeFamilyRequirements<T>;

namespace internal_type {

struct TypeVTable {
  core::Bytes (*bytes)(void const *, core::Arch const &) =
      [](void const *, core::Arch const &) { return core::Bytes{}; };
  core::Alignment (*alignment)(void const *, core::Arch const &) =
      [](void const *, core::Arch const &) { return core::Alignment{}; };
  std::string (*to_string)(void const *) = [](void const *) -> std::string {
    return "invalid";
  };
  bool (*is_big)(void const *) = [](void const *) { return false; };
  bool (*Equals)(void const *, void const *) = [](void const *, void const *) {
    return true;
  };
  size_t (*Hash)(void const *) = [](void const *) -> size_t { return 0; };
  bool (*EqualsValue)(void const *, ir::CompleteResultRef const &,
                      ir::CompleteResultRef const &) =
      [](void const *, ir::CompleteResultRef const &,
         ir::CompleteResultRef const &) -> bool { UNREACHABLE(); };
  size_t (*HashValue)(void const *, ir::CompleteResultRef const &) =
      [](void const *, ir::CompleteResultRef const &) -> size_t { return 0; };
  void (*ShowValue)(void const *, std::ostream &,
                    ir::CompleteResultRef const &) =
      [](void const *, std::ostream &, ir::CompleteResultRef const &) {};
};

inline TypeVTable DefaultTypeVTable{};

template <TypeFamily T>
inline TypeVTable TypeVTableFor = TypeVTable{
    .bytes =
        [](void const *self, core::Arch const &a) {
          return reinterpret_cast<T const *>(self)->bytes(a);
        },
    .alignment =
        [](void const *self, core::Arch const &a) {
          return reinterpret_cast<T const *>(self)->alignment(a);
        },
    .to_string =
        [](void const *self) {
          return reinterpret_cast<T const *>(self)->to_string();
        },
    .is_big =
        [](void const *self) {
          return reinterpret_cast<T const *>(self)->is_big();
        },
    .Equals =
        [](void const *lhs, void const *rhs) {
          return *reinterpret_cast<T const *>(lhs) ==
                 *reinterpret_cast<T const *>(rhs);
        },
    .Hash =
        [](void const *lhs) {
          return absl::Hash<T>{}(*reinterpret_cast<T const *>(lhs));
        },
    .EqualsValue =
        [](void const *self, ir::CompleteResultRef const &lhs,
           ir::CompleteResultRef const &rhs) {
          return reinterpret_cast<T const *>(self)->EqualsValue(lhs, rhs);
        },
    .HashValue =
        [](void const *self, ir::CompleteResultRef const &value) {
          return reinterpret_cast<T const *>(self)->HashValue(value);
        },
    .ShowValue =
        [](void const *self, std::ostream &os,
           ir::CompleteResultRef const &value) {
          reinterpret_cast<T const *>(self)->ShowValue(os, value);
        },
};

struct LegacyTypeWrapper {
  LegacyTypeWrapper(LegacyType const *t) : t_(t) {}

  core::Bytes bytes(core::Arch arch) const { return t_->bytes(arch); }
  core::Alignment alignment(core::Arch arch) const {
    return t_->alignment(arch);
  }
  std::string to_string() const { return t_->to_string(); }

  LegacyType const *get() const { return t_; }
  bool is_big() const { return t_->is_big(); }

  template <typename H>
  friend H AbslHashValue(H h, LegacyTypeWrapper const &t) {
    return H::combine(std::move(h), t.get());
  }

  friend bool operator==(LegacyTypeWrapper const &lhs,
                         LegacyTypeWrapper const &rhs) {
    return lhs.get() == rhs.get();
  }

  bool EqualsValue(ir::CompleteResultRef const &lhs,
                   ir::CompleteResultRef const &rhs) const {
    return t_->EqualsValue(lhs, rhs);
  }

  size_t HashValue(ir::CompleteResultRef const &value) const {
    return t_->HashValue(value);
  }
  void ShowValue(std::ostream &os, ir::CompleteResultRef const &value) const {
    t_->ShowValue(os, value);
  }

 private:
  LegacyType const *t_;
};

}  // namespace internal_type

struct Type {
  Type(std::nullptr_t p = nullptr)
      : data_{}, vptr_(&internal_type::DefaultTypeVTable) {}
  Type(LegacyType const *t)
      : vptr_(
            t ? &internal_type::TypeVTableFor<internal_type::LegacyTypeWrapper>
              : &internal_type::DefaultTypeVTable) {
    new (reinterpret_cast<internal_type::LegacyTypeWrapper *>(data_))
        internal_type::LegacyTypeWrapper(t);
  }

  template <TypeFamily T>
  Type(T const &t) : vptr_(&internal_type::TypeVTableFor<T>) {
    new (reinterpret_cast<T *>(data_)) T(t);
  }

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.vptr_->Hash(&t.data_));
  }

  operator bool() const { return vptr_ != &internal_type::DefaultTypeVTable; }
  bool valid() const { return vptr_ != &internal_type::DefaultTypeVTable; }

  // Template avoids implicit conversions.
  template <std::same_as<Type> T>
  friend bool operator==(T lhs, T rhs) {
    return lhs.vptr_ == rhs.vptr_ and lhs.vptr_->Equals(&lhs.data_, &rhs.data_);
  }
  friend bool operator!=(Type lhs, Type rhs) { return not(lhs == rhs); }

  core::Bytes bytes(core::Arch const &arch) const {
    return vptr_->bytes(&data_, arch);
  }
  core::Alignment alignment(core::Arch const &arch) const {
    return vptr_->alignment(&data_, arch);
  }

  bool EqualsValue(ir::CompleteResultRef const &lhs,
                   ir::CompleteResultRef const &rhs) const {
    return vptr_->EqualsValue(&data_, lhs, rhs);
  }

  size_t HashValue(ir::CompleteResultRef const &value) const {
    return vptr_->HashValue(&data_, value);
  }
  void ShowValue(std::ostream &os, ir::CompleteResultRef const &value) const {
    vptr_->ShowValue(&data_, os, value);
  }

  std::string Representation(ir::CompleteResultRef const &value) const {
    std::stringstream ss;
    ShowValue(ss, value);
    return ss.str();
  }

  std::string to_string() const { return vptr_->to_string(&data_); }

  bool is_big() const { return vptr_->is_big(&data_); }

  template <typename T>
  auto const *if_as() const {
    if constexpr (std::is_base_of_v<LegacyType, T> or
                  base::meta<T> == base::meta<LegacyType>) {
      return vptr_ == &internal_type::TypeVTableFor<
                          internal_type::LegacyTypeWrapper>
                 ? reinterpret_cast<internal_type::LegacyTypeWrapper const &>(
                       data_)
                       .get()
                       ->template if_as<T>()
                 : nullptr;
    } else {
      return vptr_ == &internal_type::TypeVTableFor<T>
                 ? &reinterpret_cast<T const &>(data_)
                 : nullptr;
    }
  }
  template <typename T>
  bool is() const {
    if constexpr (std::is_base_of_v<LegacyType, T> or
                  base::meta<T> == base::meta<LegacyType>) {
      return vptr_ == &internal_type::TypeVTableFor<
                          internal_type::LegacyTypeWrapper> and
             reinterpret_cast<internal_type::LegacyTypeWrapper const &>(data_)
                 .get()
                 ->template is<T>();
    } else {
      return vptr_ == &internal_type::TypeVTableFor<T>;
    }
  }
  template <typename T>
  T const &as() const {
    if constexpr (std::is_base_of_v<LegacyType, T> or
                  base::meta<T> == base::meta<LegacyType>) {
      return reinterpret_cast<internal_type::LegacyTypeWrapper const &>(data_)
          .get()
          ->template as<T>();
    } else {
      return reinterpret_cast<T const &>(data_);
    }
  }

  template <typename V, typename... Args>
  auto visit(V &v, Args &&... args) const {
    return get()->visit(v, std::forward<Args>(args)...);
  }

  LegacyType const *get() const { return if_as<LegacyType>(); }

  friend std::ostream &operator<<(std::ostream &os, Type t) {
    return os << t.to_string();
  }

 private:
  alignas(void const *) char data_[sizeof(void const *)];
  internal_type::TypeVTable const *vptr_;
};

// Intentionally leak this type.
template <typename T, typename... Args>
T *Allocate(Args &&... args) {
  return new T(std::forward<Args>(args)...);
}

}  // namespace type

#endif  // ICARUS_TYPE_TYPE_H
