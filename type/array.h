#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include "absl/container/node_hash_set.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "core/arch.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/value/fn.h"
#include "type/primitive.h"
#include "type/type.h"

namespace type {

// `Array` is a type representing a fixed number (the `length`) of contiguous
// values of a given type (the `data_type`).
struct Array : LegacyType {
  using length_t = ir::Integer;
  static type::Type LengthType() { return type::Integer; }

  // Construct a new array from the given parameters, or if one already exists
  // in the cache, return that.
  friend Array const *Arr(length_t len, Type t);

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool EqualsValue(ir::CompleteResultRef const &lhs,
                   ir::CompleteResultRef const &rhs) const override;
  size_t HashValue(ir::CompleteResultRef const &value) const override;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  length_t length() const { return len_; }
  Type data_type() const { return data_type_; }

  Completeness completeness() const override {
    return data_type().as<LegacyType>().completeness();
  }

  bool is_big() const override { return true; }

  template <typename H>
  friend H AbslHashValue(H h, Array a) {
    return H::combine(std::move(h), a.length(), a.data_type());
  }

  friend bool operator==(Array const &lhs, Array const &rhs) {
    return lhs.length() == rhs.length() and lhs.data_type() == rhs.data_type();
  }

  friend bool operator!=(Array const &lhs, Array const &rhs) {
    return not(lhs == rhs);
  }

  void SetInitializer(ir::Fn f) {
    ASSERT(default_init_.has_value() == false);
    default_init_.emplace(f);
  }

  void SetDestructor(ir::Fn f) {
    ASSERT(dtor_.has_value() == false);
    dtor_.emplace(f);
  }

  void SetInits(ir::Fn copy_init, ir::Fn move_init) {
    ASSERT(inits_.has_value() == false);
    inits_.emplace(copy_init, move_init);
  }

  void SetAssignments(ir::Fn copy_assignment, ir::Fn move_assignment) {
    ASSERT(assignments_.has_value() == false);
    assignments_.emplace(copy_assignment, move_assignment);
  }

  ir::Fn Initializer() const { return default_init_.value(); }
  ir::Fn Destructor() const { return dtor_.value(); }
  ir::Fn CopyInit() const { return inits_->first; }
  ir::Fn MoveInit() const { return inits_->second; }
  ir::Fn CopyAssign() const { return assignments_->first; }
  ir::Fn MoveAssign() const { return assignments_->second; }

 private:
  explicit Array(length_t l, Type t)
      : LegacyType(IndexOf<Array>(), t.as<LegacyType>().flags()),
        len_(l),
        data_type_(t) {}

  length_t len_;
  Type data_type_;
  std::optional<ir::Fn> default_init_, dtor_;
  std::optional<std::pair<ir::Fn, ir::Fn>> assignments_, inits_;
};

Array const *Arr(Array::length_t len, Type t);

struct ArrayInstruction
    : base::Extend<ArrayInstruction>::With<base::BaseSerializeExtension,
                                           base::BaseTraverseExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = array %1$s %2$s";
  using length_t                                 = Array::length_t;

  Type Resolve() const { return Arr(length.value(), data_type.value()); }

  ir::RegOr<length_t> length;
  ir::RegOr<Type> data_type;
  ir::Reg result;
};

}  // namespace type
#endif  // ICARUS_TYPE_ARRAY_H
