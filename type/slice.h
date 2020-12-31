#ifndef ICARUS_TYPE_SLICE_H
#define ICARUS_TYPE_SLICE_H

#include "absl/container/node_hash_set.h"
#include "base/extend.h"
#include "core/arch.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "type/type.h"

namespace type {

// `Slice` is a type representing a dynamic number (the `length`) of contiguous
// values of a given type (the `data_type`). Slices do not own their data. They
// are only a view into a buffer.
struct Slice : LegacyType {
  using length_t = uint64_t;
  // Construct a new slice from the given parameters, or if one already exists
  // in the cache, return that.
  friend Slice const *Slc(Type t);

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Type data_type() const { return data_type_; }

  Completeness completeness() const override {
    return data_type().get()->completeness();
  }

  bool is_big() const override { return false; }

  template <typename H>
  friend H AbslHashValue(H h, Slice const &s) {
    return H::combine(std::move(h), s.data_type());
  }

  friend bool operator==(Slice const &lhs, Slice const &rhs) {
    return lhs.data_type() == rhs.data_type();
  }

  friend bool operator!=(Slice const &lhs, Slice const &rhs) {
    return not(lhs == rhs);
  }


 private:
  explicit Slice(Type t)
      : LegacyType({
            .is_default_initializable = 0,
            .is_copyable              = 1,
            .is_movable               = 1,
            .has_destructor           = 0,
        }),
        data_type_(t) {}

  Type data_type_;
};

Slice const *Slc(Type t);

struct SliceInstruction
    : base::Extend<SliceInstruction>::With<ir::ByteCodeExtension,
                                           ir::InlineExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = slice %1$s";

  Type Resolve() const { return Slc(data_type.value()); }

  ir::RegOr<Type> data_type;
  ir::Reg result;
};

}  // namespace type
#endif  // ICARUS_TYPE_SLICE_H
