#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <mutex>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/types/span.h"
#include "ast/scope.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/traverse.h"
#include "ir/instruction/base.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/fn.h"
#include "ir/value/hashtag.h"
#include "ir/value/native_fn.h"
#include "type/type.h"

namespace type {

struct Struct : LegacyType {
  struct Field : base::Extend<Field, 4>::With<base::BaseSerializeExtension> {
    // TODO make a string_view but deal with trickiness of moving

    std::string name;
    Type type = nullptr;
    ir::CompleteResultBuffer initial_value;
    absl::flat_hash_set<ir::Hashtag> hashtags;
  };

  struct Options {
    uint8_t is_copyable : 1;
    uint8_t is_movable : 1;
  };
  Struct(module::BasicModule const *mod, Options options);

  ir::Fn Destructor() const;

  ir::Fn const *MoveInit(type::Type from_type) const;
  ir::Fn const *CopyInit(type::Type from_type) const;

  ir::Fn const *MoveAssignment(type::Type from_type) const;
  ir::Fn const *CopyAssignment(type::Type from_type) const;

  bool is_big() const override { return true; }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Completeness completeness() const override { return completeness_; }
  void complete() { completeness_ = Completeness::Complete; }

  Field const *field(std::string_view name) const;
  Field const *constant(std::string_view name) const;

  module::BasicModule const *defining_module() const { return mod_; }

  core::Bytes offset(size_t n, core::Arch const &arch) const;

  absl::Span<Field const> fields() const { return fields_; }
  absl::Span<Field const> constants() const { return constants_; }
  size_t index(std::string_view name) const;

  module::BasicModule const *mod_ = nullptr;
  Completeness completeness_      = Completeness::Incomplete;

  // TODO: Make these private.
  std::vector<ir::Hashtag> hashtags;
  std::vector<Field> fields_, constants_;
  std::optional<ir::Fn> init_, user_dtor_, dtor_;

 private:
  friend struct StructInstruction;

  void AppendConstants(std::vector<Struct::Field> constants);
  void AppendFields(std::vector<Field> fields);
  void SetDestructor(ir::Fn dtor);
  void SetInits(absl::Span<ir::Fn const> move_inits,
                absl::Span<ir::Fn const> copy_inits);
  void SetAssignments(absl::Span<ir::Fn const> move_assignments,
                      absl::Span<ir::Fn const> copy_assignments);

  absl::flat_hash_map<type::Type, ir::Fn> move_inits_, copy_inits_,
      move_assignments_, copy_assignments_;
  absl::flat_hash_map<std::string, size_t> field_indices_;
};

// When compiling a struct definition, we build up a function to be
// intepretted at compile-time which returns the newly created struct type. This
// provides benefit over directly emitting struct types from the syntax tree
// because it brings consistency whether the struct is parameterized or not, and
// allows for more powerful metaprogramming.
struct StructInstruction
    : base::Extend<StructInstruction>::With<base::BaseSerializeExtension> {
  struct Field : base::Extend<Field, 4>::With<base::BaseSerializeExtension> {
    // TODO: Remove this once ByteCodeWriter supports
    // non-default-constructible types.
    explicit Field() : name_(""), type_(ir::RegOr<Type>(nullptr)) {}

    explicit Field(std::string_view name, ir::RegOr<Type> t,
                   ir::CompleteResultBuffer value = {})
        : name_(name), type_(t), value_(std::move(value)) {}

    // Returns the name of the struct field.
    std::string_view name() const { return name_; }

    void set_export(bool b) { export_ = b; }
    constexpr bool exported() const { return export_; }

    // Returns a pointer to the register representing the type if it exists,
    // and a null pointer otherwise.
    ir::Reg *type_reg() { return type_.is_reg() ? &type_.reg() : nullptr; }
    ir::RegOr<Type> type() const { return type_; }
    ir::CompleteResultRef initial_value() const {
      return value_.empty() ? ir::CompleteResultRef() : value_[0];
    }

    friend void BaseTraverse(ir::Inliner &inliner, Field &f) {
      base::Traverse(inliner, f.type_);
    }

   private:
    friend base::EnableExtensions;

    std::string_view name_;
    ir::RegOr<Type> type_;
    ir::CompleteResultBuffer value_;
    bool export_ = false;
  };

  friend void BaseTraverse(ir::Inliner &inliner, StructInstruction &s) {
    base::Traverse(inliner, s.constants, s.fields);
  }

  void Apply(interpreter::ExecutionContext &ctx) const;

  std::string to_string() const {
    // TODO
    return "struct TODO";
  }

  // TODO: Store a special type indicating that the struct is incomplete.
  Struct *struct_;
  std::vector<Field> constants;
  std::vector<Field> fields;
  std::vector<ir::Fn> move_inits, copy_inits, move_assignments,
      copy_assignments;
  std::optional<ir::Fn> dtor;
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
