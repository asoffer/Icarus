#ifndef ICARUS_TYPE_STRUCT_H
#define ICARUS_TYPE_STRUCT_H

#include <mutex>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/types/span.h"
#include "ast/scope/scope.h"
#include "base/extend.h"
#include "ir/instruction/base.h"
#include "ir/instruction/inliner.h"
#include "ir/interpretter/execution_context.h"
#include "ir/value/fn.h"
#include "ir/value/hashtag.h"
#include "ir/value/native_fn.h"
#include "ir/value/value.h"
#include "type/type.h"

namespace ast {
struct StructLiteral;
}  // namespace ast

namespace type {

struct Struct : public LegacyType {
  struct Field {
    // TODO make a string_view but deal with trickiness of moving

    std::string name;
    Type type = nullptr;
    ir::Value initial_value;
    absl::flat_hash_set<ir::Hashtag> hashtags;
  };

  struct Options {
    uint8_t is_copyable : 1;
    uint8_t is_movable : 1;
  };
  Struct(module::BasicModule const *mod, Options options);

  ir::Fn Destructor() const;
  ir::Fn const *Assignment(type::Type from_type) const;

  bool is_big() const override { return true; }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Completeness completeness() const override { return completeness_; }
  void complete() { completeness_ = Completeness::Complete; }

  // Return the type of a field, or a nullptr if it doesn't exist
  Field const *field(std::string_view name) const;

  module::BasicModule const *defining_module() const { return mod_; }

  core::Bytes offset(size_t n, core::Arch const &arch) const;

  absl::Span<Field const> fields() const { return fields_; }
  size_t index(std::string_view name) const;

  module::BasicModule const *mod_ = nullptr;
  Completeness completeness_      = Completeness::Incomplete;

  // TODO: Make these private.
  std::vector<ir::Hashtag> hashtags;
  std::vector<Field> fields_;
  std::optional<ir::Fn> init_, user_dtor_, dtor_;

 private:
  friend struct StructInstruction;

  void AppendFields(std::vector<Field> fields);
  void SetDestructor(ir::Fn dtor);
  void SetAssignments(absl::Span<ir::Fn const> assignments);

  absl::flat_hash_map<type::Type, ir::Fn> assignments_;
  absl::flat_hash_map<std::string_view, size_t> field_indices_;
};

// When compiling a struct definition, we build up a function to be
// intepretted at compile-time which returns the newly created struct type. This
// provides benefit over directly emitting struct types from the syntax tree
// because it brings consistency whether the struct is parameterized or not, and
// allows for more powerful metaprogramming.
struct StructInstruction
    : base::Extend<StructInstruction>::With<ir::ByteCodeExtension,
                                            ir::InlineExtension> {
  struct Field {
    // TODO: Remove this once ByteCodeWriter supports non-default-constructible
    // types.
    explicit Field() : name_(""), data_(ir::RegOr<Type>(nullptr)) {}

    explicit Field(std::string_view name, ir::RegOr<Type> type)
        : name_(name), data_(type) {}

    explicit Field(std::string_view name, Type t, ir::Value val)
        : name_(name), data_(WithInitialValue{.type_ = t, .val_ = val}) {}

    // Returns the name of the struct field.
    std::string_view name() const { return name_; }

    void set_export(bool b) { export_ = b; }
    constexpr bool exported() const { return export_; }

    // Returns a pointer to the register representing the type if it exists, and
    // a null pointer otherwise.
    ir::Reg *type_reg() {
      if (auto *reg_or = std::get_if<ir::RegOr<Type>>(&data_)) {
        if (reg_or->is_reg()) { return &reg_or->reg(); }
      }
      return nullptr;
    }

    ir::RegOr<Type> type() const {
      return std::visit(
          [](auto const &data) -> ir::RegOr<Type> {
            using data_type = std::decay_t<decltype(data)>;
            if constexpr (base::meta<data_type> ==
                          base::meta<WithInitialValue>) {
              return data.type_;
            } else {
              return data;
            }
          },
          data_);
    }

    // Returns a pointer to an initial value if one exists and a null pointer
    // otherwise.
    ir::Value const *initial_value() const {
      if (auto *init_val = std::get_if<WithInitialValue>(&data_)) {
        return &init_val->val_;
      } else {
        return nullptr;
      }
    }

    ir::Value *initial_value() {
      return const_cast<ir::Value *>(
          static_cast<Field const *>(this)->initial_value());
    }

   private:
    std::string_view name_;

    struct WithInitialValue {
      Type type_;
      ir::Value val_;
    };

    std::variant<WithInitialValue, ir::RegOr<Type>> data_;

    bool export_ = false;
  };

  // TODO field.type() can be null. If the field type is inferred from the
  // initial value.
  void Apply(interpretter::ExecutionContext &ctx) const {
    std::vector<Struct::Field> struct_fields;
    struct_fields.reserve(fields.size());
    for (auto const &field : fields) {
      absl::flat_hash_set<ir::Hashtag> tags;
      if (field.exported()) { tags.insert(ir::Hashtag::Export); }

      if (ir::Value const *init_val = field.initial_value()) {
        Type t = ctx.resolve(field.type());
        struct_fields.push_back(Struct::Field{.name = std::string(field.name()),
                                              .type = t,
                                              .initial_value = *init_val,
                                              .hashtags = std::move(tags)});
      } else {
        struct_fields.push_back(Struct::Field{.name = std::string(field.name()),
                                              .type = ctx.resolve(field.type()),
                                              .initial_value = ir::Value(),
                                              .hashtags = std::move(tags)});
      }
    }

    struct_->AppendFields(std::move(struct_fields));
    struct_->SetAssignments(std::move(assignments));
    if (dtor) { struct_->SetDestructor(*dtor); }
  }

  std::string to_string() const {
    // TODO
    return "struct TODO";
  }

  // TODO: Store a special type indicating that the struct is incomplete.
  Struct *struct_;
  std::vector<Field> fields;
  std::vector<ir::Fn> assignments;
  std::optional<ir::Fn> dtor;
};

}  // namespace type
#endif  // ICARUS_TYPE_STRUCT_H
