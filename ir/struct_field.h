#ifndef ICARUS_IR_STRUCT_FIELD_H
#define ICARUS_IR_STRUCT_FIELD_H

#include <optional>
#include <string_view>
#include <variant>

#include "ir/byte_code_writer.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/type.h"

namespace ir {

// When compiling a struct definition, we build up a function to be intepretted
// at compile-time which returns the newly created struct type. (This might feel
// like a roundabout solution, but the reasons for it are discussed in TODO).
//
// Thus, when emitting the values for the struct fields, we need to pass all the
// field-related information. Because the struct-creating command would
// otherwise be immensely complicated, we provide `StructField` which represents
// the data pertaining to a specific field that needs to be passed to the
// struct-creating command.
struct StructField {
  // TODO: Remove this once ByteCodeWriter supports non-default-constructible
  // types.
  explicit StructField() : name_(""), data_(ir::RegOr<type::Type>(nullptr)) {}

  explicit StructField(std::string_view name, RegOr<type::Type> type)
      : name_(name), data_(type) {}

  explicit StructField(std::string_view name, type::Type t, ir::Value val)
      : name_(name), data_(WithInitialValue{.type_ = t, .val_ = val}) {}

  // Returns the name of the struct field.
  std::string_view name() const { return name_; }

  void set_export(bool b) { export_ = b; }
  constexpr bool exported() const { return export_; }

  // Returns a pointer to the register representing the type if it exists, and a
  // null pointer otherwise.
  Reg *type_reg() {
    if (auto *reg_or = std::get_if<RegOr<type::Type>>(&data_)) {
      if (reg_or->is_reg()) { return &reg_or->reg(); }
    }
    return nullptr;
  }

  RegOr<type::Type> type() const {
    return std::visit(
        [](auto const &data) -> RegOr<type::Type> {
          using data_type = std::decay_t<decltype(data)>;
          if constexpr (base::meta<data_type> == base::meta<WithInitialValue>) {
            return data.type_;
          } else {
            return data;
          }
        },
        data_);
  }

  // Returns a pointer to an initial value if one exists and a null pointer
  // otherwise.
  Value const *initial_value() const {
    if (auto *init_val = std::get_if<WithInitialValue>(&data_)) {
      return &init_val->val_;
    } else {
      return nullptr;
    }
  }

  Value *initial_value() {
    return const_cast<Value *>(
        static_cast<StructField const *>(this)->initial_value());
  }

 private:
  std::string_view name_;

  struct WithInitialValue {
    type::Type type_;
    Value val_;
  };

  std::variant<WithInitialValue, RegOr<type::Type>> data_;

  bool export_ = false;
};

}  // namespace ir

#endif  // ICARUS_IR_STRUCT_FIELD_H
