#ifndef ICARUS_IR_STRUCT_FIELD_H
#define ICARUS_IR_STRUCT_FIELD_H

#include <optional>
#include <string_view>
#include <variant>

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
  explicit StructField(std::string_view name, RegOr<type::Type const *> type)
      : name_(name), data_(type) {}

  RegOr<type::Type const *> &type() {
    return std::get<RegOr<type::Type const *>>(data_);
  }

  RegOr<type::Type const *> type() const {
    return std::get<RegOr<type::Type const *>>(data_);
  }

  // Returns the name of the struct field.
  std::string_view name() const { return name_; }

 private:
  std::string_view name_;

  struct WithInitialValue {
    type::Type const *type_;
    Value val_;
  };

  // TODO we're  creating the variant but only ever using one part of it. We
  // need to start potentially populating initial values and using them.
  std::variant<WithInitialValue, RegOr<type::Type const *>> data_;

  // TODO hashtags.
};

}  // namespace ir

#endif  // ICARUS_IR_STRUCT_FIELD_H
