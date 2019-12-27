#ifndef ICARUS_IR_STRUCT_FIELD_H
#define ICARUS_IR_STRUCT_FIELD_H

#include <optional>
#include <string_view>

#include "ir/reg_or.h"
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
  explicit constexpr StructField(std::string_view name,
                                 RegOr<type::Type const*> type)
      : name_(name), type_(type) {}

  RegOr<type::Type const*>& type() { return type_; }
  RegOr<type::Type const*> type() const { return type_; }
  std::string_view name() const { return name_; }

 private:
  std::string_view name_;

  // If no type is present in the syntax tree, then we must have an initial
  // value so we would end up populating the type from that. In other words this
  // type is guaranteed to be present here even if it is not present in the
  // syntax tree.
  RegOr<type::Type const*> type_;

  // TODO hashtags and initial values.
};

}  // namespace ir

#endif // ICARUS_IR_STRUCT_FIELD_H
