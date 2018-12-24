#ifndef ICARUS_TYPE_INCOMPLETE_STRUCT_H
#define ICARUS_TYPE_INCOMPLETE_STRUCT_H

#include <string_view>
#include "base/container/vector.h"
#include "type/struct_data.h"
#include "type/type.h"

namespace type {
struct Struct;

struct IncompleteStruct : public Type {
  ~IncompleteStruct() override {}
  BASIC_METHODS;

  void set_last_name(std::string_view s);
  void add_field(type::Type const *t);

  Struct const *finalize() &&;

 private:
  StructData data_;
};

}  // namespace type

#endif  // ICARUS_TYPE_INCOMPLETE_STRUCT_H