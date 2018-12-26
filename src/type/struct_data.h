#ifndef ICARUS_TYPE_STRUCT_DATA_H
#define ICARUS_TYPE_STRUCT_DATA_H

#include <string>

#include "ast/hashtag.h"
#include "base/container/unordered_map.h"
#include "base/container/vector.h"
#include "ir/val.h"

namespace type {
struct Type;

struct StructData {
  StructData(::Module const *mod) : mod_(mod) {}
  struct Field {
    Field(type::Type const *t) : type(t) {}
    // TODO make a string_view but deal with trickiness of moving
    std::string name;
    Type const *type = nullptr;
    ir::Val init_val;
    base::vector<ast::Hashtag> hashtags_;
  };

  ::Module const *mod_ = nullptr;
  base::vector<Field> fields_;
  base::unordered_map<std::string, size_t> field_indices_;
};

}  // namespace type

#endif  // ICARUS_TYPE_STRUCT_DATA_H
