#include "type/incomplete_struct.h"

#include "type/struct.h"

namespace type {
char *IncompleteStruct::WriteTo(char *buf) const { NOT_YET(); }
size_t IncompleteStruct::string_size() const { NOT_YET(); }
void IncompleteStruct::EmitAssign(const Type *from_type, ir::Val const &from,
                                  ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  UNREACHABLE();
}
void IncompleteStruct::EmitInit(ir::Register reg, Context *ctx) const {
  UNREACHABLE();
}
void IncompleteStruct::EmitDestroy(ir::Register reg, Context *ctx) const {
  UNREACHABLE();
}
ir::Val IncompleteStruct::PrepareArgument(const Type *t, const ir::Val &val,
                                          Context *ctx) const {
  UNREACHABLE();
}
void IncompleteStruct::EmitRepr(ir::Val const &id_val, Context *ctx) const {
  UNREACHABLE();
}
Cmp IncompleteStruct::Comparator() const { UNREACHABLE(); }

void IncompleteStruct::set_last_name(std::string_view s) {
  data_.fields_.back().name = std::string(s);
  auto[iter, success] = data_.field_indices_.emplace(data_.fields_.back().name,
                                                     data_.fields_.size() - 1);
  ASSERT(success);
}

void IncompleteStruct::add_field(type::Type const *t) {
  data_.fields_.emplace_back(t);
}

Struct const *IncompleteStruct::finalize() && {
  return new Struct(std::move(data_));
}

}  // namespace type
