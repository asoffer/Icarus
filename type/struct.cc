#include "type/struct.h"

#include "ast/hashtag.h"
#include "base/guarded.h"
#include "core/arch.h"
#include "module/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

Struct::Struct(module::BasicModule const *mod, Struct::Options options)
    : Type(Type::Flags{.is_default_initializable = 1,
                       .is_copyable              = options.is_copyable,
                       .is_movable               = options.is_movable,
                       .has_destructor           = 0}),
      mod_(mod),
      complete_(false) {}

void Struct::AppendFields(std::vector<Struct::Field> fields) {
  complete_ = true;
  fields_   = std::move(fields);
  size_t i  = 0;
  for (auto const &field : fields_) {
    field_indices_.emplace(field.name, i++);
    flags_.is_default_initializable &=
        field.type->IsDefaultInitializable() or not field.initial_value.empty();
    flags_.is_copyable &= field.type->IsCopyable();
    flags_.is_movable &= field.type->IsMovable();
    flags_.has_destructor |= field.type->HasDestructor();
  }
  // TODO HasDestructor is also dependent on the existence of it as a
  // free-function?
  // TODO
}

core::Bytes Struct::offset(size_t field_num, core::Arch const &a) const {
  auto offset = core::Bytes{0};
  for (size_t i = 0; i < field_num; ++i) {
    offset += fields_.at(i).type->bytes(a);
    offset = core::FwdAlign(offset, fields_.at(i + 1).type->alignment(a));
  }
  return offset;
}

size_t Struct::index(std::string_view name) const {
  return field_indices_.find(name)->second;
}

Struct::Field const *Struct::field(std::string_view name) const {
  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}

void Struct::WriteTo(std::string *result) const {
  result->append("struct.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

bool Struct::contains_hashtag(ast::Hashtag needle) const {
  for (auto const &tag : hashtags_) {
    if (tag == needle) { return true; }
  }
  return false;
}

core::Bytes Struct::bytes(core::Arch const &a) const {
  auto num_bytes = core::Bytes{0};
  for (auto const &field : fields_) {
    num_bytes += ASSERT_NOT_NULL(field.type)->bytes(a);
    // TODO it'd be in the (common, I think) case where you want both, it would
    // be faster to compute bytes and alignment simultaneously.
    num_bytes = core::FwdAlign(num_bytes, field.type->alignment(a));
  }

  return num_bytes;
}

core::Alignment Struct::alignment(core::Arch const &a) const {
  auto align = core::Alignment{1};
  for (auto const &field : fields_) {
    align = std::max(align, field.type->alignment(a));
  }
  return align;
}

}  // namespace type
