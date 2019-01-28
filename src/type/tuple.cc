#include "type/tuple.h"

#include <mutex>
#include <utility>

#include "architecture.h"
#include "base/container/map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {
static std::mutex mtx_;
void Tuple::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                       ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(this == from_type);
  for (size_t i = 0; i < entries_.size(); ++i) {
    auto *entry_type = from_type->as<type::Tuple>().entries_.at(i);
    entries_[i]->EmitCopyAssign(
        entries_[i],
        ir::Val::Reg(
            ir::PtrFix(ir::Field(std::get<ir::Register>(from.value), this, i),
                       entry_type),
            entry_type),
        ir::Field(to, this, i), ctx);
  }
}

void Tuple::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                       ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(this == from_type);
  for (size_t i = 0; i < entries_.size(); ++i) {
    auto *entry_type = from_type->as<type::Tuple>().entries_.at(i);
    entries_[i]->EmitMoveAssign(
        entries_[i],
        ir::Val::Reg(
            ir::PtrFix(ir::Field(std::get<ir::Register>(from.value), this, i),
                       entry_type),
            entry_type),
        ir::Field(to, this, i), ctx);
  }
}

void Tuple::EmitInit(ir::Register reg, Context *ctx) const {
  for (size_t i= 0; i < entries_.size(); ++i) {
    entries_[i]->EmitInit(ir::Field(reg, this, i), ctx);
  }
}

void Tuple::EmitRepr(ir::Val const &id_val, Context *ctx) const {
  auto reg = std::get<ir::Register>(id_val.value);
  ir::Print(std::string_view{"("});
  for (int i = 0; i < static_cast<int>(entries_.size()) - 1; ++i) {
    entries_[i]->EmitRepr(
        ir::Val::Reg(ir::PtrFix(ir::Field(reg, this, i), entries_[i]),
                     entries_[i]),
        ctx);
    ir::Print(std::string_view{", "});
  }

  if (!entries_.empty()) {
    entries_.back()->EmitRepr(
        ir::Val::Reg(ir::PtrFix(ir::Field(reg, this, entries_.size() - 1),
                                entries_.back()),
                     entries_.back()),
        ctx);
  }
  ir::Print(std::string_view{")"});
}

static base::guarded<base::map<base::vector<Type const *>, Tuple const>> tups_;
Type const *Tup(base::vector<Type const *> entries) {
  if (entries.size() == 1) { return entries[0]; }
  Tuple tup(entries);
  auto[iter, success] = tups_.lock()->emplace(std::move(entries), std::move(tup));
  return &iter->second;
}

Type const *Tuple::finalize() {
  auto *result = Tup(std::move(entries_));
  ASSERT(this != result);
  delete this;
  return result;
}

size_t Tuple::offset(size_t field_num, Architecture const &arch) const {
  size_t offset = 0;
  for (size_t i = 0; i < field_num; ++i) {
    offset += arch.bytes(entries_.at(i));
    offset = arch.MoveForwardToAlignment(entries_.at(i + 1), offset);
  }
  return offset;
}

void Tuple::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  for (auto *entry : entries_) { entry->defining_modules(modules); }
}

void Tuple::EmitDestroy(ir::Register reg, Context *ctx) const {
  // TODO don't auto-inline.
  for (size_t i = 0; i < entries_.size(); ++i) {
    entries_[i]->EmitDestroy(ir::Field(reg, this, i), ctx);
  }
}

void Tuple::WriteTo(std::string *result) const {
  if (entries_.empty()) {
    result->append("()");
    return;
  }
  result->append("(");
  auto iter = entries_.begin();
  (*iter)->WriteTo(result);
  ++iter;
  for (; iter != entries_.end(); ++iter) {
    result->append(", ");
    (*iter)->WriteTo(result);
  }
  result->append(")");
}

bool Tuple::IsCopyable() const {
  return std::all_of(entries_.begin(), entries_.end(),
                     [](Type const *t) { return t->IsCopyable(); });
}

bool Tuple::IsMovable() const {
  return std::all_of(entries_.begin(), entries_.end(),
                     [](Type const *t) { return t->IsMovable(); });
}

}  // namespace type
