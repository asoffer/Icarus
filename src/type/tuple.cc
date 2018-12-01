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
void Tuple::EmitAssign(Type const *from_type, ir::Val const &from,
                       ir::Register to, Context *ctx) const {
  ASSERT(this == from_type);
  for (size_t i = 0; i < entries_.size(); ++i) {
    auto *entry_type = from_type->as<type::Tuple>().entries_.at(i);
    entries_[i]->EmitAssign(
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
  ir::Print('(');
  for (int i = 0; i < static_cast<int>(entries_.size()) - 1; ++i) {
    entries_[i]->EmitRepr(
        ir::Val::Reg(ir::PtrFix(ir::Field(reg, this, i), entries_[i]),
                     entries_[i]),
        ctx);
    ir::Print(',');
    ir::Print(' ');
  }

  if (!entries_.empty()) {
    entries_.back()->EmitRepr(
        ir::Val::Reg(ir::PtrFix(ir::Field(reg, this, entries_.size() - 1),
                                entries_.back()),
                     entries_.back()),
        ctx);
  }
  ir::Print(')');
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

}  // namespace type
