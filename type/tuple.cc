#include "type/tuple.h"

#include <mutex>
#include <utility>

#include "base/guarded.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "layout/arch.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {
void Tuple::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                       ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  copy_assign_func_.init([this, ctx]() {
    Pointer const *p = Ptr(this);
    auto *fn         = ctx->mod_->AddFunc(
        Func({p, p}, {}),
        core::FnParams(core::Param{"", Typed<ast::Expression *>{nullptr, p}},
                       core::Param{"", Typed<ast::Expression *>{nullptr, p}}));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto val                = ir::Func::Current->Argument(0);
      auto var                = ir::Func::Current->Argument(1);

      // TODO is initialization order well-defined? ranodmize it? at least it
      // should always be opposite destruction order?
      for (size_t i = 0; i < entries_.size(); ++i) {
        auto *entry = entries_.at(i);
        entry->EmitCopyAssign(
            entry,
            ir::Results{ir::PtrFix(ir::Field(val, this, i).get(), entry)},
            ir::Field(var, this, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Copy(this, from.get<ir::Reg>(0), to);
}

bool Tuple::needs_destroy() const {
  return std::any_of(entries_.begin(), entries_.end(),
                     [](Type const *t) { return t->needs_destroy(); });
}

void Tuple::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                       ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  move_assign_func_.init([this, ctx]() {
    Pointer const *p = Ptr(this);
    auto *fn         = ctx->mod_->AddFunc(
        Func({p, p}, {}),
        core::FnParams(core::Param{"", Typed<ast::Expression *>{nullptr, p}},
                       core::Param{"", Typed<ast::Expression *>{nullptr, p}}));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto val                = ir::Func::Current->Argument(0);
      auto var                = ir::Func::Current->Argument(1);

      // TODO is initialization order well-defined? ranodmize it? at least it
      // should always be opposite destruction order?
      for (size_t i = 0; i < entries_.size(); ++i) {
        auto *entry = entries_.at(i);
        entry->EmitMoveAssign(
            entry,
            ir::Results{ir::PtrFix(ir::Field(val, this, i).get(), entry)},
            ir::Field(var, this, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Move(this, from.get<ir::Reg>(0), to);
}

void Tuple::EmitInit(ir::Register reg, Context *ctx) const {
  init_func_.init([this, ctx]() {
    auto *fn = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression *>{nullptr, Ptr(this)}}));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto var                = ir::Func::Current->Argument(0);

      // TODO is initialization order well-defined? ranodmize it? at least it
      // should always be opposite destruction order?
      for (size_t i = 0; i < entries_.size(); ++i) {
        entries_.at(i)->EmitInit(ir::Field(var, this, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Init(this, reg);
}

void Tuple::EmitRepr(ir::Results const &id_val, Context *ctx) const {
  auto reg = id_val.get<ir::Reg>(0);
  ir::Print(std::string_view{"("});
  for (int i = 0; i < static_cast<int>(entries_.size()) - 1; ++i) {
    entries_[i]->EmitRepr(
        ir::Results{ir::PtrFix(ir::Field(reg, this, i).get(), entries_[i])}, ctx);
    ir::Print(std::string_view{", "});
  }

  if (!entries_.empty()) {
    entries_.back()->EmitRepr(
        ir::Results{ir::PtrFix(ir::Field(reg, this, entries_.size() - 1).get(),
                               entries_.back())},
        ctx);
  }
  ir::Print(std::string_view{")"});
}

static base::guarded<std::map<std::vector<Type const *>, Tuple const>> tups_;
Type const *Tup(std::vector<Type const *> entries) {
  if (entries.size() == 1) { return entries[0]; }
  auto [iter, success] = tups_.lock()->emplace(std::piecewise_construct,
                                               std::forward_as_tuple(entries),
                                               std::forward_as_tuple(entries));
  return &iter->second;
}

Type const *Tuple::finalize() {
  auto *result = Tup(std::move(entries_));
  ASSERT(this != result);
  delete this;
  return result;
}

layout::Bytes Tuple::offset(size_t field_num, layout::Arch const &a) const {
  auto offset = layout::Bytes{0};
  for (size_t i = 0; i < field_num; ++i) {
    offset += entries_.at(i)->bytes(a);
    offset = layout::FwdAlign(offset, entries_.at(i + 1)->alignment(a));
  }
  return offset;
}

void Tuple::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  for (auto *entry : entries_) { entry->defining_modules(modules); }
}

void Tuple::EmitDestroy(ir::Register reg, Context *ctx) const {
  destroy_func_.init([this, ctx]() {
    auto *fn = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression *>{nullptr, Ptr(this)}}));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto var                = ir::Func::Current->Argument(0);

      // TODO is destruction order well-defined? ranodmize it?
      for (int i = static_cast<int>(entries_.size()) - 1; i >= 0; --i) {
        entries_.at(i)->EmitDestroy(ir::Field(var, this, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Destroy(this, reg);
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

ir::Results Tuple::PrepareArgument(Type const *from, ir::Results const &val,
                                   Context *ctx) const {
  ASSERT(from == this);
  auto arg = ir::Alloca(from);
  from->EmitMoveAssign(from, val, arg, ctx);
  return ir::Results{arg};
}

layout::Bytes Tuple::bytes(layout::Arch const &a) const {
  auto num_bytes = layout::Bytes{0};
  for (auto const *t : entries_) {
    num_bytes += t->bytes(a);
    // TODO it'd be in the (common, I think) case where you want both, it would
    // be faster to compute bytes and alignment simultaneously.
    num_bytes = layout::FwdAlign(num_bytes, t->alignment(a));
  }

  return num_bytes;
}

layout::Alignment Tuple::alignment(layout::Arch const &a) const {
  auto align = layout::Alignment{1};
  for (auto const *t : entries_) { align = std::max(align, t->alignment(a)); }
  return align;
}

bool Tuple::ReinterpretAs(Type const *t) const {
  auto *tup = t->if_as<Tuple>();
  if (!tup || tup->size() != size()) { return false; }
  for (size_t i = 0; i < size(); ++i) {
    if (!entries_.at(i)->ReinterpretAs(tup->entries_.at(i))) { return false; }
  }
  return true;
}
}  // namespace type
