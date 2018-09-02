#include "type/all.h"
#include "ir/func.h"

namespace type {
IR::Val Array::PrepareArgument(const Type *from, const IR::Val &val,
                               Context *ctx) const {
  if (from->is<Variant>()) {
    NOT_YET(this, from);
  } else {
    ASSERT(from == this);
    if (fixed_length) {
      // TODO Copy may be overkill. Think about value category.
      auto arg = IR::Val::Reg(IR::Alloca(from), type::Ptr(from));
      from->EmitAssign(from, val, arg, ctx);
      return arg;
    } else {
      NOT_YET();
    }
  }
}

IR::Val Primitive::PrepareArgument(const Type *from, const IR::Val &val,
                                   Context *ctx) const {
  if (from->is<Variant>()) {
    return IR::Load(
        IR::Val::Reg(IR::VariantValue(this, std::get<IR::Register>(val.value)),
                     type::Ptr(this)));
  } else {
    ASSERT(from == this);
    return val;
  }
}

IR::Val Pointer::PrepareArgument(const Type *from, const IR::Val &val,
                                 Context *ctx) const {
  ASSERT(from == this);
  return val;
}

IR::Val Function::PrepareArgument(const Type *from, const IR::Val &val,
                                  Context *ctx) const {
  if (this == from) {
    return val;
  } else {
    NOT_YET(this, from);
  }
}

IR::Val Enum::PrepareArgument(const Type *from, const IR::Val &val,
                              Context *ctx) const {
  ASSERT(from == this);
  return val;
}

IR::Val Flags::PrepareArgument(const Type *from, const IR::Val &val,
                               Context *ctx) const {
  ASSERT(from == this);
  return val;
}
IR::Val Variant::PrepareArgument(const Type *from, const IR::Val &val,
                                 Context *ctx) const {
  if (this == from) { return val; }
  auto alloc_reg = IR::Alloca(this);
  auto arg       = IR::Val::Reg(alloc_reg, type::Ptr(this));
  if (!from->is<Variant>()) {
    type::Type_->EmitAssign(
        Type_, IR::Val::Type(from),
        IR::Val::Reg(IR::VariantType(alloc_reg), type::Ptr(this)), ctx);
    // TODO this isn't exactly right because 'from' might not be the appropriate
    // type here.
    NOT_YET();
    // TODO this is actually the wrong type to plug in to VariantValue. It needs
    // to be the precise type stored.
    from->EmitAssign(
        from, val,
        IR::Val::Reg(IR::VariantValue(from, alloc_reg), type::Ptr(from)), ctx);
  } else {
    auto *from_v = &from->as<Variant>();
    IR::Register runtime_type =
        IR::LoadType(IR::VariantType(std::get<IR::Register>(val.value)));

    // Because variants_ is sorted, we can find the intersection quickly:
    base::vector<const Type *> intersection;
    auto f_iter = from_v->variants_.begin();
    auto t_iter = this->variants_.begin();
    while (f_iter != from_v->variants_.end() &&
           t_iter != this->variants_.end()) {
      if (*f_iter < *t_iter) {
        ++f_iter;
      } else if (*f_iter > *t_iter) {
        ++t_iter;
      } else {
        intersection.push_back(*f_iter);
        ++f_iter;
        ++t_iter;
      }
    }
    ASSERT(!intersection.empty());

    auto landing = IR::Func::Current->AddBlock();

    base::vector<IR::BlockIndex> blocks;
    blocks.reserve(intersection.size());
    for (auto *t : intersection) {
      blocks.push_back(IR::Func::Current->AddBlock());
    }

    auto current = IR::BasicBlock::Current;
    for (size_t i = 0; i < intersection.size(); ++i) {
      IR::BasicBlock::Current = blocks[i];
      this->EmitAssign(intersection[i],
                       PtrCallFix(IR::Val::Reg(
                           IR::VariantValue(intersection[i],
                                            std::get<IR::Register>(val.value)),
                           type::Ptr(intersection[i]))),
                       arg, ctx);
      IR::UncondJump(landing);
    }

    IR::BasicBlock::Current = current;
    for (size_t i = 0; i < intersection.size() - 1; ++i) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          blocks[i], IR::ValFrom(IR::EqType(runtime_type, intersection[i])));
    }
    IR::UncondJump(blocks.back());
    IR::BasicBlock::Current = landing;
  }
  return arg;
}

IR::Val Scope::PrepareArgument(const Type *from, const IR::Val &,
                               Context *ctx) const {
  NOT_YET(this, from);
}

IR::Val CharBuffer::PrepareArgument(const Type *from, const IR::Val &,
                                    Context *ctx) const {
  NOT_YET(this, from);
}

IR::Val Struct::PrepareArgument(const Type *from, const IR::Val &val,
                                Context *ctx) const {
  auto arg = IR::Val::Reg(IR::Alloca(this), type::Ptr(this));
  if (from->is<Variant>()) {
    EmitAssign(
        this,
        IR::Val::Reg(IR::VariantValue(this, std::get<IR::Register>(val.value)),
                     type::Ptr(this)),
        arg, ctx);
  } else if (this == from) {
    EmitAssign(from, val, arg, ctx);
  } else {
    UNREACHABLE(from);
  }
  return arg;
}

} // namespace type
