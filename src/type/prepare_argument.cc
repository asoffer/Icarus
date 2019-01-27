#include "ir/components.h"
#include "ir/func.h"
#include "type/all.h"

namespace type {
ir::Val Array::PrepareArgument(Type const *from, ir::Val const &val,
                               Context *ctx) const {
  if (from->is<Variant>()) {
    NOT_YET(this, from);
  } else {
    ASSERT(from == this);
    // TODO Copy may be overkill. Think about value category.
    auto arg = ir::Alloca(from);
    from->EmitAssign(from, val, arg, ctx);
    return ir::Val::Reg(arg, type::Ptr(from));
  }
}

ir::Val Primitive::PrepareArgument(Type const *from, ir::Val const &val,
                                   Context *ctx) const {
  if (from->is<Variant>()) {
    return ir::Val::Reg(
        ir::Load(ir::VariantValue(this, std::get<ir::Register>(val.value)),
                 this),
        this);
  } else {
    ASSERT(from == this);
    return val;
  }
}

ir::Val Pointer::PrepareArgument(Type const *from, ir::Val const &val,
                                 Context *ctx) const {
  ASSERT(from == this);
  return val;
}

ir::Val Function::PrepareArgument(Type const *from, ir::Val const &val,
                                  Context *ctx) const {
  if (this == from) {
    return val;
  } else {
    NOT_YET(this, from);
  }
}

ir::Val Enum::PrepareArgument(Type const *from, ir::Val const &val,
                              Context *ctx) const {
  ASSERT(from == this);
  return val;
}

ir::Val Flags::PrepareArgument(Type const *from, ir::Val const &val,
                               Context *ctx) const {
  ASSERT(from == this);
  return val;
}
ir::Val Variant::PrepareArgument(Type const *from, ir::Val const &val,
                                 Context *ctx) const {
  if (this == from) { return val; }
  auto alloc_reg = ir::Alloca(this);

  if (!from->is<Variant>()) {
    Type_->EmitAssign(Type_, ir::Val(from), ir::VariantType(alloc_reg), ctx);
    // TODO this isn't exactly right because 'from' might not be the appropriate
    // type here.
    // TODO this is actually the wrong type to plug in to VariantValue. It needs
    // to be the precise type stored.
    from->EmitAssign(from, val, ir::VariantValue(from, alloc_reg), ctx);
  } else {
    auto *from_v = &from->as<Variant>();
    auto runtime_type = ir::Load<Type const *>(
        ir::VariantType(std::get<ir::Register>(val.value)));

    // Because variants_ is sorted, we can find the intersection quickly:
    base::vector<Type const *> intersection;
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

    auto landing = ir::Func::Current->AddBlock();

    base::vector<ir::BlockIndex> blocks;
    blocks.reserve(intersection.size());
    for (auto *t : intersection) {
      blocks.push_back(ir::Func::Current->AddBlock());
    }

    auto current = ir::BasicBlock::Current;
    for (size_t i = 0; i < intersection.size(); ++i) {
      ir::BasicBlock::Current = blocks[i];
      this->EmitAssign(
          intersection[i],
          ir::Val::Reg(
              ir::PtrFix(ir::VariantValue(intersection[i],
                                          std::get<ir::Register>(val.value)),
                         intersection[i]),
              intersection[i]),
          alloc_reg, ctx);
      ir::UncondJump(landing);
    }

    ir::BasicBlock::Current = current;
    for (size_t i = 0; i < intersection.size() - 1; ++i) {
      ir::BasicBlock::Current = ir::EarlyExitOn<true>(
          blocks[i], ir::Eq(runtime_type, intersection[i]));
    }
    ir::UncondJump(blocks.back());
    ir::BasicBlock::Current = landing;
  }
  return ir::Val::Reg(alloc_reg, type::Ptr(this));
}

ir::Val Struct::PrepareArgument(Type const *from, ir::Val const &val,
                                Context *ctx) const {
  auto arg = ir::Alloca(this);

  if (from->is<Variant>()) {
    EmitAssign(
        this,
        ir::Val::Reg(ir::VariantValue(this, std::get<ir::Register>(val.value)),
                     type::Ptr(this)),
        arg, ctx);
  } else if (this == from) {
    EmitAssign(from, val, arg, ctx);
  } else {
    UNREACHABLE(from);
  }
  return ir::Val::Reg(arg, type::Ptr(this));
}

ir::Val GenericStruct::PrepareArgument(Type const *from, ir::Val const &val,
                                       Context *ctx) const {
  NOT_YET(this, from);
}

ir::Val Tuple::PrepareArgument(Type const *from, ir::Val const &val,
                               Context *ctx) const {
  ASSERT(from == this);
  auto arg = ir::Alloca(from);
  from->EmitAssign(from, val, arg, ctx);
  return ir::Val::Reg(arg, type::Ptr(from));
}

}  // namespace type
