#include "type/variant.h"

#include <algorithm>

#include "absl/algorithm/container.h"
#include "base/guarded.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"

namespace type {

bool Variant::contains(Type const *t) const {
  // TODO can do a binary search.
  for (auto *v : variants_) {
    if (v == t) { return true; }
  }
  return false;
}

static base::guarded<std::map<std::vector<Type const *>, Variant>>
    all_variants_;
Type const *Var(std::vector<Type const *> variants) {
  if (variants.empty()) { return Void(); }
  if (variants.size() == 1) { return variants[0]; }

  size_t end = variants.size();
  size_t i   = 0;
  while (i < end) {
    if (variants[i]->is<Variant>()) {
      Variant const *var = &variants[i]->as<Variant>();
      variants[i]        = variants.back();
      variants.pop_back();
      variants.insert(variants.end(), var->variants_.begin(),
                      var->variants_.end());
    } else {
      ++i;
    }
  }

  // TODO This sort order should be deterministic to allow interoperability
  // between multiple runs of the compiler.

  std::sort(variants.begin(), variants.end());
  variants.erase(std::unique(variants.begin(), variants.end()), variants.end());

  if (variants.size() == 1) { return variants.front(); }

  return &all_variants_.lock()
              ->emplace(std::piecewise_construct,
                        std::forward_as_tuple(variants),
                        std::forward_as_tuple(variants))
              .first->second;
}

void Variant::EmitDestroy(ir::Reg reg, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(mtx_);
  if (!destroy_func_) {
    destroy_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression const *>{nullptr, this}}));
    CURRENT_FUNC(destroy_func_) {
      ir::BasicBlock::Current = destroy_func_->entry();
      auto landing            = ir::CompiledFn::Current->AddBlock();
      auto type = ir::Load<Type const *>(ir::VariantType(ir::Reg::Arg(0)));

      for (Type const *v : variants_) {
        if (!v->needs_destroy()) { continue; }
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::CompiledFn::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitDestroy(ir::PtrFix(ir::VariantValue(v, ir::Reg::Arg(0)), v),
                       ctx);
        ir::UncondJump(landing);

        ir::BasicBlock::Current = old_block;
        ir::BasicBlock::Current = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegisterOr<Type const *>(type), v));
      }

      ir::UncondJump(landing);
      ir::BasicBlock::Current = landing;
      ir::ReturnJump();
    }
  }

  ir::Call(ir::AnyFunc{destroy_func_},
           ir::Arguments(destroy_func_->type_, ir::Results{reg}));
}

bool Variant::needs_destroy() const {
  return std::any_of(variants_.begin(), variants_.end(),
                     [](Type const *t) { return t->needs_destroy(); });
}

void Variant::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                             ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg_ == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  EmitDestroy(to.reg_, ctx);

  if (from_type->is<Variant>()) {
    auto actual_type = ir::Load<type::Type const *>(
        ir::VariantType(from.get<ir::Reg>(0)));
    auto landing = ir::CompiledFn::Current->AddBlock();
    for (Type const *v : from_type->as<Variant>().variants_) {
      auto next_block = ir::CompiledFn::Current->AddBlock();
      ir::BasicBlock::Current =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitCopyAssign(
          v,
          ir::Results{ir::PtrFix(ir::VariantValue(v, from.get<ir::Reg>(0)), v)},
          ir::VariantValue(v, to), ctx);
      ir::UncondJump(landing);
      ir::BasicBlock::Current = next_block;
    }
    ir::UncondJump(landing);
    ir::BasicBlock::Current = landing;
  } else {
    ir::Store(from_type, ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    Type const *best_match = from_type;
    best_match->EmitCopyAssign(from_type, from, ir::VariantValue(best_match, to),
                           ctx);
  }
}

void Variant::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                             ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg_ == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  EmitDestroy(to.reg_, ctx);

  if (from_type->is<Variant>()) {
    auto actual_type =
        ir::Load<type::Type const *>(ir::VariantType(from.get<ir::Reg>(0)));
    auto landing = ir::CompiledFn::Current->AddBlock();
    for (Type const *v : from_type->as<Variant>().variants_) {
      auto next_block = ir::CompiledFn::Current->AddBlock();
      ir::BasicBlock::Current =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitMoveAssign(
          v,
          ir::Results{ir::PtrFix(ir::VariantValue(v, from.get<ir::Reg>(0)), v)},
          ir::VariantValue(v, to), ctx);
      ir::UncondJump(landing);
      ir::BasicBlock::Current = next_block;
    }
    ir::UncondJump(landing);
    ir::BasicBlock::Current = landing;
  } else {
    ir::Store(from_type, ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    Type const *best_match = from_type;
    best_match->EmitMoveAssign(from_type, from,
                               ir::VariantValue(best_match, to), ctx);
  }
}

void Variant::EmitInit(ir::Reg, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Variant::EmitRepr(ir::Results const &id_val, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression const *>{nullptr, this}}));

    CURRENT_FUNC(repr_func_) {
      ir::BasicBlock::Current = repr_func_->entry();
      auto landing            = ir::CompiledFn::Current->AddBlock();
      auto type =
          ir::Load<type::Type const *>(ir::VariantType(ir::Reg::Arg(0)));

      for (const Type *v : variants_) {
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::CompiledFn::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitRepr(
            ir::Results{ir::PtrFix(ir::VariantValue(v, ir::Reg::Arg(0)), v)},
            ctx);
        ir::UncondJump(landing);

        ir::BasicBlock::Current = old_block;
        ir::BasicBlock::Current = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegisterOr<type::Type const *>(type), v));
      }

      ir::UncondJump(landing);
      ir::BasicBlock::Current = landing;
      ir::ReturnJump();
    }
  }

  ir::Call(ir::AnyFunc{repr_func_}, ir::Arguments{repr_func_->type_, id_val});
}

void Variant::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  for (auto *v : variants_) { v->defining_modules(modules); }
}

void Variant::WriteTo(std::string *result) const {
  auto iter = variants_.begin();

  // TODO Will you ever need parentheses?
  (*iter)->WriteTo(result);

  ++iter;
  for (; iter != variants_.end(); ++iter) {
    result->append(" | ");
    (*iter)->WriteTo(result);
  }
}

bool Variant::IsCopyable() const {
  return std::all_of(variants_.begin(), variants_.end(),
                     [](Type const *t) { return t->IsCopyable(); });
}

bool Variant::IsMovable() const {
  return std::all_of(variants_.begin(), variants_.end(),
                     [](Type const *t) { return t->IsMovable(); });
}

ir::Results Variant::PrepareArgument(Type const *from, ir::Results const &val,
                                     Context *ctx) const {
  if (this == from) { return val; }
  auto alloc_reg = ir::Alloca(this);

  if (!from->is<Variant>()) {
    Type_->EmitMoveAssign(Type_, ir::Results{from}, ir::VariantType(alloc_reg),
                          ctx);
    // TODO this isn't exactly right because 'from' might not be the appropriate
    // type here.
    // TODO this is actually the wrong type to plug in to VariantValue. It needs
    // to be the precise type stored.
    from->EmitMoveAssign(from, val, ir::VariantValue(from, alloc_reg), ctx);
  } else {
    auto *from_v = &from->as<Variant>();
    auto runtime_type =
        ir::Load<Type const *>(ir::VariantType(val.get<ir::Reg>(0)));

    // Because variants_ is sorted, we can find the intersection quickly:
    std::vector<Type const *> intersection;
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
    ASSERT(intersection.size() != 0u);

    auto landing = ir::CompiledFn::Current->AddBlock();

    std::vector<ir::BlockIndex> blocks;
    blocks.reserve(intersection.size());
    for (auto *t : intersection) {
      blocks.push_back(ir::CompiledFn::Current->AddBlock());
    }

    auto current = ir::BasicBlock::Current;
    for (size_t i = 0; i < intersection.size(); ++i) {
      ir::BasicBlock::Current = blocks[i];
      this->EmitMoveAssign(
          intersection[i],
          ir::Results{
              ir::PtrFix(ir::VariantValue(intersection[i], val.get<ir::Reg>(0)),
                         intersection[i])},
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
  return ir::Results{alloc_reg};
}

core::Bytes Variant::bytes(core::Arch const &a) const {
  auto num_bytes = core::Bytes{0};
  auto align = core::Alignment{1};
  for (auto const *t : variants_) {
    align     = std::max(align, t->alignment(a));
    num_bytes = std::max(num_bytes, t->bytes(a));
  }
  return core::FwdAlign(Type_->bytes(a), align) + num_bytes;
}

core::Alignment Variant::alignment(core::Arch const &a) const {
  auto align = Type_->alignment(a);
  for (auto const *t : variants_) { align = std::max(align, t->alignment(a)); }
  return align;
}

Cmp Variant::Comparator() const {
  using cmp_t = std::underlying_type_t<Cmp>;
  auto cmp    = static_cast<cmp_t>(Cmp::Equality);
  for (Type const *t : variants_) {
    cmp = std::min(cmp, static_cast<cmp_t>(t->Comparator()));
  }
  return static_cast<Cmp>(cmp);
}

bool Variant::ReinterpretAs(Type const *t) const {
  auto *v = t->if_as<Variant>();
  if (!v) { return false; }
  // Every type in this variant needs to be reinterprettable as a type in v
  // exactly once. The problem is this isn't quite enough because the to-type
  // could have another member that's much larger. This violates the
  // size-doesnt-chnage invariant.
  for (auto *this_v : variants_) {
    if (absl::c_count_if(v->variants_, [this_v](Type const *to) {
          return this_v->ReinterpretAs(to);
        }) == 1) {
      continue;
    } else {
      return false;
    }
  }
  return true;
}

}  // namespace type
