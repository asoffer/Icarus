#include "type/variant.h"

#include <algorithm>

#include "base/container/map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"

namespace type {

bool Variant::contains(Type const *t) const {
  // TODO can do a binary search.
  for (auto *v : variants_) {
    if (v == t) { return true; }
  }
  return false;
}

static base::guarded<base::map<base::vector<Type const *>, Variant>> variants_;
Type const *Var(base::vector<Type const *> variants) {
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

  return &variants_.lock()
              ->emplace(std::piecewise_construct,
                        std::forward_as_tuple(variants),
                        std::forward_as_tuple(variants))
              .first->second;
}

void Variant::EmitDestroy(ir::Register reg, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(mtx_);
  if (!destroy_func_) {
    destroy_func_ = ctx->mod_->AddFunc(Func({this}, {}),
                                       ast::FnParams<ast::Expression *>(1));
    CURRENT_FUNC(destroy_func_) {
      ir::BasicBlock::Current = destroy_func_->entry();
      auto landing            = ir::Func::Current->AddBlock();
      auto type =
          ir::Load<Type const *>(ir::VariantType(destroy_func_->Argument(0)));

      for (Type const *v : variants_) {
        if (!v->needs_destroy()) { continue; }
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::Func::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitDestroy(
            ir::PtrFix(ir::VariantValue(v, destroy_func_->Argument(0)), v),
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

  ir::Arguments call_args;
  call_args.append(reg);
  call_args.type_ = destroy_func_->type_;
  ir::Call(ir::AnyFunc{destroy_func_}, std::move(call_args));
}

bool Variant::needs_destroy() const {
  return std::any_of(variants_.begin(), variants_.end(),
                     [](Type const *t) { return t->needs_destroy(); });
}

void Variant::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                             ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg_);
  // TODO have EmitDestroy take RegistorOr<Addr>
  EmitDestroy(to.reg_, ctx);

  if (from_type->is<Variant>()) {
    auto actual_type = ir::Load<type::Type const *>(
        ir::VariantType(std::get<ir::Register>(from.value)));
    auto landing = ir::Func::Current->AddBlock();
    for (Type const *v : from_type->as<Variant>().variants_) {
      auto next_block = ir::Func::Current->AddBlock();
      ir::BasicBlock::Current =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitCopyAssign(
          v,
          ir::Val::Reg(
              ir::PtrFix(
                  ir::VariantValue(v, std::get<ir::Register>(from.value)), v),
              v),
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

void Variant::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                             ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg_);
  // TODO have EmitDestroy take RegistorOr<Addr>
  EmitDestroy(to.reg_, ctx);

  if (from_type->is<Variant>()) {
    auto actual_type = ir::Load<type::Type const *>(
        ir::VariantType(std::get<ir::Register>(from.value)));
    auto landing = ir::Func::Current->AddBlock();
    for (Type const *v : from_type->as<Variant>().variants_) {
      auto next_block = ir::Func::Current->AddBlock();
      ir::BasicBlock::Current =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitMoveAssign(
          v,
          ir::Val::Reg(
              ir::PtrFix(
                  ir::VariantValue(v, std::get<ir::Register>(from.value)), v),
              v),
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

void Variant::EmitInit(ir::Register, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Variant::EmitRepr(ir::Val const &id_val, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(Func({this}, {}),
                                    ast::FnParams<ast::Expression *>(1));

    CURRENT_FUNC(repr_func_) {
      ir::BasicBlock::Current = repr_func_->entry();
      auto landing            = ir::Func::Current->AddBlock();
      auto type               = ir::Load<type::Type const *>(
          ir::VariantType(repr_func_->Argument(0)));

      for (const Type *v : variants_) {
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::Func::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitRepr(
            ir::Val::Reg(
                ir::PtrFix(ir::VariantValue(v, repr_func_->Argument(0)), v), v),
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

  ir::Arguments call_args;
  call_args.append(id_val);
  call_args.type_ = repr_func_->type_;
  ir::Call(ir::AnyFunc{repr_func_}, std::move(call_args));
}

void Variant::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
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

}  // namespace type
