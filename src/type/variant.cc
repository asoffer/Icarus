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

}  // namespace type
