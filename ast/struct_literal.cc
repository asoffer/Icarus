#include "ast/struct_literal.h"

#include <sstream>
#include "ast/declaration.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

namespace ir {
// TODO: The functions here that modify struct fields typically do so by
// modifying the last field, since we always build them in order. This saves us
// from having to pass extra information and thereby bloating all commands. At
// some point we should switch to a buffer-chunk system so that one won't bloat
// another.
Reg CreateStruct(core::Scope const *scope,
                      ast::StructLiteral const *parent);
void CreateStructField(Reg struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(Reg struct_type, std::string_view field_name);
void AddHashtagToField(Reg struct_type, ast::Hashtag hashtag);
void AddHashtagToStruct(Reg struct_type, ast::Hashtag hashtag);
Reg FinalizeStruct(Reg r);

Reg ArgumentCache(ast::StructLiteral const *sl);
}  // namespace ir

namespace ast {
std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "struct (";
  for (auto &a : args_) { ss << a.to_string(n) << ", "; }
  ss << ") {\n";
  for (const auto &f : fields_) {
    ss << std::string((n + 1) * 2, ' ') << f.to_string(n) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void StructLiteral::CompleteBody(Context *ctx) const {
  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[this];
  for (size_t i = 0; i < args_.size(); ++i) {
    ctx->set_addr(&args_[i], ir::Reg::Arg(i));
  }

  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    auto cache_slot_addr    = ir::ArgumentCache(this);
    auto cache_slot         = ir::Load<type::Type const *>(cache_slot_addr);

    auto land_block         = ir::CompiledFn::Current->AddBlock();
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        land_block,
        ir::Eq(cache_slot, static_cast<type::Type const *>(nullptr)));
    auto ctx_reg    = ir::CreateContext(ctx->mod_);
    auto struct_reg = ir::CreateStruct(scope_, this);

    // TODO why isn't implicit TypedRegister -> RegisterOr cast working on
    // either of these? On the first it's clear because we don't even return a
    // typedRegister, but this is a note to remind you to make that work. On the
    // second... I don't know.
    ir::Store(static_cast<ir::RegisterOr<type::Type const *>>(struct_reg),
              cache_slot_addr);
    for (auto &arg : args_) {  // TODO const-ref
      ir::AddBoundConstant(ctx_reg, &arg, ctx->addr(&arg));
    }

    for (auto &field : fields_) { // TODO const-ref
      ir::VerifyType(&field, ctx_reg);

      // TODO exit early if verifytype fails.

      auto type_reg = ir::EvaluateAsType(field.type_expr.get(), ctx_reg);

      ir::CreateStructField(struct_reg, type_reg);
      ir::SetStructFieldName(struct_reg, field.id_);

      for (auto const &hashtag : field.hashtags_) {
        ir::AddHashtagToField(struct_reg, hashtag);
      }
    }

    for (auto hashtag : hashtags_) {
      ir::AddHashtagToStruct(struct_reg, hashtag);
    }

    ir::RegisterOr<type::Type const *> result = ir::FinalizeStruct(struct_reg);
    ir::DestroyContext(ctx_reg);

    // Exit path from creating a new struct.
    ir::SetRet(0, static_cast<ir::RegisterOr<type::Type const *>>(result));
    ir::Store(static_cast<ir::RegisterOr<type::Type const *>>(result),
              cache_slot_addr);
    ir::ReturnJump();

    // Exit path from finding the cache
    ir::BasicBlock::Current = land_block;
    ir::SetRet(0, static_cast<ir::RegisterOr<type::Type const *>>(cache_slot));
    ir::ReturnJump();
  }
}
}  // namespace ast
