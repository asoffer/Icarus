#include "backend/eval.h"

#include <iomanip>
#include "architecture.h"
#include "ast/expression.h"
#include "backend/exec.h"
#include "context.h"
#include "ir/func.h"
#include "type/all.h"

namespace backend {
void Execute(IR::Func *fn, const base::untyped_buffer &arguments,
             const base::vector<IR::Addr> &ret_slots, backend::ExecContext *ctx);

static std::unique_ptr<IR::Func> ExprFn(type::Type const *expr_type,
                                        AST::Expression *expr, Context *ctx) {
  ASSERT(expr_type != nullptr);
  auto fn = std::make_unique<IR::Func>(
      ctx->mod_, type::Func({}, {expr_type}),
      base::vector<std::pair<std::string, AST::Expression *>>{});
  CURRENT_FUNC(fn.get()) {
    // TODO this is essentially a copy of the body of GeneratedFunction::EmitIR.
    // Factor these out together.
    IR::BasicBlock::Current = fn->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).

    auto start_block = IR::BasicBlock::Current = IR::Func::Current->AddBlock();

    ASSERT(ctx != nullptr);
    auto vals = expr->EmitIR(ctx);
    // TODO wrap this up into SetReturn(vector)
    for (size_t i = 0; i < vals.size(); ++i) {
      if (!vals[i].type) {
        LOG << vals[i];
      }
      IR::SetReturn(i, std::move(vals[i]));
    }
    IR::ReturnJump();

    IR::BasicBlock::Current = fn->entry();
    IR::UncondJump(start_block);
  }
  return fn;
}

base::untyped_buffer EvaluateToBuffer(AST::Expression *expr, Context *ctx) {
  if (ctx->num_errors() != 0u) {
    ctx->DumpErrors();
    UNREACHABLE();
  }

  auto *expr_type = ctx->mod_->types_.at(expr);
  auto fn = ExprFn(expr_type, expr, ctx);

  size_t bytes_needed = Architecture::InterprettingMachine().bytes(expr_type);
  base::untyped_buffer ret_buf(bytes_needed);
  ret_buf.append_bytes(bytes_needed, 1);
  base::vector<IR::Addr> ret_slots;

  IR::Addr addr;
  addr.kind    = IR::Addr::Kind::Heap;
  addr.as_heap = ret_buf.raw(0);
  ret_slots.push_back(addr);
  backend::ExecContext exec_context;
  Execute(fn.get(), base::untyped_buffer(0), ret_slots, &exec_context);
  return ret_buf;
}

// TODO migrate to untyped_buffer
base::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx) {
  if (ctx->num_errors() != 0) { return {}; }

  auto result_buf = EvaluateToBuffer(expr, ctx);

  auto expr_type = ctx->mod_->types_.at(expr);
  base::vector<type::Type const *> types =
      expr_type->is<type::Tuple>()
          ? expr_type->as<type::Tuple>().entries_
          : base::vector<type::Type const *>{expr_type};

  base::vector<IR::Val> results;
  results.reserve(types.size());

  auto arch     = Architecture::InterprettingMachine();
  size_t offset = 0;
  for (auto *t : types) {
    offset = arch.MoveForwardToAlignment(ASSERT_NOT_NULL(t), offset);
    if (t == type::Bool) {
      results.emplace_back(result_buf.get<bool>(offset));
    } else if (t == type::Char) {
      results.emplace_back(result_buf.get<char>(offset));
    } else if (t == type::Int) {
      results.emplace_back(result_buf.get<i32>(offset));
    } else if (t == type::Real) {
      results.emplace_back(result_buf.get<double>(offset));
    } else if (t == type::Type_) {
      results.emplace_back(result_buf.get<type::Type const *>(offset));
    } else if (t->is<type::CharBuffer>()) {
      results.push_back(IR::Val::CharBuf(
          std::string(result_buf.get<std::string_view>(offset))));
    } else if (t->is<type::Function>()) {
      // TODO foreign func, etc?
      auto any_func = result_buf.get<IR::AnyFunc>(offset);
      results.push_back(any_func.is_fn_
                            ? IR::Val::Func(any_func.fn_)
                            : IR::Val::Foreign(t, any_func.foreign_));
    } else if (t->is<type::Scope>()) {
      results.emplace_back(result_buf.get<AST::ScopeLiteral *>(offset));
    } else if (t == type::Module) {
      results.emplace_back(result_buf.get<Module const *>(offset));
    } else if (t == type::Generic || t->is<type::Function>()) {
      // TODO mostly wrong.
      results.push_back(IR::Val::Func(result_buf.get<AST::Function *>(offset)));
    } else if (t == type::Block || t == type::OptBlock) {
      results.push_back(
          IR::Val::BlockSeq(result_buf.get<IR::BlockSequence>(offset)));
    } else {
      NOT_YET(t->to_string());
    }

    offset += arch.bytes(t);
  }

  return results;
}
}  // namespace backend
