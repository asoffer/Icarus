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

static std::unique_ptr<IR::Func> ExprFn(AST::Expression *expr, Context *ctx) {
  ASSERT(expr->type != nullptr);
  auto fn = std::make_unique<IR::Func>(
      ctx->mod_, type::Func({}, {expr->type}),
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

  auto fn = ExprFn(expr, ctx);

  size_t bytes_needed = Architecture::InterprettingMachine().bytes(expr->type);
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

  base::vector<type::Type const *> types =
      expr->type->is<type::Tuple>()
          ? expr->type->as<type::Tuple>().entries_
          : base::vector<type::Type const *>{expr->type};

  base::vector<IR::Val> results;
  results.reserve(types.size());

  auto arch     = Architecture::InterprettingMachine();
  size_t offset = 0;
  for (auto *t : types) {
    offset = arch.MoveForwardToAlignment(t, offset);
    if (t == type::Bool) {
      results.push_back(IR::Val::Bool(result_buf.get<bool>(offset)));
    } else if (t == type::Char) {
      results.push_back(IR::Val::Char(result_buf.get<char>(offset)));
    } else if (t == type::Int) {
      results.push_back(IR::Val::Int(result_buf.get<i32>(offset)));
    } else if (t == type::Real) {
      results.push_back(IR::Val::Real(result_buf.get<double>(offset)));
    } else if (t == type::Type_) {
      results.push_back(
          IR::Val::Type(result_buf.get<type::Type const *>(offset)));
    } else if (t->is<type::CharBuffer>()) {
      results.push_back(IR::Val::CharBuf(
          std::string(result_buf.get<std::string_view>(offset))));
    } else if (t->is<type::Function>()) {
      // TODO foreign func, etc?
      results.push_back(IR::Val::Func(result_buf.get<IR::Func *>(offset)));
    } else if (t->is<type::Scope>()) {
      // TODO foreign func, etc?
      results.push_back(
          IR::Val::Scope(result_buf.get<AST::ScopeLiteral *>(offset)));
    } else if (t == type::Module) {
      results.push_back(IR::Val::Mod(result_buf.get<Module const *>(offset)));
    } else if (t == type::Generic) {
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
