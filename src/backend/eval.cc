#include "backend/eval.h"

#include "ast/expression.h"
#include "backend/exec.h"
#include "context.h"
#include "ir/func.h"
#include "type/function.h"

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

namespace IR {
// TODO namespace migration.
base::vector<Val> Execute(Func *fn, const base::vector<Val> &arguments,
                         ExecContext *ctx);
}  // namespace IR

namespace backend {
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

base::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx) {
  IR::ExecContext exec_context;
  // TODO wire through errors.
  auto fn = ExprFn(expr, ctx);
  if (ctx->num_errors() == 0) {
    return Execute(fn.get(), {}, &exec_context);
  } else {
    return {};
  }
}
}  // namespace backend
