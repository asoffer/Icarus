#include "backend/eval.h"

#include "ast/expression.h"
#include "backend/exec.h"
#include "compiler/compiler.h"
#include "core/arch.h"
#include "ir/builder.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/return.h"
#include "ir/compiled_fn.h"
#include "type/generic_struct.h"
#include "type/util.h"

namespace backend {
static ir::CompiledFn ExprFn(compiler::Compiler *visitor,
                             type::Typed<ast::Expression const *> typed_expr) {
  ir::CompiledFn fn(visitor->module(),
                    type::Func({}, {ASSERT_NOT_NULL(typed_expr.type())}),
                    core::FnParams<type::Typed<ast::Expression const *>>{});
  ICARUS_SCOPE(ir::SetCurrentFunc(&fn)) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    ir::GetBuilder().CurrentBlock() = fn.entry();

    auto vals = typed_expr.get()->EmitValue(visitor);
    // TODO wrap this up into SetRet(vector)
    std::vector<type::Type const *> extracted_types;
    if (auto *tup = typed_expr.type()->if_as<type::Tuple>()) {
      extracted_types = tup->entries_;
    } else {
      extracted_types = {typed_expr.type()};
    }
    for (size_t i = 0; i < vals.size(); ++i) {
      ir::SetRet(i, type::Typed{vals.GetResult(i), extracted_types.at(i)});
    }
    ir::ReturnJump();

    visitor->CompleteDeferredBodies();
  }
  return fn;
}

base::untyped_buffer EvaluateToBuffer(
    type::Typed<ast::Expression const *> typed_expr,
    compiler::Compiler *visitor) {
  DEBUG_LOG("eval")(ast::Dump::ToString(typed_expr.get()));
  DEBUG_LOG("eval")(*typed_expr.type());
  auto fn = ExprFn(visitor, typed_expr);

  size_t bytes_needed =
      typed_expr.type()->bytes(core::Interpretter()).value();
  base::untyped_buffer ret_buf(bytes_needed);
  ret_buf.append_bytes(bytes_needed, 1);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  ExecContext exec_context;
  Execute(&fn, base::untyped_buffer(0), ret_slots, &exec_context);
  return ret_buf;
}

ir::Results Evaluate(type::Typed<ast::Expression const *> typed_expr,
                     compiler::Compiler *visitor) {
  // TODO is the error-case distinguishible from successfully returning void?
  if (visitor->num_errors() != 0) { return ir::Results{}; }

  ASSERT(typed_expr.type() != nullptr);
  std::vector<uint32_t> offsets;
  auto buf = EvaluateToBuffer(typed_expr, visitor);

  if (auto *tup = typed_expr.type()->if_as<type::Tuple>()) {
    offsets.reserve(tup->entries_.size());
    auto arch   = core::Interpretter();
    auto offset = core::Bytes{0};
    for (auto *t : tup->entries_) {
      offset = core::FwdAlign(offset, t->alignment(arch));
      offsets.push_back(offset.value());
      offset += t->bytes(arch);
    }
  } else {
    offsets.push_back(0);
  }

  return ir::Results::FromUntypedBuffer(std::move(offsets), std::move(buf));
}

}  // namespace backend
