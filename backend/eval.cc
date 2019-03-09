#include "backend/eval.h"

#include <iomanip>
#include "ast/expression.h"
#include "backend/exec.h"
#include "ir/func.h"
#include "layout/arch.h"
#include "misc/context.h"
#include "type/generic_struct.h"
#include "type/util.h"

namespace backend {
static std::unique_ptr<ir::Func> ExprFn(
    type::Typed<ast::Expression *> typed_expr, Context *ctx) {
  auto fn = std::make_unique<ir::Func>(
      ctx->mod_, type::Func({}, {ASSERT_NOT_NULL(typed_expr.type())}),
      core::FnParams<ast::Expression *>{});

  CURRENT_FUNC(fn.get()) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitIr
    // Factor these out together.
    ir::BasicBlock::Current = fn->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).

    auto start_block = ir::BasicBlock::Current = ir::Func::Current->AddBlock();

    ASSERT(ctx != nullptr);
    auto vals = typed_expr.get()->EmitIr(ctx);
    // TODO wrap this up into SetRet(vector)
    std::vector<type::Type const *> extracted_types;
    if (auto *tup = typed_expr.type()->if_as<type::Tuple>()) {
      extracted_types = tup->entries_;
    } else {
      extracted_types = {typed_expr.type()};
    }
    for (size_t i = 0; i < vals.size(); ++i) {
      ir::SetRet(i, type::Typed{vals.GetResult(i), extracted_types.at(i)}, ctx);
    }
    ir::ReturnJump();

    ir::BasicBlock::Current = fn->entry();
    ir::UncondJump(start_block);
  }
  return fn;
}

base::untyped_buffer EvaluateToBuffer(type::Typed<ast::Expression *> typed_expr,
                                      Context *ctx) {
  auto fn = ExprFn(typed_expr, ctx);

  size_t bytes_needed =
      typed_expr.type()->bytes(layout::Interpretter()).value();
  base::untyped_buffer ret_buf(bytes_needed);
  ret_buf.append_bytes(bytes_needed, 1);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  backend::ExecContext exec_context;
  Execute(fn.get(), base::untyped_buffer(0), ret_slots, &exec_context);
  return ret_buf;
}

std::vector<ir::Val> Evaluate(type::Typed<ast::Expression *> typed_expr,
                               Context *ctx) {
  if (ctx->num_errors() != 0) { return {}; }

  // TODO migrate to untyped_buffer
  ASSERT(typed_expr.type() != nullptr);
  auto result_buf = EvaluateToBuffer(typed_expr, ctx);

  std::vector<type::Type const *> types =
      typed_expr.type()->is<type::Tuple>()
          ? typed_expr.type()->as<type::Tuple>().entries_
          : std::vector<type::Type const *>{typed_expr.type()};

  std::vector<ir::Val> results;
  results.reserve(types.size());

  auto arch     = layout::Interpretter();
  auto offset = layout::Bytes{0};
  for (auto *t : types) {
    offset = layout::FwdAlign(offset, t->alignment(arch));
    if (t == type::Scope || t == type::StatefulScope) {
      results.emplace_back(result_buf.get<ast::ScopeLiteral *>(offset.value()));
    } else if (t == type::ByteView) {
      results.emplace_back(result_buf.get<std::string_view>(offset.value()));
    } else if (t->is<type::Function>() || t->is<type::GenericStruct>()) {
      // TODO foreign func, etc?
      auto any_func = result_buf.get<ir::AnyFunc>(offset.value());
      results.push_back(ir::Val::Func(t, any_func));
    } else if (t == type::Module) {
      results.emplace_back(result_buf.get<Module *>(offset.value()));
    } else if (t == type::Generic || t->is<type::Function>()) {
      // TODO mostly wrong.
      results.push_back(ir::Val::Func(
          result_buf.get<ast::FunctionLiteral *>(offset.value())));
    } else if (t->is<type::Block>() || t == type::OptBlock) {
      results.push_back(
          ir::Val::BlockSeq(result_buf.get<ir::BlockSequence>(offset.value())));
    } else {
      type::Apply(t, [&](auto type_holder) -> void {
        using T = typename decltype(type_holder)::type;
        if constexpr (std::is_same_v<T, ir::EnumVal>) {
          results.emplace_back(type::Typed<ir::EnumVal, type::Enum>(
              result_buf.get<ir::EnumVal>(offset.value()),
              &t->as<type::Enum>()));
        } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
          results.emplace_back(type::Typed<ir::FlagsVal, type::Flags>(
              result_buf.get<ir::FlagsVal>(offset.value()),
              &t->as<type::Flags>()));
        } else {
          results.emplace_back(result_buf.get<T>(offset.value()));
        }
      });
    }

    offset += t->bytes(arch);
  }

  return results;
}

std::vector<ir::Val> Evaluate(ast::Expression *expr, Context *ctx) {
  return Evaluate({expr, ASSERT_NOT_NULL(ctx->type_of(expr))}, ctx);
}
}  // namespace backend
