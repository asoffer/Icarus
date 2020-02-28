#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/expr_ptr.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "ir/results.h"
#include "type/jump.h"

namespace compiler {

Compiler::Compiler(CompiledModule *mod,
                   diagnostic::DiagnosticConsumer &consumer)
    : data_(mod->root_data()),
      current_constants_(mod->root_node()),
      diag_consumer_(consumer) {}

type::QualType const *Compiler::qual_type_of(ast::ExprPtr expr) const {
  if (auto *decl = expr.get()->if_as<ast::Declaration>()) {
    // If the declarations module is the same as this one, we haven't completed
    // compiling it yet and so we need to access it through the compiler.
    // Otherwise, we have finished compiling, so we access it through the
    // module.
    //
    // TODO This could be a TestModule which doesn't have a .type_of(). we
    // really shouldn't need to pay for the check here.
    if (auto const *mod =
            ASSERT_NOT_NULL(decl->module())->if_as<CompiledModule>()) {
      if (mod != module()) { return mod->qual_type_of(decl); }
      if (auto *t = current_constants_->binding().type_of(decl)) {
        // TODO obviously this is nonsense, but we don't store a stable QualType
        // pointer in constants.
        return new type::QualType(type::QualType::Constant(t));
      }
    }
  }

  if (auto *result = data_.result(expr)) { return result; }

  // TODO embedded modules?
  return nullptr;
}

type::Type const *Compiler::type_of(ast::Expression const *expr) const {
  auto *q = qual_type_of(expr);
  return q ? q->type() : nullptr;
}

void Compiler::set_addr(ast::Declaration const *decl, ir::Reg addr) {
  data_.addr_[decl] = addr;
}
type::QualType Compiler::set_result(ast::ExprPtr expr, type::QualType r) {
  return data_.set_result(expr, r);
}

ir::Reg Compiler::addr(ast::Declaration const *decl) const {
  return data_.addr_.at(decl);
}

void Compiler::set_dispatch_table(ast::ExprPtr expr,
                                  ast::DispatchTable &&table) {
  // TODO data_.dispatch_tables_.emplace(expr,
  // std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

void Compiler::set_jump_table(ast::ExprPtr jump_expr, ast::ExprPtr node,
                              ast::DispatchTable &&table) {
  // TODO data_.jump_tables_.emplace(std::pair{jump_expr,
  // node},
  //                                        std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

void Compiler::set_pending_module(ast::Import const *import_node,
                                  module::Pending<LibraryModule> mod) {
  data_.imported_module_.emplace(import_node, std::move(mod));
}

ast::DispatchTable const *Compiler::dispatch_table(ast::ExprPtr expr) const {
  /* TODO auto &table = data_.dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }*/
  return nullptr;
}

ast::DispatchTable const *Compiler::jump_table(ast::ExprPtr jump_expr,
                                               ast::ExprPtr node) const {
  /* TODOauto &table = data_.jump_tables_;
  if (auto iter = table.find(std::pair(jump_expr, node)); iter != table.end()) {
    return &iter->second;
  }*/
  return nullptr;
}

module::Pending<LibraryModule> *Compiler::pending_module(
    ast::Import const *import_node) const {
  if (auto iter = data_.imported_module_.find(import_node);
      iter != data_.imported_module_.end()) {
    return &iter->second;
  }
  return nullptr;
}

void Compiler::CompleteDeferredBodies() {
  base::move_func<void()> f;
  while (true) {
    {
      auto handle = data_.deferred_work_.lock();
      if (handle->empty()) { return; }
      auto nh = handle->extract(handle->begin());
      DEBUG_LOG("CompleteDeferredBodies")(nh.key()->DebugString());
      f = std::move(nh.mapped());
    }
    if (f) { std::move(f)(); }
  }
}

ir::CompiledFn *Compiler::AddFunc(
    type::Function const *fn_type,
    core::Params<type::Typed<ast::Declaration const *>> params) {
  return data_.fns_
      .emplace_back(
          std::make_unique<ir::CompiledFn>(fn_type, std::move(params)))
      .get();
}

ir::CompiledFn Compiler::MakeThunk(ast::Expression const *expr,
                                   type::Type const *type) {
  ir::CompiledFn fn(type::Func({}, {ASSERT_NOT_NULL(type)}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(&fn)) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    builder().CurrentBlock() = fn.entry();

    auto vals = Visit(expr, compiler::EmitValueTag{});
    // TODO wrap this up into SetRet(vector)
    std::vector<type::Type const *> extracted_types;
    if (auto *tup = type->if_as<type::Tuple>()) {
      extracted_types = tup->entries_;
    } else {
      extracted_types = {type};
    }

    // TODO is_big()?
    for (size_t i = 0; i < vals.size(); ++i) {
      auto const *t = extracted_types[i];
      if (t->is_big()) {
        // TODO must `r` be holding a register?
        // TODO guaranteed move-elision

        ASSERT(vals.GetResult(i).size() == 1u);
        EmitMoveInit(t, vals.GetResult(i),
                     type::Typed(ir::GetRet(i, t), type::Ptr(t)));

      } else {
        ir::SetRet(i, type::Typed{vals.GetResult(i), t});
      }
    }
    builder().ReturnJump();
  }
  fn.WriteByteCode();

  return fn;
}

}  // namespace compiler
