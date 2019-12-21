#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/expr_ptr.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/library_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "ir/results.h"
#include "type/jump.h"

namespace compiler {

std::unique_ptr<module::BasicModule> CompileLibraryModule(
    frontend::Source *src) {
  auto mod = std::make_unique<LibraryModule>(
      [](base::PtrSpan<ast::Node const> nodes, CompiledModule *mod) {
        diagnostic::StreamingConsumer consumer(stderr);
        compiler::Compiler c(mod, consumer);
        for (ast::Node const *node : nodes) { c.Visit(node, VerifyTypeTag{}); }
        if (c.num_errors() > 0) { return; }

        for (ast::Node const *node : nodes) { c.Visit(node, EmitValueTag{}); }
        c.CompleteDeferredBodies();

        mod->dep_data_   = std::move(c.data_.dep_data_);
        mod->fns_        = std::move(c.data_.fns_);
        mod->scope_defs_ = std::move(c.data_.scope_defs_);
        mod->block_defs_ = std::move(c.data_.block_defs_);
        mod->jumps_      = std::move(c.data_.jumps_);
      });
  mod->Process(frontend::Parse(src));
  return mod;
}

Compiler::Compiler(module::BasicModule *mod,
                   diagnostic::DiagnosticConsumer &consumer)
    : data_(mod), diag_consumer_(consumer) {}

type::QualType const *Compiler::prior_verification_attempt(ast::ExprPtr expr) {
  return data_.constants_->second.result(expr);
}

type::Type const *Compiler::type_of(ast::Expression const *expr) const {
  if (auto *decl = expr->if_as<ast::Declaration>()) {
    // If the declarations module is the same as this one, we haven't completed
    // compiling it yet and so we need to access it through the compiler.
    // Otherwise, we have finished compiling, so we access it through the
    // module.
    //
    // TODO This could be a TestModule which doesn't have a .type_of(). we
    // really shouldn't need to pay for the check here.
    if (auto const *mod =
            ASSERT_NOT_NULL(decl->module())->if_as<CompiledModule>()) {
      if (mod != module()) { return mod->type_of(decl); }
      if (auto *t = data_.current_constants_.type_of(decl)) { return t; }
    }
  }

  auto *result = data_.constants_->second.result(expr);
  if (result and result->type()) { return result->type(); }

  // TODO reenable once modules are all in core.
  // // When searching in embedded modules we intentionally look with no bound
  // // constants. Across module boundaries, a declaration can't be present
  // anyway. for (module::BasicModule const *mod :
  // mod_->scope_.embedded_modules_) {
  //   // TODO use right constants
  //   if (auto iter = mod->dep_data_.front().second.verify_results_.find(expr);
  //       iter != mod->dep_data_.front().second.verify_results_.end()) {
  //     return iter->second.type_;
  //   }
  // }
  return nullptr;
}

void Compiler::set_addr(ast::Declaration const *decl, ir::Reg addr) {
  data_.constants_->second.addr_[decl] = addr;
}
type::QualType Compiler::set_result(ast::ExprPtr expr, type::QualType r) {
  return data_.constants_->second.set_result(expr, r);
}

ir::Reg Compiler::addr(ast::Declaration const *decl) const {
  return data_.constants_->second.addr_.at(decl);
}

void Compiler::set_dispatch_table(ast::ExprPtr expr,
                                  ast::DispatchTable &&table) {
  // TODO data_.constants_->second.dispatch_tables_.emplace(expr,
  // std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

std::pair<ConstantBinding, DependentData> *Compiler::insert_constants(
    ConstantBinding const &constant_binding) {
  // TODO remove this iteration
  for (auto iter = data_.dep_data_.begin(); iter != data_.dep_data_.end();
       ++iter) {
    auto &[key, val] = *iter;
    if (key == constant_binding) { return &*iter; }
  }
  auto *pair =
      &data_.dep_data_.emplace_front(constant_binding, DependentData{});
  pair->second.constants_ = pair->first;

  for (auto const &[decl, binding] : constant_binding.keys_) {
    pair->second.set_result(decl, type::QualType::Constant(binding.type_));
  }
  return pair;
}

void Compiler::set_jump_table(ast::ExprPtr jump_expr, ast::ExprPtr node,
                              ast::DispatchTable &&table) {
  // TODO data_.constants_->second.jump_tables_.emplace(std::pair{jump_expr,
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
                                  module::PendingModule mod) {
  data_.constants_->second.imported_module_.emplace(import_node,
                                                    std::move(mod));
}

ast::DispatchTable const *Compiler::dispatch_table(ast::ExprPtr expr) const {
  /* TODO auto &table = data_.constants_->second.dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }*/
  return nullptr;
}

ast::DispatchTable const *Compiler::jump_table(ast::ExprPtr jump_expr,
                                               ast::ExprPtr node) const {
  /* TODOauto &table = data_.constants_->second.jump_tables_;
  if (auto iter = table.find(std::pair(jump_expr, node)); iter != table.end()) {
    return &iter->second;
  }*/
  return nullptr;
}

module::PendingModule *Compiler::pending_module(
    ast::Import const *import_node) const {
  if (auto iter = data_.constants_->second.imported_module_.find(import_node);
      iter != data_.constants_->second.imported_module_.end()) {
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
      f       = std::move(nh.mapped());
    }
    std::move(f)();
  }
}

ir::CompiledFn *Compiler::AddFunc(
    type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Declaration const *>> params) {
  return data_.fns_
      .emplace_back(
          std::make_unique<ir::CompiledFn>(fn_type, std::move(params)))
      .get();
}

ir::CompiledFn *Compiler::AddJump(
    type::Jump const *jump_type,
    core::FnParams<type::Typed<ast::Declaration const *>> params) {
  return data_.fns_
      .emplace_back(std::make_unique<ir::CompiledFn>(jump_type->ToFunction(),
                                                     std::move(params)))
      .get();
}

ir::CompiledFn Compiler::MakeThunk(ast::Expression const *expr,
                                   type::Type const *type) {
  ir::CompiledFn fn(type::Func({}, {ASSERT_NOT_NULL(type)}),
                    core::FnParams<type::Typed<ast::Declaration const *>>{});
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
    for (size_t i = 0; i < vals.size(); ++i) {
      ir::SetRet(i, type::Typed{vals.GetResult(i), extracted_types.at(i)});
    }
    builder().ReturnJump();

    CompleteDeferredBodies();
  }
  return fn;
}

}  // namespace compiler
