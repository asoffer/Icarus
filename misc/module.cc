#include "misc/module.h"

#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
// #include "backend/emit.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "frontend/source.h"
#include "ir/compiled_fn.h"
#include "type/function.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/ir/LLVMContext.h"
#include "llvm/ir/Module.h"

namespace backend {
std::string WriteObjectFile(std::string const &name, Module *mod);
}  // namespace backend

#endif  // ICARUS_USE_LLVM

namespace frontend {
std::unique_ptr<ast::Statements> Parse(Src *src, ::Module *mod);
}  // namespace frontend

std::atomic<bool> found_errors = false;
ir::CompiledFn *main_fn = nullptr;

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
Module::Module()
    : scope_(this)
#ifndef ICARUS_USE_LLVM
#else
          llvm_ctx_(std::make_unique<llvm::LLVMContext>()),
      llvm_(std::make_unique<llvm::Module>("my module", *llvm_ctx_))
#endif  // ICARUS_USE_LLVM
{
  dep_data_.emplace_back();
}

Module::~Module() = default;

ir::CompiledFn *Module::AddFunc(
    type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  auto *result = fns_.emplace_back(std::make_unique<ir::CompiledFn>(
                                       this, fn_type, std::move(params)))
                     .get();

#ifdef ICARUS_USE_LLVM
  result->llvm_fn_ =
      llvm::Function::Create(fn_type->llvm_fn(*llvm_ctx_),
                             llvm::Function::ExternalLinkage, "", llvm_.get());
  result->llvm_fn_->setName(fns_.back()->name());
#endif  // ICARUS_USE_LLVM

  return result;
}

type::Type const *Module::GetType(std::string_view name) const {
  ASSIGN_OR(return nullptr, auto &decl, GetDecl(name));
  return dep_data_.front().second.verify_results_.at(&decl).type_;
}

ast::Declaration *Module::GetDecl(std::string_view name) const {
  for (auto const &stmt : statements_.content_) {
    ASSIGN_OR(continue, auto &decl, stmt->if_as<ast::Declaration>());
    if (decl.id_ != name) { continue; }
    auto &hashtags = decl.hashtags_;
    bool exported =
        std::any_of(hashtags.begin(), hashtags.end(), [](ast::Hashtag h) {
          return h.kind_ == ast::Hashtag::Builtin::Export;
        });
    if (!exported) { continue; }
    return &decl;
  }
  return nullptr;
}

void Module::CompleteAllDeferredWork() {
  while (!deferred_work_.empty()) {
    auto &work = deferred_work_.front();
    if (work == nullptr) { continue; }
    work();
    deferred_work_.pop();
  }
}

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
Module *CompileModule(Module *mod, std::filesystem::path const *path) {
  mod->path_ = ASSERT_NOT_NULL(path);
  // TODO log an error if this fails.
  ASSIGN_OR(return nullptr, frontend::FileSrc src,
                   frontend::FileSrc::Make(*mod->path_));

  auto file_stmts = frontend::Parse(&src, mod);
  if (mod->error_log_.size() > 0) {
    mod->error_log_.Dump();
    found_errors = true;
    return mod;
  }

  {
    ast_visitor::AssignScope visitor;
    file_stmts->assign_scope(&visitor, &mod->scope_);
  }

  Context ctx(mod);
  {
    ast_visitor::VerifyType visitor;
    file_stmts->VerifyType(&visitor, &ctx);
  }
  mod->CompleteAllDeferredWork();

  if (ctx.num_errors() > 0) {
    // TODO Is this right?
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  {
    ast_visitor::EmitIr visitor;
    file_stmts->EmitIr(&visitor, &ctx);
  }
  mod->CompleteAllDeferredWork();

  if (ctx.num_errors() > 0) {
    // TODO Is this right?
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  ctx.mod_->statements_ = std::move(*file_stmts);

  for (auto &fn : ctx.mod_->fns_) { fn->ComputeInvariants(); }
  for (auto &fn : ctx.mod_->fns_) { fn->CheckInvariants(); }

#ifdef ICARUS_USE_LLVM
  backend::EmitAll(ctx.mod_->fns_, ctx.mod_->llvm_.get());
#endif  // ICARUS_USE_LLVM

  for (auto const &stmt : ctx.mod_->statements_.content_) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      if (decl->id_ != "main") { continue; }
      auto f = backend::EvaluateAs<ir::AnyFunc>(decl->init_val.get(), &ctx);
      ASSERT(f.is_fn() == true);
      auto ir_fn = f.func();

      // TODO check more than one?

#ifdef ICARUS_USE_LLVM
      ir_fn->llvm_fn_->setName("main");
      ir_fn->llvm_fn_->setLinkage(llvm::GlobalValue::ExternalLinkage);
#else
      // TODO need to be holding a lock when you do this.
      main_fn = ir_fn;
#endif  // ICARUS_USE_LLVM
    } else {
      continue;
    }
  }

#ifdef ICARUS_USE_LLVM
  if (std::string err = backend::WriteObjectFile(
          src.substr(0, src.size() - 2) + "o", ctx.mod_);
      err != "") {
    std::cerr << err;
  }
#endif  // ICARUS_USE_LLVM

  return mod;
}
