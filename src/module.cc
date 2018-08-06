#include "module.h"

#include <future>

#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "backend/emit.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "base/source.h"
#include "ir/func.h"
#include "type/function.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace backend {
std::string WriteObjectFile(const std::string& name, Module* mod);
}  // namespace backend

#endif  // ICARUS_USE_LLVM

std::atomic<bool> found_errors = false;
IR::Func* main_fn;

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of IR::Func which we want to forward declare.
Module::Module()
    : global_(std::make_unique<DeclScope>(nullptr))
#ifdef ICARUS_USE_LLVM
      ,
      llvm_ctx_(std::make_unique<llvm::LLVMContext>()),
      llvm_(std::make_unique<llvm::Module>("my module", *llvm_ctx_))
#endif  // ICARUS_USE_LLVM
{
  global_->module_ = this;
}
Module::~Module() = default;

IR::Func* Module::AddFunc(
    AST::GeneratedFunction* fn_lit,
    base::vector<std::pair<std::string, AST::Expression*>> args) {
  auto* result = fns_.emplace_back(std::make_unique<IR::Func>(this, fn_lit,
                                                              std::move(args)))
                     .get();

#ifdef ICARUS_USE_LLVM
  result->llvm_fn_ = llvm::Function::Create(
      fn_lit->type->as<type::Function>().llvm_fn(*llvm_ctx_),
      llvm::Function::ExternalLinkage, "", llvm_.get());
  result->llvm_fn_->setName(fns_.back()->name());
#endif  // ICARUS_USE_LLVM

  return result;
}

IR::Func* Module::AddFunc(
    const type::Function* fn_type,
    base::vector<std::pair<std::string, AST::Expression*>> args) {
  auto* result = fns_.emplace_back(std::make_unique<IR::Func>(this, fn_type,
                                                              std::move(args)))
                     .get();

#ifdef ICARUS_USE_LLVM
  result->llvm_fn_ =
      llvm::Function::Create(fn_type->llvm_fn(*llvm_ctx_),
                             llvm::Function::ExternalLinkage, "", llvm_.get());
  result->llvm_fn_->setName(fns_.back()->name());
#endif  // ICARUS_USE_LLVM

  return result;
}

const type::Type* Module::GetType(const std::string& name) const {
  auto* decl = GetDecl(name);
  if (decl == nullptr) { return nullptr; }
  return decl->type;
}

AST::Declaration* Module::GetDecl(const std::string& name) const {
  for (const auto& stmt : statements_.content_) {
    if (!stmt->is<AST::Declaration>()) { continue; }
    const auto& id = stmt->as<AST::Declaration>().identifier->token;
    if (id != name) { continue; }
    return &stmt->as<AST::Declaration>();
  }
  return nullptr;
}

void Module::Complete() {
  while (!to_complete_.empty()) {
    auto* fn_lit = to_complete_.front();
    fn_lit->CompleteBody(this);
    to_complete_.pop();
  }
}

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
std::unique_ptr<Module> Module::Compile(const Source::Name& src) {
  auto mod = std::make_unique<Module>();
  AST::BoundConstants bc;
  Context ctx(mod.get());
  ctx.bound_constants_ = &bc;
  File f(src);
  auto file_stmts = f.Parse(&ctx);
  if (ctx.num_errors() > 0) {
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  file_stmts->assign_scope(ctx.mod_->global_.get());
  file_stmts->VerifyType(&ctx);
  file_stmts->Validate(&ctx);
  if (ctx.num_errors() != 0) {
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  file_stmts->EmitIR(&ctx);
  if (ctx.num_errors() != 0) {
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  ctx.mod_->statements_ = std::move(*file_stmts);
  ctx.mod_->Complete();

  for (auto& fn : ctx.mod_->fns_) { fn->ComputeInvariants(); }
  for (auto& fn : ctx.mod_->fns_) { fn->CheckInvariants(); }

#ifdef ICARUS_USE_LLVM
  backend::EmitAll(ctx.mod_->fns_, ctx.mod_->llvm_.get());
#endif  // ICARUS_USE_LLVM

  for (const auto &stmt : ctx.mod_->statements_.content_) {
    if (!stmt->is<AST::Declaration>()) { continue; }
    auto &decl = stmt->as<AST::Declaration>();
    if (decl.identifier->token != "main") { continue; }
    auto ir_fn = backend::EvaluateAs<IR::Func *>(decl.init_val.get(), &ctx);

    // TODO check more than one?

#ifdef ICARUS_USE_LLVM
    ir_fn->llvm_fn_->setName("main");
    ir_fn->llvm_fn_->setLinkage(llvm::GlobalValue::ExternalLinkage);
#else
    main_fn  = ir_fn;
#endif  // ICARUS_USE_LLVM
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
