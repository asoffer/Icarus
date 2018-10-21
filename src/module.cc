#include "module.h"

#include <future>

#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "backend/emit.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "frontend/source.h"
#include "ir/func.h"
#include "type/function.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace backend {
std::string WriteObjectFile(const std::string &name, Module *mod);
}  // namespace backend

#endif  // ICARUS_USE_LLVM

std::atomic<bool> found_errors = false;
IR::Func *main_fn;

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

IR::Func *Module::AddFunc(
    AST::FunctionLiteral *fn_lit, type::Function const *fn_type,
    base::vector<std::pair<std::string, AST::Expression *>> args) {
  auto *result = fns_.emplace_back(std::make_unique<IR::Func>(
                                       this, fn_lit, fn_type, std::move(args)))
                     .get();

#ifdef ICARUS_USE_LLVM
  result->llvm_fn_ = llvm::Function::Create(
      types_.at(fn_lit)->as<type::Function>().llvm_fn(*llvm_ctx_),
      llvm::Function::ExternalLinkage, "", llvm_.get());
  result->llvm_fn_->setName(fns_.back()->name());
#endif  // ICARUS_USE_LLVM

  return result;
}

IR::Func *Module::AddFunc(
    const type::Function *fn_type,
    base::vector<std::pair<std::string, AST::Expression *>> args) {
  auto *result = fns_.emplace_back(std::make_unique<IR::Func>(this, fn_type,
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

const type::Type *Module::GetType(const std::string &name) const {
  auto *decl = GetDecl(name);
  if (decl == nullptr) { return nullptr; }
  return types_.at(AST::BoundConstants{}).at(decl);
}

AST::Declaration *Module::GetDecl(const std::string &name) const {
  for (const auto &stmt : statements_.content_) {
    if (!stmt->is<AST::Declaration>()) { continue; }
    if (stmt->as<AST::Declaration>().id_ != name) { continue; }
    return &stmt->as<AST::Declaration>();
  }
  return nullptr;
}

void Module::Complete() {
  while (!to_complete_.empty()) {
    auto[bc, fn_lit] = to_complete_.front();
    // Need to copy bc because this needs to be set before we call CompleteBody.
    // TODO perhaps on ctx it could be a pointer?
    if (completed_[bc].emplace(fn_lit).second) {
      Context ctx(this);
      ctx.bound_constants_ = std::move(bc);
      fn_lit->CompleteBody(&ctx);
    }
    to_complete_.pop();
  }
}

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
std::unique_ptr<Module> Module::Compile(const frontend::Source::Name &src) {
  auto mod = std::make_unique<Module>();
  AST::BoundConstants bc;
  Context ctx(mod.get());
  frontend::File f(src);
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

  for (auto &fn : ctx.mod_->fns_) { fn->ComputeInvariants(); }
  for (auto &fn : ctx.mod_->fns_) { fn->CheckInvariants(); }

#ifdef ICARUS_USE_LLVM
  backend::EmitAll(ctx.mod_->fns_, ctx.mod_->llvm_.get());
#endif  // ICARUS_USE_LLVM

  for (const auto &stmt : ctx.mod_->statements_.content_) {
    if (!stmt->is<AST::Declaration>()) { continue; }
    auto &decl = stmt->as<AST::Declaration>();
    if (decl.id_ != "main") { continue; }
    auto f = backend::EvaluateAs<IR::AnyFunc>(decl.init_val.get(), &ctx);
    ASSERT(f.is_fn_);
    auto ir_fn = f.fn_;

    // TODO check more than one?

#ifdef ICARUS_USE_LLVM
    ir_fn->llvm_fn_->setName("main");
    ir_fn->llvm_fn_->setLinkage(llvm::GlobalValue::ExternalLinkage);
#else
    main_fn = ir_fn;
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

type::Type const *Module::type_of(AST::BoundConstants const &bc,
                                  AST::Expression const *expr) const {
  auto bc_iter = types_.find(bc);
  if (bc_iter != types_.end()) {
    auto iter = bc_iter->second.data_.find(expr);
    if (iter != bc_iter->second.data_.end()) { return iter->second; }
  }

  // TODO figure out why this is necessary. It shouldn't be because you should
  // be able to find the expression in this module (even if the declaration is
  // in another.
  for (Module const *mod : embedded_modules_) {
    bc_iter = mod->types_.find(bc);
    if (bc_iter == mod->types_.end()) { continue; }
    auto iter = bc_iter->second.data_.find(expr);
    if (iter != bc_iter->second.data_.end()) { return iter->second; }
  }
  return nullptr;
}

IR::Register Module::addr(AST::BoundConstants const &bc,
                          AST::Declaration *decl) const {
  return addr_.at(bc).at(decl);
}

void Module::set_type(AST::BoundConstants const &bc,
                      AST::Expression const *expr, type::Type const *t) {
  types_[bc].emplace(expr, t);
}
