#include "module.h"

#include <list>

#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
#include "backend/emit.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "frontend/source.h"
#include "import_graph.h"
#include "ir/func.h"
#include "type/function.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/ir/LLVMContext.h"
#include "llvm/ir/Module.h"

namespace backend {
std::string WriteObjectFile(std::string const &name, Module *mod);
}  // namespace backend

#endif  // ICARUS_USE_LLVM

static std::mutex mtx;
static ImportGraph import_graph;
static std::list<std::shared_future<Module const*>> pending_module_futures;
static base::unordered_map<
    std::filesystem::path const *,
    std::pair<std::shared_future<Module const *> *, Module>>
    modules;

std::atomic<bool> found_errors = false;
ir::Func *main_fn;

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::Func which we want to forward declare.
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

ir::Func *Module::AddFunc(
    type::Function const *fn_type,
    base::vector<std::pair<std::string, ast::Expression *>> args) {
  auto *result = fns_.emplace_back(std::make_unique<ir::Func>(this, fn_type,
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

type::Type const *Module::GetType(std::string const &name) const {
  ASSIGN_OR(return nullptr, auto &decl, GetDecl(name));
  return types_.at(ast::BoundConstants{}).at(&decl);
}

ast::Declaration *Module::GetDecl(std::string const &name) const {
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

void Module::CompilationWorkItem::Complete() {
  // Need to copy bc because this needs to be set before we call CompleteBody.
  // TODO perhaps on ctx it could be a pointer?
  if (mod_->completed_[bound_constants_].emplace(expr_).second) {
    Context ctx(mod_);
    ctx.bound_constants_ = bound_constants_;
    if (expr_->is<ast::FunctionLiteral>()) {
      expr_->as<ast::FunctionLiteral>().CompleteBody(&ctx);
    } else if (expr_->is<ast::StructLiteral>()) {
      expr_->as<ast::StructLiteral>().CompleteBody(&ctx);
    } else {
      UNREACHABLE(expr_);
    }
  }
}

void Module::CompleteAll() {
  while (!to_complete_.empty()) {
    auto work = std::move(to_complete_.front());
    to_complete_.pop();
    work.Complete();
  }
}

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
static Module const *CompileModule(Module *mod) {
  ast::BoundConstants bc;
  Context ctx(mod);
  frontend::File f(ASSERT_NOT_NULL(mod->path_)->string());
  auto file_stmts = f.Parse(&ctx);
  if (ctx.num_errors() > 0) {
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  file_stmts->assign_scope(ctx.mod_->global_.get());
  file_stmts->VerifyType(&ctx);
  if (ctx.num_errors() != 0) {
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

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
  ctx.mod_->CompleteAll();

  for (auto &fn : ctx.mod_->fns_) { fn->ComputeInvariants(); }
  for (auto &fn : ctx.mod_->fns_) { fn->CheckInvariants(); }

#ifdef ICARUS_USE_LLVM
  backend::EmitAll(ctx.mod_->fns_, ctx.mod_->llvm_.get());
#endif  // ICARUS_USE_LLVM

  for (auto const &stmt : ctx.mod_->statements_.content_) {
    if (!stmt->is<ast::Declaration>()) { continue; }
    auto &decl = stmt->as<ast::Declaration>();
    if (decl.id_ != "main") { continue; }
    auto f = backend::EvaluateAs<ir::AnyFunc>(decl.init_val.get(), &ctx);
    ASSERT(f.is_fn());
    auto ir_fn = f.func();

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

type::Type const *Module::type_of(ast::BoundConstants const &bc,
                                  ast::Expression const *expr) const {
  auto bc_iter = types_.find(bc);
  if (bc_iter != types_.end()) {
    auto iter = bc_iter->second.data_.find(expr);
    if (iter != bc_iter->second.data_.end()) { return iter->second; }
  }

  return nullptr;
}

ir::Register Module::addr(ast::BoundConstants const &bc,
                          ast::Declaration *decl) const {
  return addr_.at(bc).at(decl);
}

type::Type const *Module::set_type(ast::BoundConstants const &bc,
                                   ast::Expression const *expr,
                                   type::Type const *t) {
  types_[bc].emplace(expr, t);
  return t;
}

PendingModule Module::Schedule(std::filesystem::path const &src,
                               std::filesystem::path const &requestor) {
  std::lock_guard lock(mtx);
  auto[src_ptr, newly_inserted] = import_graph.node(src);
  ASSERT(src_ptr != nullptr);

  // Need to add dependencies even if the node was already scheduled (hence the
  // "already scheduled" check is done after this).
  if (requestor != std::filesystem::path{""}) {
    ASSERT(import_graph.AddDependency(
        src_ptr, ASSERT_NOT_NULL(import_graph.node(requestor).first)));
  }

  if (!newly_inserted) {
    return PendingModule{ASSERT_NOT_NULL(modules[src_ptr].first)};
  }

  auto & [ fut, mod ] = modules[src_ptr];
  ASSERT(fut == nullptr);
  mod.path_ = src_ptr;
  fut       = &pending_module_futures.emplace_back(
      std::async(std::launch::async, CompileModule, &mod));
  return PendingModule{fut};
}

Module const *PendingModule::get() {
  if ((data_ & 1) == 0) { return reinterpret_cast<Module const *>(data_); }
  Module const *result =
      reinterpret_cast<std::shared_future<Module const *> *>(data_ - 1)->get();
  *this = PendingModule{result};
  return result;
}

void AwaitAllModulesTransitively() {
  decltype(pending_module_futures)::iterator iter, end_iter;
  {
    std::lock_guard lock(mtx);
    iter     = pending_module_futures.begin();
    end_iter = pending_module_futures.end();
  }

  while (iter != end_iter) {
    iter->wait();
    std::lock_guard lock(mtx);
    ++iter;
  }
}
