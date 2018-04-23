#include <cstring>
#include <future>
#include <vector>
#include <execinfo.h>

#include "ast/ast.h"
#include "ast/statements.h"
#include "backend/emit.h"
#include "base/debug.h"
#include "base/guarded.h"
#include "base/source.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetSelect.h"
#include "module.h"

// Debug flags and their default values
namespace debug {
inline bool parser        = false;
inline bool ct_eval       = false;
inline bool no_validation = false;
} // namespace debug

const char *output_file_name = "a.out";
std::vector<Source::Name> files;

static void
ShowUsage(char *argv0) {
  fprintf(stderr,
          R"(Usage: %s [options] input_file... -o output_file

  -o output_file                 Default is a.out

  -e, --eval                     Run compile-time evaluator step-by-step (debug).

  --file-type=[ir|nat|bin|none]  Output a file of the specified type:
                                   ir   - LLVM intermediate representation
                                   nat  - Output a native object file
                                   bin  - Output a single native object file and
                                          link it (requires gcc)
                                   none - Do not write any files (debug)
                                          This is the default option.

  -h, --help                     Display this usage message.

  -n, --no-validation            Do not run property validation

  -p, --parser                   Display step-by-step file parsing (debug).

  -r, --repl                     Invoke Icarus Read-Eval-Print-Loop

)",
          argv0);
}

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

extern void ReplEval(AST::Expression *expr);

namespace backend {
std::string WriteObjectFile(const std::string &name, Module *mod);
}  // namespace backend

base::guarded<std::unordered_map<Source::Name,
                                 std::shared_future<std::unique_ptr<Module>>>>
    modules;

// TODO deprecate source_map
std::unordered_map<Source::Name, File *> source_map;
std::unique_ptr<Module> CompileModule(const Source::Name &src) {
  auto mod = std::make_unique<Module>();
  Context ctx(mod.get());
  auto *f = new File(src);
  source_map[src] = f;
  error::Log log;
  auto file_stmts = f->Parse(&log);
  if (log.size() > 0) {
    log.Dump();
    return mod;
  }

  file_stmts->assign_scope(ctx.mod_->global_.get());
  file_stmts->VerifyType(&ctx);
  file_stmts->Validate(&ctx);
  if (ctx.num_errors() != 0) {
    ctx.DumpErrors();
    return mod;
  }

  file_stmts->EmitIR(&ctx);
  if (ctx.num_errors() != 0) {
    ctx.DumpErrors();
    return mod;
  }

  ctx.mod_->statements_ = std::move(*file_stmts);
  ctx.mod_->Complete();
  backend::EmitAll(ctx.mod_->fns_, ctx.mod_->llvm_.get());

  for (const auto &stmt : ctx.mod_->statements_.content_) {
    if (!stmt->is<AST::Declaration>()) { continue; }
    auto &decl = stmt->as<AST::Declaration>();
    if (decl.identifier->token != "main") { continue; }
    auto val = Evaluate(decl.init_val.get(), &ctx);
    ASSERT(val.size() == 1u);
    auto fn_lit = std::get<AST::FunctionLiteral *>(val[0].value);
    // TODO check more than one?

    fn_lit->ir_func_->llvm_fn_->setName("main");
    fn_lit->ir_func_->llvm_fn_->setLinkage(llvm::GlobalValue::ExternalLinkage);
  }

  if (std::string err = backend::WriteObjectFile(
          src.substr(0, src.size() - 2) + "o", ctx.mod_);
      err != "") {
    std::cerr << err;
  }

  return mod;
}

void ScheduleModule(const Source::Name &src) {
  auto handle = modules.lock();
  auto iter = handle->find(src);
  if (iter != handle->end()) { return; }
  handle->emplace(src, std::shared_future<std::unique_ptr<Module>>(
                           std::async(std::launch::async, CompileModule, src)));
}

int GenerateCode() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  for (const auto &src : files) { ScheduleModule(src); }

  size_t current_size = 0;
  do {
    std::vector<std::shared_future<std::unique_ptr<Module>> *> future_ptrs;
    {
      auto handle  = modules.lock();
      current_size = handle->size();
      for (auto & [ src, module ] : *handle) { future_ptrs.push_back(&module); }
    }
    for (auto *future : future_ptrs) { future->wait(); }
  } while (current_size != modules.lock()->size());

  return 0;
}


int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  Repl repl;
  auto mod = std::make_unique<Module>();
  Context ctx(mod.get());
repl_start : {
  auto stmts = repl.Parse(&ctx.error_log_);
  if (ctx.num_errors() > 0) {
    ctx.DumpErrors();
    goto repl_start;
  }

  for (auto &stmt : stmts->content_) {
    if (stmt->is<AST::Declaration>()) {
      auto *decl = &stmt->as<AST::Declaration>();
      decl->assign_scope(ctx.mod_->global_.get());
      decl->VerifyType(&ctx);
      decl->Validate(&ctx);
      decl->EmitIR(&ctx);
      if (ctx.num_errors() != 0) {
        ctx.DumpErrors();
        goto repl_start;
      }

    } else if (stmt->is<AST::Expression>()) {
      auto *expr = &stmt->as<AST::Expression>();
      expr->assign_scope(ctx.mod_->global_.get());
      ReplEval(expr);
      fprintf(stderr, "\n");
    } else {
      NOT_YET(*stmt);
    }
  }
  goto repl_start;
}
}

int main(int argc, char *argv[]) {
#ifdef DBG
  signal(SIGABRT, +[](int) {
    constexpr u32 max_frames = 20;
    fprintf(stderr, "stack trace:\n");
    void *addrlist[max_frames + 1];
    int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void *));
    if (addrlen == 0) {
      fprintf(stderr, "  \n");
    } else {
      char **symbollist = backtrace_symbols(addrlist, addrlen);
      for (int i = 4; i < addrlen; i++) {
        std::string symbol = symbollist[i];
        auto start_iter    = symbol.find('(');
        auto end_iter      = symbol.find(')');
        std::string mangled =
            symbol.substr(start_iter + 1, end_iter - start_iter - 1);
        end_iter = mangled.find('+');
        mangled =
            end_iter >= mangled.size() ? mangled : mangled.substr(0, end_iter);
        char demangled[1024];
        size_t demangled_size = 1024;

        int status;
        abi::__cxa_demangle(mangled.c_str(), &demangled[0], &demangled_size,
                            &status);
        if (status != 0) {
          fprintf(stderr, "#%2d| %s\n", i - 3, symbol.c_str());
        } else {
          if (demangled_size > 70) {
            auto s = std::string(&demangled[0], 70) + " ...";
            fprintf(stderr, "#%2d| %s\n", i - 3, s.c_str());
          } else {
            fprintf(stderr, "#%2d| %s\n", i - 3, demangled);
          }
        }
      }
      free(symbollist);
    }
  });
#endif

  bool repl = false;
  for (int arg_num = 1; arg_num < argc; ++arg_num) {
    auto arg = argv[arg_num];

    if (strcmp(arg, "-o") == 0) {
      if (++arg_num == argc) {
        ShowUsage(argv[0]);
        return -1;
      }
      arg = argv[arg_num];
      if (arg[0] == '-') {
        ShowUsage(argv[0]);
        return -1;
      }
      output_file_name = arg;
      goto next_arg;
    }

    if (arg[0] == '-') {
      if (arg[1] == '-') {
        /* Long-form argument */
        char *ptr = arg + 2;
        while (*ptr != '=' && *ptr != '\0') { ++ptr; }
        if (*ptr == '=') {
          /* Long-form with value */

          *ptr = '\0';
          ptr++;  // points to the argument
        } else {
          /* Long-form flag */
          if (strcmp(arg + 2, "eval") == 0) {
            debug::ct_eval = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "help") == 0) {
            ShowUsage(argv[0]);
            return 0;

          } else if (strcmp(arg + 2, "no-validation") == 0) {
            debug::no_validation = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "parser") == 0) {
            debug::parser = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "repl") == 0) {
            repl = true;
            goto next_arg;

          } else {
            ShowUsage(argv[0]);
            return -1;
          }
        }

      } else {
        /* Short-form arguments */
        for (auto ptr = arg + 1; ptr; ++ptr) {
          switch (*ptr) {
            case 'h': ShowUsage(argv[0]); return 0;
            case 'e': debug::ct_eval = true; break;
            case 'n': debug::no_validation = true; break;
            case 'p': debug::parser = true; break;
            case 'r': repl = true; break;
            case '\0': goto next_arg;
            default: ShowUsage(argv[0]); return -1;
          }
        }
      }
    } else {
      files.emplace_back(arg);
    }
  next_arg:;
  }

  if (files.empty() && !repl) {
    ShowUsage(argv[0]);
    return -1;
  }

  return repl ? RunRepl() : GenerateCode();
}
