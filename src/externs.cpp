#ifndef ICARUS_UNITY
#include "Type/Type.h"
#include "IR/IR.h"
#include "Scope.h"
#endif

// TODO 32 is hard-coded here as an int size. Change it

namespace Language {
// Associativity stored in the lowest two bits.
size_t precedence(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Operator::name:                                                         \
    return (((prec) << 2) + (assoc));
#include "config/operator.conf"
#undef OPERATOR_MACRO
  }
}
} // namespace Language

// Debug flags and their default values
namespace debug {
bool parser            = false;
bool timer             = false;
bool parametric_struct = false;
bool ct_eval           = false;
} // namespace debug

std::vector<AST::StructLiteral*> created_types;
llvm::IRBuilder<> builder(llvm::getGlobalContext());

enum class Lib {
  String
};

std::map<Lib, AST::Identifier *> lib_type;
std::queue<std::string> file_queue;

llvm::Module *global_module         = nullptr;
llvm::TargetMachine *target_machine = nullptr;

std::map<std::string, llvm::Value*> global_strings;

llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn) {
  return llvm::BasicBlock::Create(llvm::getGlobalContext(), name, fn);
}

#define CSTDLIB(fn, variadic, in, out)                                         \
  llvm::Constant *fn() {                                                       \
    static llvm::Constant *func_ = global_module->getOrInsertFunction(         \
        #fn, llvm::FunctionType::get(out, {in}, variadic));                    \
    return func_;                                                              \
  }

// TODO Reduce the dependency on the C standard library. This probably means
// writing platform-specific assembly.
namespace cstdlib {
CSTDLIB(free, false, *Ptr(Char), *Void);
CSTDLIB(malloc, false, *Uint, *Ptr(Char));

// CSTDLIB(memcpy,  false, Type::get_tuple({ Ptr(Char), Ptr(Char), Uint }),
// Ptr(Char));
CSTDLIB(putchar, false, *Char, *Int);
CSTDLIB(puts, false, *Ptr(Char), *Int);
CSTDLIB(printf, true, *Ptr(Char), *Int);
CSTDLIB(scanf, true, *Ptr(Char), *Int);
// TODO Even though it's the same, shouldn't it be RawPtr for return type rather
// than Ptr(Char)?

llvm::Constant *calloc() {
  static llvm::Constant *func_ = global_module->getOrInsertFunction(
      "calloc", llvm::FunctionType::get(*RawPtr, {*Uint, *Uint}, false));
  return func_;
}

llvm::Constant *memcpy() {
  static llvm::Constant *func_ = global_module->getOrInsertFunction(
      "memcpy",
      llvm::FunctionType::get(*Ptr(Char), {*RawPtr, *RawPtr, *Uint}, false));
  return func_;
}
} // namespace cstdlib
#undef CSTDLIB

namespace data {
llvm::Constant *null_pointer(Type *t) {
  return llvm::ConstantPointerNull::get(llvm::PointerType::get(*t, 0));
}

llvm::Constant *null(const Type *t) {
  assert(t->is_pointer() && "type must be a pointer to have a null value");
  return null_pointer(((const Pointer *)t)->pointee);
}

llvm::ConstantInt *const_int(long n) {
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(32, (unsigned int)n, false));
}

llvm::ConstantInt *const_uint(size_t n) {
  // The safety of this cast is verified only in debug mode
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(32, (size_t)n, false));
}

llvm::ConstantFP *const_real(double d) {
  return llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(d));
}

llvm::ConstantInt *const_false() {
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(1, 0, false));
  }

  llvm::ConstantInt *const_true() {
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(1, 1, false));
  }

  llvm::ConstantInt *const_bool(bool b) {
    return b ? const_true() : const_false();
  }

  llvm::ConstantInt* const_char(char c) {
    // TODO check safety of char cast
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                  llvm::APInt(8, (size_t)c, false));
  }

  llvm::Value *global_string(const std::string &s) {
    auto iter = global_strings.find(s);
    return iter == global_strings.end()
               ? global_strings[s] = builder.CreateGlobalStringPtr(s)
               : iter->second;
  }
}  // namespace data

namespace builtin {
llvm::Function *ord() {
  static llvm::Function *ord_ = nullptr;
  if (ord_ != nullptr) { return ord_; }

  ord_ = llvm::Function::Create(
      *Func(Char, Uint), llvm::Function::ExternalLinkage, "ord", global_module);

  llvm::Value *val = ord_->args().begin();
  llvm::IRBuilder<> bldr(llvm::getGlobalContext());

  auto entry_block = make_block("entry", ord_);

  bldr.SetInsertPoint(entry_block);
  // TODO check bounds if build option specified

  bldr.CreateRet(bldr.CreateZExt(val, *Uint));

  return ord_;
}

llvm::Function *ascii() {
  static llvm::Function *ascii_ = nullptr;
  if (ascii_ != nullptr) { return ascii_; }

  ascii_ =
      llvm::Function::Create(*Func(Uint, Char), llvm::Function::ExternalLinkage,
                             "ascii", global_module);

  llvm::Value *val = ascii_->args().begin();
  llvm::IRBuilder<> bldr(llvm::getGlobalContext());

  auto entry_block = make_block("entry", ascii_);
  bldr.SetInsertPoint(entry_block);
  // TODO check bounds if build option specified

  bldr.CreateRet(bldr.CreateTrunc(val, *Char));
  return ascii_;
}
} // namespace builtin

namespace IR {
Func *AsciiFunc() {
  static IR::Func *ascii_ = nullptr;

  if (ascii_) { return ascii_; }
  ascii_ = new IR::Func;

  auto saved_func  = Func::Current;
  auto saved_block = Block::Current;

  Func::Current  = ascii_;
  Block::Current = ascii_->entry();

  Block::Current->exit.SetReturn(IR::Cast(Uint, Char, IR::Value::Arg(0)));

  Func::Current  = saved_func;
  Block::Current = saved_block;

  return ascii_;
}

Func *OrdFunc() {
  static IR::Func *ord_ = nullptr;

  if (ord_) { return ord_; }
  ord_ = new IR::Func;

  auto saved_func  = Func::Current;
  auto saved_block = Block::Current;

  Func::Current  = ord_;
  Block::Current = ord_->entry();

  Block::Current->exit.SetReturn(IR::Cast(Char, Uint, IR::Value::Arg(0)));

  Func::Current  = saved_func;
  Block::Current = saved_block;

  return ord_;
}
} // namespace IR


// TODO make calls to call_repr not have to first check if we pass the
// object or a pointer to the object.
//
// This really ought to be inlined, but that's not possible keeping it externed
llvm::Value *PtrCallFix(Type *t, llvm::Value *ptr) {
  return (t->is_big()) ? ptr : builder.CreateLoad(ptr);
}

IR::Value PtrCallFix(Type *t, IR::Value v) {
  return t->is_big() ? v : IR::Load(t, v);
}

Type *GetFunctionTypeReferencedIn(Scope *scope, const std::string &fn_name,
                                  Type *input_type) {
  for (auto scope_ptr = scope; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto id_ptr = scope_ptr->IdentifierHereOrNull(fn_name);
    if (!id_ptr) { continue; }

    if (id_ptr->type->is_function()) {
      auto fn_type = (Function *)id_ptr->type;
      if (fn_type->input == input_type) { return fn_type; }

    } else {
      assert(false && "What else could it be?");
    }
  }
  return nullptr;
}

llvm::Value *GetFunctionReferencedIn(Scope *scope, const std::string &fn_name,
                                     Function *fn_type) {
  Scope *scope_ptr = scope;
  AST::Declaration *decl;

  decl = scope->DeclReferencedOrNull(fn_name, fn_type);

  for (; scope_ptr; scope_ptr = scope_ptr->parent) {
    decl = scope_ptr->DeclHereOrNull(fn_name, fn_type);
    if (decl) { break; }
  }

  if (!decl) { return nullptr; }

  auto mangled_name = Mangle(fn_type, decl->identifier, scope_ptr);
  llvm::FunctionType *llvm_fn_type = *fn_type;

  if(!decl->alloc) {
    decl->alloc =
        global_module->getOrInsertFunction(mangled_name, llvm_fn_type);
  }
  return decl->alloc;
}

IR::Func *GetFuncReferencedIn(Scope *scope, const std::string &fn_name,
                              Function *fn_type) {
  Scope *scope_ptr = scope;
  AST::Declaration *decl;

  decl = scope->DeclReferencedOrNull(fn_name, fn_type);

  for (; scope_ptr; scope_ptr = scope_ptr->parent) {
    decl = scope_ptr->DeclHereOrNull(fn_name, fn_type);
    if (decl) { break; }
  }

  if (!decl) { return nullptr; }

  // TODO change name of this stack_loc -> alloc or something similar.
  if(!decl->stack_loc.as_func) {
    if (decl->init_val->is_function_literal()) {
      auto old_func = IR::Func::Current;
      auto old_block = IR::Block::Current;

      decl->stack_loc = decl->init_val->EmitIR();
      decl->stack_loc.as_func->name =
          Mangle(fn_type, decl->identifier, scope_ptr);

      IR::Func::Current  = old_func;
      IR::Block::Current = old_block;
    } else {
      NOT_YET;
    }
  }
  return decl->stack_loc.as_func;
}

AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr) {
  if (expr->is_function_literal()) {
    return (AST::FunctionLiteral *)expr;

  } else if (expr->is_identifier()) {
    auto id = (AST::Identifier *)expr;
    assert(id->decl->IsInferred());
    return GetFunctionLiteral(id->decl->init_val);

  } else if (expr->is_declaration()) {
    auto decl = (AST::Declaration *)expr;
    assert(decl->IsInferred());
    return GetFunctionLiteral(decl->init_val);
  } else if (expr->is_binop()) {
    NOT_YET;
  } else {
    assert(false);
  }
}
