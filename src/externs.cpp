#ifndef ICARUS_UNITY
#include "Type/Type.h"
#include "IR/IR.h"
#include "Scope.h"
#endif

llvm::Module *global_module         = nullptr;
llvm::TargetMachine *target_machine = nullptr;
llvm::IRBuilder<> builder(llvm::getGlobalContext());
Timer timer;

namespace Language {
// Associativity stored in the lowest two bits.
size_t precedence(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Operator::name: return (((prec) << 2) + (assoc));
#include "config/operator.conf"
#undef OPERATOR_MACRO
  }
}
} // namespace Language

// Debug flags and their default values
namespace debug {
bool parser  = false;
bool timer   = false;
bool ct_eval = false;
} // namespace debug

namespace Hashtag {
static std::vector<const char *> table = {"const"};

size_t GetOrFailValue(const std::string &tag) {
  for (size_t i = 0; i < table.size(); ++i) {
    if (tag == table[i]) { return i; }
  }

  return FAIL;
}

size_t Get(const std::string &tag) {
  for (size_t i = 0; i < table.size(); ++i) {
    if (tag == table[i]) { return i; }
  }

  size_t result = table.size();
  char *new_tag = new char[tag.size() + 1];
  strcpy(new_tag, tag.c_str());
  table.push_back(new_tag);

  assert(table[result] == new_tag);

  return result;
}
} // namespace Hashtag

namespace cstdlib {
llvm::Constant *malloc() {
  static llvm::Constant *func_ = global_module->getOrInsertFunction(
      "malloc", llvm::FunctionType::get(*Ptr(Char), {*Uint}, false));
  return func_;
}
} // namespace cstdlib

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
                                llvm::APInt(64, (unsigned int)n, false));
}

llvm::ConstantInt *const_uint16(uint16_t n) {
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(16, (size_t)n, false));
}

llvm::ConstantInt *const_uint32(uint32_t n) {
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(32, (size_t)n, false));
}

llvm::ConstantInt *const_uint(size_t n) {
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(64, (size_t)n, false));
}

llvm::ConstantFP *const_real(double d) {
  return llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(d));
}

llvm::ConstantInt *const_bool(bool b) {
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(1, b ? 1 : 0, false));
}

llvm::ConstantInt *const_char(char c) {
  // TODO check safety of char cast
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(8, (size_t)c, false));
}

// NOTE: It's important that you pass a std::string because the first char could be '\0'
llvm::Value *global_string(const std::string &s) {
  static std::map<std::string, llvm::Value *> global_strings;

  auto iter = global_strings.find(s);
  return iter == global_strings.end()
             ? global_strings[s] = builder.CreateGlobalStringPtr(s)
             : iter->second;
}
} // namespace data

IR::Value PtrCallFix(Type *t, IR::Value v) {
  return t->is_big() ? v : IR::Load(t, v);
}

Type *GetFunctionTypeReferencedIn(Scope *scope, const std::string &fn_name,
                                  Type *input_type) {
  for (auto scope_ptr = scope; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto id_ptr = scope_ptr->IdentifierHereOrNull(fn_name);
    if (!id_ptr) { continue; }

    if (!id_ptr->type) {
      // NOTE: IdentifierHereOrNull always returns the identifier bound to the
      // declaration, so if the type isn't specified, we need to actually verify
      // the type of it's declaration.
      assert(id_ptr->decl);
      id_ptr->decl->verify_types();
      assert(id_ptr->type);
    }

    if (id_ptr->type->is_function()) {
      auto fn_type = (Function *)id_ptr->type;
      if (fn_type->input == input_type) { return fn_type; }

    } else {
      UNREACHABLE;
    }
  }
  return nullptr;
}

IR::Value GetFuncReferencedIn(Scope *scope, const std::string &fn_name,
                              Function *fn_type) {
  Scope *scope_ptr = scope;
  AST::Declaration *decl;

  decl = scope->DeclReferencedOrNull(fn_name, fn_type);

  for (; scope_ptr; scope_ptr = scope_ptr->parent) {
    decl = scope_ptr->DeclHereOrNull(fn_name, fn_type);
    if (decl) { break; }
  }

  if (!decl) { return IR::Value::None(); }

  if(decl->addr == IR::Value::None()) {
    if (decl->init_val->is_function_literal()) {
      auto old_func = IR::Func::Current;
      auto old_block = IR::Block::Current;

      decl->addr = decl->init_val->EmitIR();
      decl->addr.as_func->SetName(
          Mangle(fn_type, decl->identifier, scope_ptr));

      IR::Func::Current  = old_func;
      IR::Block::Current = old_block;
    } else {
      NOT_YET;
    }
  }

  assert(decl->addr != IR::Value::None());
  return IR::Load(fn_type, decl->addr);
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
