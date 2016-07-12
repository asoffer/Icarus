#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

namespace cstdlib {
extern llvm::Constant *memcpy();
} // namespace cstdlib

namespace data {
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::Constant *null_pointer(Type *t);
} // namespace data

BlockScope *Scope::Global = nullptr; // Initialized in main
std::vector<AST::Declaration *> Scope::decl_registry_ = {};

static size_t scope_num_counter = 0;
Scope::Scope()
    : parent(Scope::Global), containing_function_(nullptr),
      name("anon" + std::to_string(scope_num_counter++)) {}

AST::Declaration *Scope::DeclHereOrNull(const std::string &name,
                                        Type *declared_type) {
  for (auto decl : DeclRegistry) {
    if (decl->type == declared_type && decl->identifier->token == name) {
      return decl;
    }
  }
  return nullptr;
}

AST::Declaration *Scope::DeclReferencedOrNull(const std::string &name, Type *declared_type) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto ptr = scope_ptr->DeclHereOrNull(name, declared_type);
    if (ptr) { return ptr; }
  }
  return nullptr;
}

AST::Identifier *Scope::IdentifierHereOrNull(const std::string &name) {
  for (auto decl : DeclRegistry) {
    if (decl->identifier->token == name) { return decl->identifier; }
  }

  return nullptr;
}

AST::Identifier *Scope::IdentifierBeingReferencedOrNull(const std::string &name) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto ptr  = scope_ptr->IdentifierHereOrNull(name);
    if (ptr) { return ptr; }
  }

  return nullptr;
}

// Set pointer to the parent scope. This is an independent concept from LLVM's
// "parent". For us, functions can be declared in local scopes, so we will
// likely need this structure.
void Scope::set_parent(Scope* new_parent) {
  assert(new_parent && "Setting scope's parent to be a nullptr.");
  assert(new_parent != this && "Setting parent as self");

  if (parent && parent->containing_function_) {
    parent->containing_function_->remove_scope(this);
  }

  parent = new_parent;

  containing_function_ = new_parent->is_function_scope()
                             ? (FnScope *)parent
                             : parent->containing_function_;

  if (containing_function_) { containing_function_->add_scope(this); }
}

BlockScope::BlockScope(ScopeType st)
    : type(st), entry_block(nullptr), exit_block(nullptr), land_block(nullptr) {
}

void Scope::verify_no_shadowing() {
  for (auto decl_ptr1 : decl_registry_) {
    for (auto decl_ptr2 : decl_registry_) {
      if (decl_ptr1 == decl_ptr2) continue;
      if (decl_ptr1->identifier->token != decl_ptr2->identifier->token)
        continue;

      auto scope_ptr = decl_ptr1->scope_;
      // If the shadowing occurs in the same scope, we don't need to display
      // the error message twice.
      if (scope_ptr == decl_ptr2->scope_) {
        if ((!decl_ptr1->type->is_function() ||
             !decl_ptr2->type->is_function()) &&
            decl_ptr1->loc.line_num <= decl_ptr2->loc.line_num) {
          Error::Log::Log(decl_ptr1->loc,
                        "Identifier `" + decl_ptr1->identifier->token +
                            "` already declared in this scope (on line " +
                            std::to_string(decl_ptr2->loc.line_num) + ").");
        }

        continue;
      }

      // TODO is_block_scope is a standin for not being a struct/enum scope
      while (scope_ptr != nullptr && scope_ptr->is_block_scope()) {
        if (scope_ptr == decl_ptr2->scope_) {
          Error::Log::Log(decl_ptr1->loc,
                        "Identifier `" + decl_ptr1->identifier->token +
                            "` shadows identifier declared on line " +
                            std::to_string(decl_ptr2->loc.line_num) + ".");
          // Do NOT skip out here. It's possible to have many shadows and we
          // might as well catch them all.
        }
        scope_ptr = scope_ptr->parent;
      }
    }
  }
}

void BlockScope::defer_uninit(Type *type, llvm::Value *val) {
  deferred_uninits.emplace(type, val);
}

void BlockScope::MakeReturn(Type *ret_type, IR::Value val) {
  FnScope *fn_scope =
      is_function_scope() ? (FnScope *)this : containing_function_;

  // TODO actual returned type (second type arg) may be different
  Type::CallAssignment(this, ret_type, ret_type, val, fn_scope->ret_val);

  assert(fn_scope->exit_block);
  IR::Block::Current->exit.SetUnconditional(fn_scope->exit_block);
}

// TODO what should the exit_flag default be?
FnScope::FnScope()
    : BlockScope(ScopeType::Function), fn_type(nullptr), return_value(nullptr),
      exit_flag_(nullptr), exit_flag('\0') {}

void FnScope::add_scope(Scope *scope) { innards_.insert(scope); }
void FnScope::remove_scope(Scope *scope) { innards_.erase(scope); }

/* TODO delete this once it's copied into IR correctly
llvm::Value *FnScope::ExitFlag() {
  if (exit_flag_) { return exit_flag_; }
  auto ip = builder.saveIP();
  if (entry->empty()) {
    builder.SetInsertPoint(entry);
  } else {
    builder.SetInsertPoint(entry->begin());
  }
  exit_flag_ = builder.CreateAlloca(*Char, nullptr, "exit.flag");
  builder.restoreIP(ip);
  return exit_flag_;
}*/

bool Scope::is_loop_scope() {
  if (!is_block_scope()) return false;
  auto bs = (BlockScope *)this;
  return bs->type == ScopeType::For || bs->type == ScopeType::While;
}
