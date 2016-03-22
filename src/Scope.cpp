#include "Scope.h"

#include <iostream>
#include <algorithm>

#include "AST.h"
#include "ErrorLog.h"

extern llvm::DataLayout* data_layout;
extern ErrorLog error_log;

namespace cstdlib {
extern llvm::Constant *memcpy();
} // namespace cstdlib

namespace data {
extern llvm::Value *const_uint(size_t n);
} // namespace data

BlockScope *Scope::Global = nullptr; // Initialized in main
std::vector<AST::Declaration *> Scope::decl_registry_ = {};

std::stack<Scope *> Scope::Stack;

Context &CurrentContext() { return CurrentScope()->context; }
Scope *CurrentScope() {
  return Scope::Stack.empty() ? nullptr : Scope::Stack.top();
}

AST::Declaration *Scope::make_declaration(size_t line_num,
                                          AST::DeclType decl_type,
                                          const std::string &id_string) {
  auto decl = new AST::Declaration;
  decl_registry_.emplace_back(decl);
  decl->identifier = new AST::Identifier(line_num, id_string);
  decl->line_num   = line_num;
  decl->decl_type  = decl_type;

  return decl;
}

Scope::Scope() : parent(Scope::Global), containing_function_(nullptr) {}

AST::Identifier *Scope::identifier(AST::Expression *id_as_eptr) {
  auto idptr = static_cast<AST::Identifier *>(id_as_eptr);

  Scope *current_scope = this;
  while (current_scope != nullptr) {
    auto iter = current_scope->ids_.find(idptr->token());
    if (iter != current_scope->ids_.end()) { return iter->second; }
    current_scope = current_scope->parent;
  }

  // If you reach here it's because we never saw a declaration for the
  // identifier
  error_log.log(idptr->line_num,
                "Undeclared identifier `" + idptr->token() + "`.");

  return nullptr;
}

AST::Identifier *Scope::identifier(const std::string &name) const {
  auto iter = ids_.find(name);
  if (iter == ids_.end()) {
    if (parent) return parent->identifier(name);
    return nullptr;
  }
  return iter->second;
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
  context.set_parent(&new_parent->context);

  containing_function_ = new_parent->is_function_scope()
                             ? static_cast<FnScope *>(parent)
                             : parent->containing_function_;

  if (containing_function_) { containing_function_->add_scope(this); }
}

BlockScope::BlockScope(ScopeType st)
    : type(st), exit_flag(nullptr), entry(make_block("entry", nullptr)),
      exit(make_block("exit", nullptr)), land(nullptr) {}

void Scope::verify_no_shadowing() {
  for (auto decl_ptr1 : decl_registry_) {
    for (auto decl_ptr2 : decl_registry_) {
      if (decl_ptr1 == decl_ptr2) continue;
      if (decl_ptr1->identifier->token() != decl_ptr2->identifier->token())
        continue;

      auto scope_ptr = decl_ptr1->scope_;
      // If the shadowing occurs in the same scope, we don't need to display
      // the error message twice.
      if (scope_ptr == decl_ptr2->scope_) {
        if (decl_ptr1->line_num <= decl_ptr2->line_num) {
          error_log.log(decl_ptr1->line_num,
                        "Identifier `" + decl_ptr1->identifier->token() +
                            "` already declared in this scope (on line " +
                            std::to_string(decl_ptr2->line_num) + ").");
        }

        continue;
      }

      while (scope_ptr != nullptr) {
        if (scope_ptr == decl_ptr2->scope_) {
          error_log.log(decl_ptr1->line_num,
                        "Identifier `" + decl_ptr1->identifier->token() +
                            "` shadows identifier declared on line " +
                            std::to_string(decl_ptr2->line_num) + ".");
          // Do NOT skip out here. It's possible to have many shadows and we
          // might as well catch them all.
        }
        scope_ptr = scope_ptr->parent;
      }
    }
  }
}

void BlockScope::initialize() {
  builder.SetInsertPoint(entry);
  for (auto decl_ptr : ordered_decls_) {
    auto decl_id   = decl_ptr->identifier;
    auto decl_type = decl_id->type;

    if (!decl_type.stores_data()) continue;

    // if (decl_type->is_array()) {
    //   auto array_dim = static_cast<Array*>(decl_type)->dimension;
    //   std::vector<llvm::Value*> init_args(array_dim + 1,
    //   data::const_uint(0));
    //   init_args[0] = decl_id->alloc;
    //   // TODO
    //   // auto array_type = static_cast<Array*>(decl_type);
    //   // builder.CreateCall(array_type->initialize(), init_args);
    //   continue;

    // } else {
    if (decl_ptr->decl_type == AST::DeclType::In) continue;
    if (decl_id->is_function_arg) continue;
    decl_type.get->call_init({decl_id->alloc});
    // }
  }
}

void BlockScope::uninitialize() {
  builder.SetInsertPoint(exit);

  for (int i = static_cast<int>(ordered_decls_.size()) - 1; i >= 0; --i) {
    auto decl_id = ordered_decls_[static_cast<size_t>(i)]->identifier;

    // TODO is this correct?
    if (decl_id->is_function_arg) continue;

    decl_id->type.get->call_uninit({decl_id->alloc});
  }
}

void BlockScope::make_return(llvm::Value *val) {
  FnScope *fn_scope =
      is_function_scope() ? static_cast<FnScope *>(this) : containing_function_;

  // TODO multiple return values?

  if (fn_scope->fn_type->output.is_big()) {
    builder.CreateCall(fn_scope->fn_type->output.get->assign(),
                       {val, fn_scope->return_value});
  } else {
    builder.CreateStore(val, fn_scope->return_value);
  }

  builder.CreateBr(fn_scope->exit);
}

FnScope::FnScope(llvm::Function *fn)
    : BlockScope(ScopeType::Function), fn_type(nullptr), return_value(nullptr) {
  set_parent_function(fn);
}

void BlockScope::set_parent_function(llvm::Function *fn) {
  // NOTE previously you also checked if fn was a nullptr. Must you still do this?
  if (is_function_scope()) { static_cast<FnScope *>(this)->llvm_fn = fn; }

  if (entry && entry->getParent()) { entry->removeFromParent(); }
  if (exit && exit->getParent()) { exit->removeFromParent(); }

  if (fn) {
    entry->insertInto(fn);
    exit->insertInto(fn);
  }
}

void FnScope::add_scope(Scope *scope) { innards_.insert(scope); }
void FnScope::remove_scope(Scope *scope) { innards_.erase(scope); }

void FnScope::initialize() {
  builder.SetInsertPoint(entry);

  if (fn_type->output != Void) {
    // TODO multiple return types
    if (fn_type->output.is_big()) {
      // TODO which one? This works iff there is only one
      return_value = llvm_fn->args().begin();
    } else {
      return_value = builder.CreateAlloca(fn_type->output, nullptr, "retval");
    }
  }

  allocate(this);

  for (auto scope : innards_) { allocate(scope); }

  BlockScope::initialize();
}

void FnScope::leave() {
  builder.SetInsertPoint(exit);
  uninitialize();
  // TODO multiple return values
  auto ret_type = fn_type->output;
  if (ret_type == Void || ret_type.is_big()) {
    builder.CreateRetVoid();
  } else {
    builder.CreateRet(builder.CreateLoad(return_value));
  }
}


void FnScope::allocate(Scope* scope) {
  // TODO iterate through fn args
  for (const auto& decl_ptr : scope->ordered_decls_) {
    auto decl_id = decl_ptr->identifier;
    auto decl_type = decl_id->type;

    if (decl_id->is_function_arg && decl_type.is_struct()) {
      // Insert this alloc in the FunctionLiteral node
      continue;
    }

    // TODO make this for compile-time stuff
    if (decl_type == Type_) {
      // TODO Set the types name
      continue;
    }
   
    decl_id->alloc = decl_type.get->allocate();
    decl_id->alloc->setName(decl_ptr->identifier->token());
  }
}

bool Scope::is_loop_scope() {
  if (!is_block_scope()) return false;
  auto bs = static_cast<BlockScope*>(this);
  return bs->type == ScopeType::For || bs->type == ScopeType::While;
}
