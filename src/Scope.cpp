#include "Scope.h"

#include <iostream>
#include <algorithm>

#include "AST.h"
#include "ErrorLog.h"

extern llvm::Module* global_module;
extern llvm::DataLayout* data_layout;
extern ErrorLog error_log;

namespace cstdlib {
  extern llvm::Constant* memcpy();
}  // namespace cstdlib

GlobalScope* Scope::Global = nullptr;  // Initialized in main

std::vector<DeclPtr> Scope::decl_registry_ = {};

namespace data {
  extern llvm::Value* const_uint(size_t n);
}  // namespace data

Scope::Scope() : parent_(Scope::Global), containing_function_(nullptr),
  bldr_(llvm::getGlobalContext()) {}

void GlobalScope::initialize() {
  for (const auto& decl_ptr : ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    if (decl_id->is_function_arg_) continue;

    auto decl_type = decl_id->type();
    if (decl_type->llvm() == nullptr) continue;

    if (decl_type->is_function()) {
      if (decl_id->token()[0] != '_') {  // Ignore operators
        decl_id->alloc_ = decl_type->allocate(bldr_);
        decl_id->alloc_->setName(decl_ptr->identifier_string());
      }
    } else {
      assert(decl_type == Type_ && "Global variables not currently allowed.");
    }
  }
}

FnScope::FnScope(llvm::Function* fn) :
  fn_type_(nullptr), return_val_(nullptr),
  entry(make_block("entry", nullptr)),
  exit(make_block("exit", nullptr))
{ 
  if (fn) set_parent_function(fn);
}

void Scope::enter() {
  bldr_.SetInsertPoint(entry_block());

  for (const auto& decl_ptr : ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    auto decl_type = decl_id->type();

    if (decl_type->is_function() || decl_type == Type_) {
      continue;

    } else if (decl_type->is_array()) {
      auto array_dim = static_cast<Array*>(decl_type)->dim();
      std::vector<llvm::Value*> init_args(array_dim + 1, data::const_uint(0));
      init_args[0] = decl_id->alloc_;
      auto array_type = static_cast<Array*>(decl_type);
      bldr_.CreateCall(array_type->initialize(), init_args);
      continue;

    } else {
      if (decl_id->is_function_arg_) continue;
      decl_type->call_init(bldr_, { decl_id->alloc_ });
    }
  }
}

void FnScope::enter() {
  bldr_.SetInsertPoint(entry_block());

  allocate(this);

  for (auto scope : innards_) {
    allocate(scope);
  }

  // Even though this is an allocation, it cannot be put in
  // FnScope::allocate() because that gets called multiple times
  // TODO multiple return types for now just take one
  if (fn_type_->return_type()->is_struct()) {
    auto iter = llvm_fn_->args().end();
    --iter;
    return_val_ = iter;
    // return_val_ is the last argument
    return_val_->setName("retval");

    // TODO multiple return types for now just take one
  } else if (fn_type_->return_type() != Void) {
    // TODO multiple return types for now just take one
    return_val_ = fn_type_->return_type()->allocate(bldr_);
    return_val_->setName("retval");
  }

  Scope::enter();
}

// TODO simplify this. No need for an exit block unless it's in FnScope
void Scope::exit() {
  bldr_.CreateBr(exit_block());
  bldr_.SetInsertPoint(exit_block());
  uninitialize();
}

void FnScope::exit() {
  Scope::exit();

  // TODO multiple return types for now just take one
  if (fn_type_->return_type() == Void
      // TODO multiple return types for now just take one
      || fn_type_->return_type()->is_struct()) {
    bldr_.CreateRetVoid();

  } else {
    bldr_.CreateRet(bldr_.CreateLoad(return_val_, "retval"));
  }
}

void Scope::make_return(llvm::Value* val) {
  containing_function_->make_return(val);
}

void FnScope::make_return(llvm::Value* val) {
  // nullptr means void return type
  if (val == nullptr) return;

  // TODO multiple return types for now just take one
  auto ret_type = fn_type_->return_type();
  if (ret_type->is_struct()) {
    // TODO pull out memcpy into a single fn call
    auto val_raw = bldr_.CreateBitCast(val, *RawPtr);
    auto ret_raw = bldr_.CreateBitCast(return_val_, *RawPtr);
    bldr_.CreateCall(cstdlib::memcpy(), { ret_raw, val_raw,
        data::const_uint(
          data_layout->getTypeStoreSize(ret_type->llvm())) });
  } else {
    bldr_.CreateStore(val, return_val_);
  }
}

void FnScope::add_scope(Scope* scope) {
  innards_.insert(scope);
}

// Set pointer to the parent scope. This is an independent concept from LLVM's
// "parent". For us, functions can be declared in local scopes, so we will
// likely need this structure.
void Scope::set_parent(Scope* parent) {
  assert(parent && "Setting scope's parent to be a nullptr.");
  assert(parent != this && "Setting parent as self");

  if (parent_ != nullptr && parent_->containing_function_ != nullptr) {
    parent_->containing_function_->remove_scope(this);
  }

  parent_ = parent;
  ctx_.set_parent(&parent->context());

  if (parent->is_function_scope()) {
    containing_function_ = static_cast<FnScope*>(parent_);
  } else {
    containing_function_ = parent_->containing_function_;
  }

  if (containing_function_ != nullptr) {
    containing_function_->add_scope(this);
  }
}

// Gets the type of that the identifier was declared as. This is a pointer to
// an expression object, rather than a Type object.
EPtr Scope::get_declared_type(IdPtr id_ptr) const {
  for (const auto& decl_ptr : ordered_decls_) {
    if (decl_ptr->id_ != id_ptr) continue;
    return decl_ptr->decl_type_;
  }

  // This cannot segfault because the program would have exited earlier
  // if it was undeclared.
  return parent()->get_declared_type(id_ptr);

}

// This function allocates all the things declared the scope parameter in this
// function scope.
//
// TODO maybe we should set this up differently, so it's a method of the scope
// and it just calls it using containing_function_?
void FnScope::allocate(Scope* scope) {
  // TODO iterate through fn args
  for (const auto& decl_ptr : scope->ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    auto decl_type = decl_id->type();

    if (decl_id->is_function_arg_ && decl_type->is_struct()) {
      // Insert this alloc in the FunctionLiteral node
      continue;
    }

    // TODO make this for compile-time stuff
    if (decl_type == Type_) {
      // TODO Set the types name
      continue;
    }
   
    decl_id->alloc_ = decl_type->allocate(bldr_);
    decl_id->alloc_->setName(decl_ptr->identifier_string());
  }
}

void Scope::uninitialize() {
  for (const auto& decl_ptr : ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    if (decl_id->is_function_arg_) continue;
    decl_id->type()->call_uninit(bldr_, { decl_id->alloc_ });
  }
}

// TODO have a getter-only version for when we know we've passed the
// verification step
EPtr Scope::identifier(EPtr id_as_eptr) {
  auto id_ptr = std::static_pointer_cast<AST::Identifier>(id_as_eptr);
  Scope* current_scope = this;
  while (current_scope != nullptr) {
    auto iter = current_scope->ids_.find(id_ptr->token());
    if (iter != current_scope->ids_.end()) {
      return std::static_pointer_cast<AST::Expression>(iter->second);
    }
    current_scope = current_scope->parent();
  }

  // If you reach here it's because we never saw a declaration for the identifier
  error_log.log(id_as_eptr->line_num(),
      "Undeclared identifier `" + id_as_eptr->token() + "`.");

  return nullptr;
}

EPtr Scope::identifier(const std::string& name) const {
  auto iter = ids_.find(name);
  if (iter == ids_.end()) {
    if (parent_) return parent_->identifier(name);
    return nullptr;
  }
  return iter->second;
}

void Scope::verify_no_shadowing() {
  for (auto decl_ptr1 : decl_registry_) {
    for (auto decl_ptr2 : decl_registry_) {
      if (decl_ptr1 == decl_ptr2) continue;
      if (decl_ptr1->identifier_string() != decl_ptr2->identifier_string()) continue;

      auto scope_ptr = decl_ptr1->scope_;
      // If the shadowing occurs in the same scope, we don't need to display
      // the error message twice.
      if (scope_ptr == decl_ptr2->scope_) {
        if (decl_ptr1->line_num() <= decl_ptr2->line_num()) {
          error_log.log(decl_ptr1->line_num(),
              "Identifier `" + decl_ptr1->identifier_string()
              + "` already declared in this scope (on line "
              + std::to_string(decl_ptr2->line_num()) + ").");
        }

        continue;
      }

      while (scope_ptr != nullptr) {
        if (scope_ptr == decl_ptr2->scope_) {
          error_log.log(decl_ptr1->line_num(),
              "Identifier `" + decl_ptr1->identifier_string() + "` shadows identifier declared on line " + std::to_string(decl_ptr2->line_num()) + ".");
          // Do NOT skip out here. It's possible to have many shadows and we
          // might as well catch them all.
        }
        scope_ptr = scope_ptr->parent_;
      }
    }
  }
}

DeclPtr Scope::make_declaration(size_t line_num, const std::string& id_string) {
  auto d = std::make_shared<AST::Declaration>();
  decl_registry_.emplace_back(d);
  d->id_ = std::make_shared<AST::Identifier>(line_num, id_string);
  d->line_num_ = line_num;

  return d;
}

void FnScope::set_parent_function(llvm::Function* fn) {
  llvm_fn_ = fn;
  Scope::set_parent_function(fn);
}

void Scope::set_parent_function(llvm::Function* fn) {
  if (entry_block() != nullptr && entry_block()->getParent() != nullptr) {
    entry_block()->removeFromParent();
  }

  entry_block()->insertInto(fn);

  if (exit_block() != nullptr && exit_block()->getParent() != nullptr) {
    exit_block()->removeFromParent();
  }

  exit_block()->insertInto(fn);
}

void WhileScope::set_parent_function(llvm::Function* fn) {
  Scope::set_parent_function(fn);

  if (land_block_ != nullptr && land_block_->getParent() != nullptr) {
    land_block_->removeFromParent();
  }

  land_block_->insertInto(fn);

  if (cond_block_ != nullptr && cond_block_->getParent() != nullptr) {
    cond_block_->removeFromParent();
  }

  cond_block_->insertInto(fn);

}

void WhileScope::enter() {
  Scope::enter();
  bldr_.CreateBr(cond_block_);
  bldr_.SetInsertPoint(cond_block_);
}


void WhileScope::exit() {
  Scope::exit();
  bldr_.CreateBr(cond_block_);
}
