#include "Scope.h"

#include <iostream>
#include <stack>
#include <algorithm>

#include "AST.h"
#include "ErrorLog.h"

extern llvm::Module* global_module;
extern ErrorLog error_log;

std::map<IdPtr, DeclPtr> Scope::decl_of_;
std::map<EPtr, std::set<EPtr>> Scope::dependencies_;
std::vector<DeclPtr> Scope::decl_registry_;
std::map<IdPtr, Scope*> Scope::scope_containing_;

std::vector<Scope*> Scope::registry_;

namespace data {
  extern llvm::Value* const_uint(size_t n);
}  // namespace data


FnScope* Scope::build_global() {
  // TODO add an explicit main entry point and make this scope not a function.
  FnScope* scope_ptr = build<FnScope>();

  for (auto& ptr : registry_) {
    if (ptr == scope_ptr) continue;

    ptr->parent_ = scope_ptr;
  }

  return scope_ptr;
}

size_t Scope::num_scopes() {
  return registry_.size();
}

void Scope::enter() {
  bldr_.SetInsertPoint(entry_block_);

  for (const auto& decl_ptr : ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    auto decl_type = decl_id->type();

    if (decl_type->is_array()) continue;
    decl_type->initialize(bldr_, decl_id->alloc_);
  }
}

void FnScope::enter() {
  bldr_.SetInsertPoint(entry_block_);

  allocate(this);

  for (auto scope : innards_) {
    allocate(scope);
  }

  // Even though this is an allocation, it cannot be put in
  // FnScope::allocate() because that gets called multiple times
  if (return_type_ != Type::get_void()) {
    return_val_ = return_type_->allocate(bldr_);
    return_val_->setName("retval");
  }

  Scope::enter();
}


// TODO simplify this. No need for an exit block unless it's in FnScope
void Scope::exit(llvm::BasicBlock* jump_to) {
  bldr_.CreateBr(exit_block_);
  bldr_.SetInsertPoint(exit_block_);
  uninitialize();

  bldr_.CreateBr(jump_to);
}


// Take in input and ignore it.
void FnScope::exit(llvm::BasicBlock*) {
  bldr_.CreateBr(exit_block_);
  bldr_.SetInsertPoint(exit_block_);
  uninitialize();

  if (return_type_ == Type::get_void()) {
    bldr_.CreateRetVoid();

  } else {
    bldr_.CreateRet(bldr_.CreateLoad(return_val_, "retval"));
  }
}
void Scope::make_return_void() {
  containing_function_->make_return_void();
}

void Scope::make_return(llvm::Value* val) {
  containing_function_->make_return(val);
}

void FnScope::make_return_void() {
  return_type_ = Type::get_void();
}

void FnScope::make_return(llvm::Value* val) {
#ifdef DEBUG
  if (val == nullptr) {
    std::cerr << "FATAL: making a return value before it's allocation." << std::endl;
  }
#endif

  bldr_.CreateStore(val, return_val_);
}


// Set pointer to the parent scope. This is an independent concept from LLVM's
// "parent". For us, functions can be declared in local scopes, so we will
// likely need this structure.
void Scope::set_parent(Scope* parent) {
#ifdef DEBUG
  if (parent == nullptr) {
    std::cerr << "FATAL: Setting scope's parent to be a nullptr." << std::endl;
  }

  if (parent == this) {
    std::cerr << "FATAL: Setting parent as self." << std::endl;
  }
#endif

  if (parent_ != nullptr && parent_->containing_function_ != nullptr) {
    parent_->containing_function_->innards_.erase(this);
  }

  parent_ = parent;
  if (!is_function_scope()) {
    containing_function_ = parent_->containing_function_;
  }

  if (parent_->is_function_scope()) {
    static_cast<FnScope*>(parent_)->innards_.insert(this);

  } else {
    parent_->containing_function_->innards_.insert(this);
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
  for (const auto& decl_ptr : scope->ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    auto decl_type = decl_id->type();

    // TODO make this for compile-time stuff
    if (decl_type == Type::get_type()) {
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
    decl_id->type()->uninitialize(bldr_, decl_id->alloc_);

    // TODO make this for compile-time stuff
    if (decl_id->type() == Type::get_type()) {
      // TODO Set the types name
      continue;
    }

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

void Scope::determine_declared_types() {
  for (auto scope_ptr : registry_) {
    for (auto decl_ptr : scope_ptr->ordered_decls_) {
      if (decl_ptr->infer_type_) {
        // TODO do inference correctly inside this scope in necessary.
        decl_ptr->declared_type()->verify_types();
        decl_ptr->declared_identifier()->expr_type_ =
          decl_ptr->declared_type()->expr_type_;

      } else {
        decl_ptr->declared_identifier()->expr_type_ =
          decl_ptr->declared_type()->interpret_as_type(); // If no type inference
      }
    }
  }
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
  DeclPtr d(new AST::Declaration);
  decl_registry_.push_back(d);
  d->id_ = IdPtr(new AST::Identifier(line_num, id_string));
  d->line_num_ = line_num;

  return d;
}


void Scope::fill_db() {
  for (auto scope_ptr : Scope::registry_) {
    scope_ptr->ordered_decls_.clear();
  }

  for (const auto& decl_ptr : decl_registry_) {
    IdPtr decl_id = decl_ptr->declared_identifier();
    decl_of_[decl_id] = decl_ptr;

    // Build up dependencies_ starting with empty sets
    dependencies_[std::static_pointer_cast<AST::Expression>(decl_id)] = std::set<EPtr>();
  }
}

void Scope::assign_type_order() {
  // Counts the number of times a given IdPtr is an immediate dependency of
  // something else. So a value of zero means that nothing depends on it.
  std::map<EPtr, size_t> num_immediate_dep_refs;

  // Char just used as a mask
  std::map<EPtr, char> already_seen;

  std::stack<EPtr> expr_stack;

  // Push back all the sources
  for (const auto& kv : dependencies_) {
    // Ensure each identifier is present in the map
    num_immediate_dep_refs[kv.first];
    already_seen[kv.first] = 0x00;

    for (const auto& dep : kv.second) {
      num_immediate_dep_refs[dep]++;
    }
  }

  for (const auto& kv : num_immediate_dep_refs) {
    if (kv.second == 0) {
      expr_stack.push(kv.first);
    }
  }

  // Count the number of EPtrs seen. If at the end this isn't equal to the
  // total number, we know there's a cycle.
  //
  // TODO For better error messages we should write down what the cycle is.
  size_t num_seen = 0;


  // Preallocate a vector of the right size
  std::vector<EPtr> topo_order(already_seen.size(), nullptr);

  // 0x02 means already seen and already popped into topo_order
  // 0x01 means seen but not yet popped
  // 0x00 means not yet seen
  while (!expr_stack.empty()) {
    auto eptr = expr_stack.top();

    if ((already_seen[eptr] & 2) == 2) {
      // Already popped it into topo_order, so just ignore it
      expr_stack.pop();
      continue;
    }

    if ((already_seen[eptr] & 1) == 1) {
      // pop it off and put it in topo_order
      expr_stack.pop();
      topo_order[num_seen] = eptr;

      // mark it as already seen
      already_seen[eptr] = 0x03;
      ++num_seen;
      continue;
    }


    already_seen[eptr] = 0x01;

    for (const auto& dep : dependencies_[eptr]) {
      if ((already_seen[dep] & 2) == 2) continue;

      if ((already_seen[dep] & 1) == 1) {
        error_log.log(dep->line_num(), "Cyclic dependency found.");
        // TODO give information about cycle
        return;
      }

      expr_stack.push(dep);
    }
  }

  if (num_seen != already_seen.size()) {
    error_log.log(0, "A dependency cycle was found.");
    return;
  }

  for (const auto& eptr : topo_order) {
    eptr->verify_types();

    // If it's an identifier, push it into the declarations for the
    // appropriate scope, so they can be allocated correctly
    if (eptr->is_identifier()) {
      auto id_ptr = std::static_pointer_cast<AST::Identifier>(eptr);
      scope_containing_[id_ptr]->ordered_decls_
        .push_back(decl_of_[id_ptr]);
    }
  }
}

void Scope::set_parent_function(llvm::Function* fn) {
  if (entry_block_ != nullptr && entry_block_->getParent() != nullptr) {
    entry_block_->removeFromParent();
  }

  entry_block_->insertInto(fn);

  if (exit_block_ != nullptr && exit_block_->getParent() != nullptr) {
    exit_block_->removeFromParent();
  }

  exit_block_->insertInto(fn);
}
