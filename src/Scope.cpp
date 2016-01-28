#include "Scope.h"

#include <iostream>
#include <stack>
#include <algorithm>

#include "AST.h"
#include "ErrorLog.h"

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::Module* global_module;
extern llvm::DataLayout* data_layout;
extern ErrorLog error_log;

namespace cstdlib {
  extern llvm::Constant* memcpy();
}  // namespace cstdlib

namespace debug {
  extern bool dependency_system;
}  // namespace debug


std::map<IdPtr, DeclPtr> Scope::decl_of_ = {};
std::map<EPtr, std::set<EPtr>> Scope::dependencies_ = {};
std::vector<DeclPtr> Scope::decl_registry_ = {};
std::map<IdPtr, Scope*> Scope::scope_containing_ = {};

std::vector<Scope*> Scope::registry_;

namespace data {
  extern llvm::Value* const_uint(size_t n);
}  // namespace data


GlobalScope* Scope::build_global() {
  GlobalScope* scope_ptr = build<GlobalScope>();

  for (auto& ptr : registry_) {
    if (ptr == scope_ptr) continue;

    ptr->set_parent(scope_ptr);
  }

  scope_ptr->bldr_.SetInsertPoint(scope_ptr->entry_block());

  return scope_ptr;
}

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
    } else if (decl_type == Type_) {
      continue;
    } else {
      std::cerr << "FATAL: Global variables not currently allowed." << std::endl;
    }
  }
}

size_t Scope::num_scopes() {
  return registry_.size();
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
  if (fn_type_->return_type()->is_user_defined()) {
    auto iter = llvm_fn_->args().end();
    --iter;
    return_val_ = iter;
    // return_val_ is the last argument
    return_val_->setName("retval");

  } else if (fn_type_->return_type() != Void) {
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

void SimpleFnScope::exit() {
  // Cannot Branch to exit_block() because that's the same block!
  // Thus, calling up to Scope::exit() is not possible.
  uninitialize();

  if (fn_type_->return_type() == Void) {
    bldr_.CreateRetVoid();

  } else {
    bldr_.CreateRet(bldr_.CreateLoad(return_val_, "retval"));
  }
}

// Take in input and ignore it.
void FnScope::exit() {
  Scope::exit();

  if (fn_type_->return_type() == Void
      || fn_type_->return_type()->is_user_defined()) {
    bldr_.CreateRetVoid();

  } else {
    bldr_.CreateRet(bldr_.CreateLoad(return_val_, "retval"));
  }
}

void Scope::make_return(llvm::Value* val) {
  containing_function_->make_return(val);
}

void GenericFnScope::make_return(llvm::Value* val) {
  // nullptr means void return type
  if (val == nullptr) return;

  auto ret_type = fn_type_->return_type();
  if (ret_type->is_user_defined()) {
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

void GenericFnScope::add_scope(Scope* scope) {
  innards_.insert(scope);
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
    parent_->containing_function_->remove_scope(this);
  }

  parent_ = parent;
  if (parent->is_function_scope()) {
    containing_function_ = static_cast<GenericFnScope*>(parent_);
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
void GenericFnScope::allocate(Scope* scope) {
  // TODO iterate through fn args
  for (const auto& decl_ptr : scope->ordered_decls_) {
    auto decl_id = decl_ptr->declared_identifier();
    auto decl_type = decl_id->type();
    
    if (decl_id->is_function_arg_ && decl_type->is_user_defined()) {
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
    auto uninit_function = decl_id->type()->uninitialize();
    if (uninit_function != nullptr) {
      bldr_.CreateCall(uninit_function, decl_id->alloc_);
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
          decl_ptr->declared_type()->type();

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
    if (debug::dependency_system) {
      std::cout << "Make decl_of_: " << decl_id->token() << " (line " << decl_ptr->line_num() << ")" << std::endl;
      std::cout << "\t" << decl_id << " -> " << decl_ptr << std::endl;
      std::cout << "\tRHS: " << decl_ptr->declared_type() << std::endl;
    }
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

  // Push back all the sources (i.e., all the EPtrs with num_immediate_dep_refs
  // equal to zero
  for (const auto& kv : dependencies_) {
    // Ensure each identifier is present in the map
    num_immediate_dep_refs[kv.first];
    already_seen[kv.first] = 0x00;

    for (const auto& dep : kv.second) {
      num_immediate_dep_refs[dep]++;
    }
  }
  // num_immediate_dep_refs[foo] counts the number of things which directly
  // need foo's type in order to determine their own.

  // Start the stack with the sources of the dependency graph.
  // That is, those expressions which no one depends on.
  for (const auto& kv : num_immediate_dep_refs) {
    if (kv.second == 0) {
      expr_stack.push(kv.first);

      if (debug::dependency_system) {
        if (kv.first == nullptr) {
          std::cout << "Found a null pointer!" << std::endl;
        }
      }
    }
  }

  // Count the number of EPtrs seen. If at the end this isn't equal to the
  // total number, we know there's a cycle.
  //
  // TODO For better error messages we should write down what the cycle is.
  size_t num_seen = 0;


  // Preallocate a vector of the right size
  std::vector<EPtr> topo_order(already_seen.size(), nullptr);

  if (debug::dependency_system) {
    std::cout << "Expressions seen: " << already_seen.size() << std::endl;
  }

  // 0x02 means already seen and already popped into topo_order
  // 0x01 means seen but not yet popped
  // 0x00 means not yet seen

  // Standard depth-first search
  while (!expr_stack.empty()) {
    auto eptr = expr_stack.top();

    if (debug::dependency_system) {
      if (eptr == nullptr) {
        std::cerr << "Found a null pointer!" << std::endl;
        assert(false);

      } else {
        std::cout << "Looking at:   " << eptr << "\n" << *eptr << std::endl;
      }
    }


    if ((already_seen AT(eptr) & 2) == 2) {
      if (debug::dependency_system) {
        std::cout << "Already done: "  << eptr << "\n" << *eptr << std::endl;
      }
      // Already popped it into topo_order, so just ignore it
      expr_stack.pop();
      continue;

    }
    
    if ((already_seen AT(eptr) & 1) == 1) {
      // pop it off and put it in topo_order
      if (debug::dependency_system) {
        std::cout << "Adding:       " << eptr << "\n"  << *eptr << std::endl;
      }

      expr_stack.pop();
      topo_order[num_seen] = eptr;

      // mark it as already seen
      already_seen AT(eptr) = 0x03;
      ++num_seen;
      continue;
    }


    already_seen[eptr] = 0x01;

    for (const auto& dep : dependencies_[eptr]) {
      if (debug::dependency_system) {
        std::cout << "Found deps:   " << dependencies_[eptr].size() << std::endl;

        if (dep == nullptr) {
          std::cerr
            << "Looking at a null dependency from "
            << *eptr
            << " seen on line "
            << eptr->line_num()
            << std::endl;
          assert(false);
        }

        if (already_seen.find(dep) == already_seen.end()) {
          std::cerr
            << "FATAL: Dependency has not been seen yet: "
            << dep << std::endl
            << *dep << std::endl
            << "COMING FROM " << eptr << "\n" << *eptr << std::endl;
          assert(false);
        }
      }

      if ((already_seen AT(dep) & 2) == 2) {
        if (debug::dependency_system) {
          std::cout << "Skipping:     " << eptr << "\n"  << *eptr << std::endl;
        }
        continue;
      }

      if ((already_seen AT(dep) & 1) == 1) {
        error_log.log(dep->line_num(), "Cyclic dependency found.");
        // TODO give information about cycle
        return;
      }

      if (debug::dependency_system) {
        std::cout << "Pushing dep:  " << eptr << "\n"  << *dep << std::endl;
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
