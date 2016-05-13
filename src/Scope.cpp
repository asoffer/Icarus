#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

extern llvm::DataLayout* data_layout;

namespace cstdlib {
extern llvm::Constant *memcpy();
} // namespace cstdlib

namespace data {
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::Value *null_pointer(Type *t);
} // namespace data

BlockScope *Scope::Global = nullptr; // Initialized in main
std::vector<AST::Declaration *> Scope::decl_registry_ = {};

std::stack<Scope *> Scope::Stack;

Context &CurrentContext() { return CurrentScope()->context; }
Scope *CurrentScope() {
  return Scope::Stack.empty() ? nullptr : Scope::Stack.top();
}

static size_t scope_num_counter = 0;
Scope::Scope()
    : parent(Scope::Global), containing_function_(nullptr),
      name("anon" + std::to_string(scope_num_counter++)) {}

AST::Identifier *Scope::identifier(AST::Expression *id_as_eptr) {
  assert(id_as_eptr->is_identifier());
  auto idptr = static_cast<AST::Identifier *>(id_as_eptr);

  Scope *scope_ptr = this;
  while (scope_ptr != nullptr) {
    auto iter = scope_ptr->ids_.find(idptr->token());
    if (iter != scope_ptr->ids_.end()) { return iter->second; }
    scope_ptr = scope_ptr->parent;
  }

  // If you reach here it's because we never saw a declaration for the
  // identifier
  error_log.log(idptr->loc, "Undeclared identifier `" + idptr->token() + "`.");

  return nullptr;
}

AST::Identifier *Scope::IdentifierHereOrNull(const std::string &name) {
  auto iter = ids_.find(name);
  return (iter == ids_.end()) ? nullptr : iter->second;
}

AST::Identifier *Scope::IdentifierBeingReferencedOrNull(const std::string &name) {
  auto scope_ptr = this;
  while (scope_ptr) {
    auto iter = scope_ptr->ids_.find(name);
    if (iter != scope_ptr->ids_.end()) { return iter->second; }

    scope_ptr = scope_ptr->parent;
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
  context.set_parent(&new_parent->context);

  containing_function_ = new_parent->is_function_scope()
                             ? static_cast<FnScope *>(parent)
                             : parent->containing_function_;

  if (containing_function_) { containing_function_->add_scope(this); }
}

BlockScope::BlockScope(ScopeType st)
    : type(st), entry(make_block("entry", nullptr)),
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
        if ((!decl_ptr1->type->is_function() ||
             !decl_ptr2->type->is_function()) &&
            decl_ptr1->loc.line_num <= decl_ptr2->loc.line_num) {
          error_log.log(decl_ptr1->loc,
                        "Identifier `" + decl_ptr1->identifier->token() +
                            "` already declared in this scope (on line " +
                            std::to_string(decl_ptr2->loc.line_num) + ").");
        }

        continue;
      }

      // TODO is_block_scope is a standin for not being a struct/enum scope
      while (scope_ptr != nullptr && scope_ptr->is_block_scope()) {
        if (scope_ptr == decl_ptr2->scope_) {
          error_log.log(decl_ptr1->loc,
                        "Identifier `" + decl_ptr1->identifier->token() +
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

void BlockScope::initialize() {
  builder.SetInsertPoint(entry);

  for (auto decl_ptr : ordered_decls_) {
    auto decl_id   = decl_ptr->identifier;
    auto decl_type = decl_id->type;

    if (decl_type->time() == Time::compile) continue;
    if (!decl_type->stores_data()) continue;

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
    if (decl_id->is_arg) continue;
    decl_type->call_init(decl_id->alloc);
    // }
  }
}

void BlockScope::uninitialize() {
  builder.SetInsertPoint(exit);

  while (!deferred_uninits.empty()) {
    auto &deferred = deferred_uninits.top();

    deferred.first->CallDestroy(this, deferred.second);
    deferred_uninits.pop();
  }

  for (int i = static_cast<int>(ordered_decls_.size()) - 1; i >= 0; --i) {
    auto decl_id = ordered_decls_[static_cast<size_t>(i)]->identifier;

    // TODO is this correct?
    if (decl_id->is_arg) continue;
    if (!decl_id->type->stores_data()) continue;

    decl_id->type->CallDestroy(this, decl_id->alloc);
  }
}

void BlockScope::defer_uninit(Type *type, llvm::Value *val) {
  deferred_uninits.emplace(type, val);
}

void BlockScope::make_return(llvm::Value *val) {
  FnScope *fn_scope =
      is_function_scope() ? static_cast<FnScope *>(this) : containing_function_;

  // TODO multiple return values?
  Type::CallAssignment(this, fn_scope->fn_type->output,
                       fn_scope->fn_type->output, fn_scope->return_value, val);
  builder.CreateBr(fn_scope->exit);
}

FnScope::FnScope(llvm::Function *fn)
    : BlockScope(ScopeType::Function), fn_type(nullptr), return_value(nullptr),
      exit_flag_(nullptr) {
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
}

llvm::Value *BlockScope::AllocateLocally(Type *type, const std::string& name) {
  auto ip = builder.saveIP();
  auto entry_block = is_function_scope() ? entry : containing_function_->entry;

  if (entry_block->empty()) {
    builder.SetInsertPoint(entry_block);
   } else {
    builder.SetInsertPoint(entry_block->begin());
  }

  auto local = builder.CreateAlloca(*type, nullptr, name);

  // TODO optimize this by only setting what needs to be set. For example,
  // arrays of primitives, you only need to set the pointer to null.
  type->call_init(local);

  builder.restoreIP(ip);
  return local;
}

llvm::Value *BlockScope::CreateLocalReturn(Type *type) {
  return AllocateLocally(type, "local.ret");
}

void FnScope::initialize() {
  builder.SetInsertPoint(entry);

  if (fn_type->output != Void) {

    // TODO multiple return types
    if (fn_type->output->is_big() && !fn_type->output->is_function()) {
      // Start here and move forward until it's in place.
      return_value = llvm_fn->args().begin();

      if (fn_type->input == Void) {

      } else if (fn_type->input->is_tuple()) {
        auto num_inputs =
            static_cast<Tuple *>(fn_type->input)->entries.size();

        // TODO is there a way to get direct access? Probably. Look it up.
        auto ret_val_arg = llvm_fn->args().begin();
        for (size_t i = 0; i < num_inputs; ++i) {
          ret_val_arg = ++ret_val_arg;
        }
        return_value = ret_val_arg;

      } else {
        return_value = ++llvm_fn->args().begin();
      }

    } else {
      return_value = builder.CreateAlloca(*(fn_type->output->is_function()
                                                ? Ptr(fn_type->output)
                                                : fn_type->output),
                                          nullptr, "retval");
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
  if (ret_type == Void || (ret_type->is_big() && !ret_type->is_function())) {
    builder.CreateRetVoid();
  } else {
    builder.CreateRet(builder.CreateLoad(return_value));
  }
}


void FnScope::allocate(Scope* scope) {
  // Check that on the way up you don't hit a FnScope. This makes it so you
  // don't allocate for a function declared in a function.
  if (scope != this &&
      (scope->is_function_scope() || scope->containing_function_ != this)) {
    return;
  }

  // TODO iterate through fn args
  for (const auto& decl_ptr : scope->ordered_decls_) {
    auto decl_id = decl_ptr->identifier;
    auto decl_type = decl_id->type;

    if (decl_id->is_arg && decl_type->is_big()) {
      // Insert this alloc in the FunctionLiteral node
      continue;
    }

    if (decl_type->time() == Time::compile) {
      // TODO Set the types name
      continue;
    }

    if (decl_type->is_quantum()) {
      decl_id->alloc = nullptr;
      continue;
    }

    decl_id->alloc = decl_type->allocate();
    decl_id->alloc->setName(decl_ptr->identifier->token());
  }
}

bool Scope::is_loop_scope() {
  if (!is_block_scope()) return false;
  auto bs = static_cast<BlockScope*>(this);
  return bs->type == ScopeType::For || bs->type == ScopeType::While;
}
