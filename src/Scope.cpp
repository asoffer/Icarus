#ifndef ICARUS_UNITY
#include "Scope.h"
#include "IR/IR.h"
#endif

static size_t scope_num_counter = 0;
BlockScope *Scope::Global       = new BlockScope(ScopeEnum::Global);

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

AST::Declaration *Scope::DeclReferencedOrNull(const std::string &name,
                                              Type *declared_type) {
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

std::vector<AST::Declaration *> Scope::AllDeclsWithId(const std::string &id) {
  std::vector<AST::Declaration *> matching_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr = scope_ptr->parent) {
    for (auto decl : scope_ptr->DeclRegistry) {
      if (decl->identifier->token != id) { continue; }
      decl->verify_types();
      if (decl->type == Err) { continue; }
      matching_decls.push_back(decl);
    }
  }
  return matching_decls;
}

// Set pointer to the parent scope. This is an independent concept from LLVM's
// "parent". For us, functions can be declared in local scopes, so we will
// likely need this structure.
void Scope::set_parent(Scope *new_parent) {
  assert(new_parent && "Setting scope's parent to be a nullptr.");
  assert(new_parent != this && "Setting parent as self");

  if (parent && parent->containing_function_) {
    parent->containing_function_->innards_.erase(this);
  }

  parent = new_parent;

  containing_function_ = new_parent->is_function_scope()
                             ? (FnScope *)parent
                             : parent->containing_function_;

  if (containing_function_) { containing_function_->innards_.insert(this); }
}

BlockScope::BlockScope(ScopeEnum st)
    : type(st), entry_block(nullptr), exit_block(nullptr) {}

void BlockScope::MakeReturn(Type *ret_type, IR::Value val) {
  // TODO actual returned type (second type arg) may be different
  Type::CallAssignment(this, ret_type, ret_type, val, GetFnScope()->ret_val);

  IR::Store(Char, RETURN_FLAG, GetFnScope()->exit_flag);
  IR::Block::Current->SetUnconditional(exit_block);
  // TODO set current block to be unreachable. access to it should trigger an
  // error that no code there will ever be executed.
}

void BlockScope::InsertDestroy() {
  assert(exit_block);
  assert(IR::Block::Current == exit_block);

  for (auto decl : DeclRegistry) {
    if (decl->arg_val || decl->is_in_decl() ||
        decl->type->time() == Time::compile) {
      continue;
    }
    decl->type->EmitDestroy(decl->identifier->EmitLVal());
  }
}

FnScope::FnScope()
    : BlockScope(ScopeEnum::Function), fn_type(nullptr),
      exit_flag(NORMAL_FLAG) {}
