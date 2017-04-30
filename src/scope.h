#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
struct FunctionLiteral;
} // namespace AST

namespace IR {
struct Block;
struct Func;
} // namespace IR

struct Type;
struct Function;

#include <vector>
#include <set>
#include "ir/ir.h"

enum class ScopeEnum { For, Function, Global, Type, Standard };

struct BlockScope;
struct FnScope;

struct Scope {
  static BlockScope *Global; // TODO Should this be it's own type

  void set_parent(Scope *parent);

  FnScope *GetFnScope() {
    return is_function_scope() ? (FnScope *)this : containing_function_;
  }

  virtual bool is_block_scope() { return false; }
  virtual bool is_function_scope() { return false; }
  bool is_loop_scope();

  // Returns an identifier pointer if there is a declaration of this identifier
  // in this scope. Otherwise it returns nullptr. It does *not* look in parent
  // scopes.
  AST::Identifier *IdentifierHereOrNull(const std::string &name);

  // Returns the identifier pointer being referenced by this string name, going
  // up the chaing of scopes as necessary. It returns nullptr if no such
  // identifier can be found.
  AST::Identifier *IdentifierBeingReferencedOrNull(const std::string &name);

  AST::Declaration *DeclHereOrNull(const std::string &name,
                                   Type *declared_type);

  AST::Declaration *DeclReferencedOrNull(const std::string &name,
                                         Type *declared_type);

  std::vector<AST::Declaration *> AllDeclsWithId(const std::string &id);

  Scope();
  Scope(const Scope &) = delete;
  Scope(Scope &&) = delete;
  virtual ~Scope() {}

  std::vector<AST::Declaration *> DeclRegistry;

  Scope *parent;
  FnScope *containing_function_;
  std::string name;
};

struct BlockScope : public Scope {
  BlockScope() = delete;
  BlockScope(ScopeEnum st);
  virtual ~BlockScope() {}
  virtual bool is_block_scope() { return true; }

  void InsertDestroy();

  void MakeReturn(IR::Val val);

  ScopeEnum type;
  IR::Block *entry_block, *exit_block;
};

struct FnScope : public BlockScope {
  FnScope(AST::FunctionLiteral *lit);
  virtual bool is_function_scope() { return true; }
  virtual ~FnScope() {}

  Function *fn_type            = nullptr;
  AST::FunctionLiteral *fn_lit = nullptr;
  std::set<Scope *> innards_;
  IR::Val exit_flag, ret_val;
};

inline bool Scope::is_loop_scope() {
  return is_block_scope() && ((BlockScope *)this)->type == ScopeEnum::For;
}

#endif // ICARUS_SCOPE_H
