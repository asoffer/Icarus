#include "ast.h"

static void copy_fields(const AST::Node *from, AST::Node *to) {
  to->scope_ = from->scope_;
  to->span   = from->span;
}

static void copy_fields(const AST::Expression *from, AST::Expression *to) {
  to->scope_     = from->scope_;
  to->span       = from->span;
  to->precedence = from->precedence;
  to->lvalue     = from->lvalue;
  to->type       = from->type;
  to->value      = from->value;
}

namespace AST {
base::owned_ptr<Terminal> Terminal::copy_stub() const {
  auto result = base::own(new Terminal);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<Identifier> Identifier::copy_stub() const {
  auto result = base::own(new Identifier(span, token));
  copy_fields(this, result.get());
  result->token = token;
  // TODO decl
  return result;
}

base::owned_ptr<CodeBlock> CodeBlock::copy_stub() const {
  auto result = base::own(new CodeBlock);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<Jump> Jump::copy_stub() const {
  auto result = base::own(new Jump(span, jump_type));
  copy_fields(this, result.get());
  // TODO execscope
  return result;
}

base::owned_ptr<Binop> Binop::copy_stub() const {
  auto result = base::own(new Binop);
  copy_fields(this, result.get());
  result->op = op;
  return result;
}

base::owned_ptr<Declaration> Declaration::copy_stub() const {
  auto result = base::own(new Declaration(const_));
  copy_fields(this, result.get());
  // TODO addr and arg_val
  return result;
}

base::owned_ptr<InDecl> InDecl::copy_stub() const {
  auto result = base::own(new InDecl);
  copy_fields(this, result.get());
  // TODO dispatch to Declaration
  return result;
}

base::owned_ptr<Statements> Statements::copy_stub() const {
  auto result = base::own(new Statements);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<Unop> Unop::copy_stub() const {
  auto result = base::own(new Unop);
  copy_fields(this, result.get());
  result->op = op;
  return result;
}

base::owned_ptr<Access> Access::copy_stub() const {
  auto result = base::own(new Access);
  copy_fields(this, result.get());
  result->member_name = member_name;
  return result;
}

base::owned_ptr<ChainOp> ChainOp::copy_stub() const {
  auto result = base::own(new ChainOp);
  copy_fields(this, result.get());
  result->ops = ops;
  return result;
}

base::owned_ptr<CommaList> CommaList::copy_stub() const {
  auto result = base::own(new CommaList);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<ArrayLiteral> ArrayLiteral::copy_stub() const {
  auto result = base::own(new ArrayLiteral);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<ArrayType> ArrayType::copy_stub() const {
  auto result = base::own(new ArrayType);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<Case> Case::copy_stub() const {
  auto result = base::own(new Case);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<FunctionLiteral> FunctionLiteral::copy_stub() const {
  auto result = base::own(new FunctionLiteral);
  copy_fields(this, result.get());
  // TODO ir_func, captures, cache
  return result;
}

base::owned_ptr<For> For::copy_stub() const {
  auto result = base::own(new For);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<ScopeNode> ScopeNode::copy_stub() const {
  auto result = base::own(new ScopeNode);
  copy_fields(this, result.get());
  return result;
}

base::owned_ptr<ScopeLiteral> ScopeLiteral::copy_stub() const {
  auto result = base::own(new ScopeLiteral(span));
  copy_fields(this, result.get());
  return result;
}
} // namespace AST
