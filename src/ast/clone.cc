#include "ast.h"

namespace AST {
TokenNode *TokenNode::Clone() const { return new TokenNode(*this); }
Terminal *Terminal::Clone() const { return new Terminal(*this); }
Identifier *Identifier::Clone() const { return new Identifier(*this); }
Hole *Hole::Clone() const { return new Hole(*this); }
Binop *Binop::Clone() const { return new Binop(*this); }
Call *Call::Clone() const { return new Call(*this); }
Declaration *Declaration::Clone() const { return new Declaration(*this); }
InDecl *InDecl::Clone() const { return new InDecl(*this); }
Statements *Statements::Clone() const { return new Statements(*this); }
CodeBlock *CodeBlock::Clone() const { return new CodeBlock(*this); }
Unop *Unop::Clone() const { return new Unop(*this); }
Access *Access::Clone() const { return new Access(*this); }
ChainOp *ChainOp::Clone() const { return new ChainOp(*this); }

CommaList *CommaList::Clone() const { return new CommaList(*this); }
ArrayLiteral *ArrayLiteral::Clone() const { return new ArrayLiteral(*this); }
ArrayType *ArrayType::Clone() const { return new ArrayType(*this); }
Case *Case::Clone() const { return new Case(*this); }

FunctionLiteral *FunctionLiteral::Clone() const {
  return new FunctionLiteral(*this);
}
For *For::Clone() const { return new For(*this); }
Jump *Jump::Clone() const { return new Jump(*this); }
ScopeNode *ScopeNode::Clone() const { return new ScopeNode(*this); }
ScopeLiteral *ScopeLiteral::Clone() const { return new ScopeLiteral(*this); }
} // namespace AST
