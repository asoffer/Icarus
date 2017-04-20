#include "../scope.h"
#include "ast.h"

#define DELETE(ptr)                                                            \
  {                                                                            \
    delete ptr;                                                                \
    ptr = nullptr;                                                             \
  }

namespace AST {
Expression::~Expression() {}
Unop::~Unop() { DELETE(operand); }
Access::~Access() { DELETE(operand); }
Binop::~Binop() {
  DELETE(lhs);
  DELETE(rhs);
}

ChainOp::~ChainOp() {
  for (auto ptr : exprs) DELETE(ptr);
}

ArrayLiteral::~ArrayLiteral() {
  for(auto e : elems) DELETE(e);
}

ArrayType::~ArrayType() {
  DELETE(length);
  DELETE(data_type);
}

Terminal::~Terminal() {}
Identifier::~Identifier() {}

Case::~Case() {
  for (auto kv : key_vals) {
    DELETE(kv.first);
    DELETE(kv.second);
  }
}

Statements::~Statements() {
  for (auto node : statements) DELETE(node);
}

Generic::~Generic() {
  DELETE(identifier);
  DELETE(test_fn);
}

InDecl::~InDecl() {
  DELETE(identifier);
  DELETE(container);
}

Declaration::~Declaration() {
  DELETE(identifier);
  DELETE(type_expr);
  DELETE(init_val);
}

FunctionLiteral::~FunctionLiteral() {
  DELETE(fn_scope);
  DELETE(return_type_expr);
  for (auto decl : inputs) { DELETE(decl); }
  DELETE(statements);
}

ScopeNode::~ScopeNode() {
  DELETE(scope_expr);
  DELETE(expr);
  DELETE(stmts);
  DELETE(internal);
}

For::~For() {
  for (auto ptr : iterators) { DELETE(ptr); }
  DELETE(for_scope);
}

ScopeLiteral::~ScopeLiteral() {
  DELETE(enter_fn);
  DELETE(exit_fn);
}

CodeBlock::~CodeBlock() { DELETE(stmts); }

Jump::~Jump() {}
DummyTypeExpr::~DummyTypeExpr() {}
} // namespace AST
#undef DELETE
