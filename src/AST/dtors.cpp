#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

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
Eval::~Eval() { DELETE(expr); }

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

Conditional::~Conditional() {
  for (auto ptr : conditions) { DELETE(ptr); }
  for (auto ptr : statements) { DELETE(ptr); }
  for (auto ptr : body_scopes) { DELETE(ptr); }
}

While::~While() {
  DELETE(condition);
  DELETE(statements);
  DELETE(while_scope);
}

For::~For() {
  for (auto ptr : iterators) { DELETE(ptr); }
  DELETE(for_scope);
}

ParametricStructLiteral::~ParametricStructLiteral() {
  for (auto d : decls) DELETE(d);
  for (auto p : params) DELETE(p);
  DELETE(type_scope);
}
StructLiteral::~StructLiteral() {
  for (auto d : decls) DELETE(d);
  DELETE(type_scope);
}

EnumLiteral::~EnumLiteral() {}
Jump::~Jump() {}
DummyTypeExpr::~DummyTypeExpr() {}

} // namespace AST
#undef DELETE
