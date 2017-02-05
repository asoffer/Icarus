#ifndef ICARUS_AST_H
#define ICARUS_AST_H

namespace Hashtag {
size_t GetOrFailValue(const std::string &tag);
} // namespace Hashtag

namespace AST {

#define ENDING = 0
#define OVERRIDE
#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void assign_scope() ENDING;                                          \
  virtual void lrvalue_check() ENDING;                                         \
  virtual void verify_types() ENDING;                                          \
  virtual IR::Value EmitIR() ENDING;                                           \
  virtual Node *clone(size_t num_entries, TypeVariable **lookup_key,           \
                      Type **lookup_val) ENDING

#define EXPR_FNS(name, checkname)                                              \
  virtual ~name();                                                             \
  virtual bool is_##checkname() const OVERRIDE { return true; }                \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void lrvalue_check() ENDING;                                         \
  virtual void assign_scope() ENDING;                                          \
  virtual IR::Value EmitIR() ENDING;                                           \
  virtual IR::Value EmitLVal() ENDING;                                         \
  virtual void verify_types() ENDING;                                          \
  virtual Node *clone(size_t num_entries, TypeVariable **lookup_key,           \
                      Type **lookup_val) ENDING

struct Node {
  virtual std::string to_string(size_t n) const = 0;
  virtual void lrvalue_check() {}
  virtual void assign_scope() {}
  virtual void verify_types() {}
  virtual void VerifyReturnTypes(Type *) {}
  virtual Node *clone(size_t num_entries, TypeVariable **lookup_key,
                      Type **lookup_val);
  virtual IR::Value EmitIR() = 0;

  virtual bool is_identifier() const { return false; }
  virtual bool is_terminal() const { return false; }
  virtual bool is_expression() const { return false; }
  virtual bool is_binop() const { return false; }
  virtual bool is_generic() const { return false; }
  virtual bool is_function_literal() const { return false; }
  virtual bool is_chain_op() const { return false; }
  virtual bool is_case() const { return false; }
  virtual bool is_eval() const { return true; }
  virtual bool is_unop() const { return false; }
  virtual bool is_access() const { return false; }
  virtual bool is_comma_list() const { return false; }
  virtual bool is_in_decl() const { return false; }
  virtual bool is_declaration() const { return false; }
  virtual bool is_indecl() const { return false; }
  virtual bool is_array_type() const { return false; }
  virtual bool is_statements() const { return false; }
  virtual bool is_scope() const { return false; }
  virtual bool is_array_literal() const { return false; }
  virtual bool is_token_node() const { return false; }
  virtual bool is_dummy() const { return false; }
  virtual bool is_jump() const { return false; }
  virtual bool is_hole() const { return false; }

  Node(Cursor cursor = Cursor())
      : scope_(nullptr), loc(cursor), time_(Time::error) {}

  virtual ~Node() {}

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_;

  Cursor loc;
  Time::Eval time_;
};

struct Expression : public Node {
  Expression();
  EXPR_FNS(Expression, expression);
  static Node *AddHashtag(NPtrVec &&nodes);

  virtual void VerifyReturnTypes(Type *) {}

  // Use these two functions to verify that an identifier can be declared using
  // these expressions. We pass in a string representing the identifier being
  // declared to be used in error messages.
  //
  // VerifyTypeForDeclaration verifies that the expresison represents a type and
  // returns the type it represents (or Error if the type is invalid). An
  // expression could be invalid if it doesn't represent a type or it represnts
  // void or parametric struct.
  Type *VerifyTypeForDeclaration(const std::string &id_tok);

  // VerifyValueForDeclaration verifies that the expression's type can be used
  // for a declaration. In practice, it is typically used on initial values for
  // a declaration. That is, when we see "foo := bar", we verify that the type
  // of bar is valid. This function has the same return characteristics as
  // VerifyTypeForDeclaration. Specifically, it returns the type or Error if the
  // type is invalid.
  Type *VerifyValueForDeclaration(const std::string &id_tok);

  std::vector<size_t> hashtag_indices;
  inline bool HasHashtag(const std::string &str) const {
    size_t idx = Hashtag::GetOrFailValue(str);
    if (idx == FAIL) return false;
    for (const auto &tag_index : hashtag_indices) {
      if (tag_index == idx) return true;
    }
    return false;
  }

  size_t precedence;
  Assign lvalue;
  Type *type;
  IR::Value value;
};

struct TokenNode : public Node {
  virtual std::string to_string(size_t n) const;

  virtual Node *clone(size_t num_entries, TypeVariable **lookup_key,
                      Type **lookup_val);

  virtual IR::Value EmitIR() { assert(false); }

  virtual bool is_token_node() const { return true; }

  virtual ~TokenNode() {}

  TokenNode(const Cursor &cursor = Cursor(), const char *str_lit = "");

  const char *token;
  Language::Operator op;
};

#undef ENDING
#define ENDING override
#undef OVERRIDE
#define OVERRIDE override

struct Terminal : public Expression {
  EXPR_FNS(Terminal, terminal);
  Language::Terminal terminal_type;

  virtual bool is_hole() const override {
    return terminal_type == Language::Terminal::Hole;
  }

};

struct Identifier : public Terminal {
  Identifier() = delete;
  EXPR_FNS(Identifier, identifier);
  Identifier(const Cursor &cursor, const std::string &token_string);

  std::string token;
  Declaration *decl = nullptr;
};

struct Binop : public Expression {
  EXPR_FNS(Binop, binop);
  static Node *BuildElseRocket(NPtrVec &&nodes);
  static Node *BuildCallOperator(NPtrVec &&nodes);
  static Node *BuildIndexOperator(NPtrVec &&nodes);

  bool is_assignment() const {
    using Language::Operator;
    return op == Operator::Assign || op == Operator::OrEq ||
           op == Operator::XorEq || op == Operator::AndEq ||
           op == Operator::AddEq || op == Operator::SubEq ||
           op == Operator::MulEq || op == Operator::DivEq ||
           op == Operator::ModEq;
  }

  Language::Operator op;
  Expression *lhs = nullptr, *rhs = nullptr;
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration, declaration);
  static Node *Build(NPtrVec &&nodes);

  void AllocateGlobal();
  void EmitGlobal();
  void EmitLLVMGlobal();

  void AllocateLocally(IR::Func *fn);

  Identifier *identifier = nullptr;
  Expression *type_expr  = nullptr;
  Expression *init_val   = nullptr;
  IR::Value addr = IR::Value::None();

  // If it's an argument, this points to the function/parametric-struct for
  // which it's an argument. Otherwise this field is null.
  Expression *arg_val = nullptr;

  inline bool IsInferred() const { return !type_expr; }
  inline bool IsDefaultInitialized() const { return !init_val; }
  inline bool IsCustomInitialized() const {
    return init_val && !init_val->is_hole();
  }
  inline bool IsUninitialized() const {
    return init_val && init_val->is_hole();
  }
};

struct Generic : public Declaration {
  EXPR_FNS(Generic, generic);
  static Node *Build(NPtrVec &&nodes);

  virtual bool is_declaration() const override { return false; }
  Expression *test_fn = nullptr;
};

struct InDecl : public Declaration {
  EXPR_FNS(InDecl, in_decl);
  static Node *Build(NPtrVec &&nodes);

  virtual bool is_indecl() const override { return true; }
  virtual bool is_declaration() const override { return false; }
  Expression *container = nullptr;
};

struct Statements : public Node {
  static Node *build_one(NPtrVec &&nodes);
  static Node *build_more(NPtrVec &&nodes);

  VIRTUAL_METHODS_FOR_NODES;
  virtual void VerifyReturnTypes(Type *ret_val) override;

  inline size_t size() { return statements.size(); }

  bool is_statements() const override { return true; }

  void add(Statements *stmts) {
    for (const auto &stmt : stmts->statements) { statements.push_back(stmt); }
  }

  Statements() {}
  virtual ~Statements();

  std::vector<AST::Node *> statements;
};

struct Unop : public Expression {
  EXPR_FNS(Unop, unop);
  static Node *BuildLeft(NPtrVec &&nodes);
  static Node *BuildDots(NPtrVec &&nodes);
  virtual void VerifyReturnTypes(Type *ret_val) override;

  Expression *operand = nullptr;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access, access);
  static Node *Build(NPtrVec &&nodes);

  void Verify(bool emit_errors);
  std::string member_name;
  Expression *operand;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp, chain_op);
  static Node *Build(NPtrVec &&nodes);

  virtual bool is_comma_list() const override {
    return ops.front() == Language::Operator::Comma;
  }

  std::vector<Language::Operator> ops;
  std::vector<Expression *> exprs;
};

struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral, array_literal);
  static Node *build(NPtrVec &&nodes);
  static Node *BuildEmpty(NPtrVec &&nodes);

  std::vector<Expression *> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType, array_type);
  static Node *build(NPtrVec &&nodes);

  Expression *length    = nullptr;
  Expression *data_type = nullptr;
};

struct Case : public Expression {
  EXPR_FNS(Case, case);
  static Node *Build(NPtrVec &&nodes);

  std::vector<std::pair<Expression *, Expression *>> key_vals;
};

struct FunctionLiteral : public Expression {
  FunctionLiteral();
  EXPR_FNS(FunctionLiteral, function_literal);
  static Node *build(NPtrVec &&nodes);
  static Node *BuildOneLiner(NPtrVec &&nodes);
  static Node *BuildNoLiner(NPtrVec &&nodes);

  FnScope *fn_scope            = nullptr;
  Expression *return_type_expr = nullptr;

  std::vector<Declaration *> inputs;
  llvm::Function *llvm_fn = nullptr;
  Statements *statements  = nullptr;

  inline IR::Value EmitAnonymousIR() { return Emit(false); }

  IR::Func *ir_func = nullptr;

  std::map<Type *, Declaration *> cache;

private:
  IR::Value Emit(bool should_gen);

};

struct Conditional : public Node {
  static Node *BuildIf(NPtrVec &&nodes);
  static Node *build_else_if(NPtrVec &&nodes);
  static Node *build_else(NPtrVec &&nodes);

  VIRTUAL_METHODS_FOR_NODES;
  virtual void VerifyReturnTypes(Type *ret_val) override;

  bool has_else() const { return else_line_num != 0; }

  Conditional() : else_line_num(0) {}
  virtual ~Conditional();

  std::vector<Expression *> conditions;
  std::vector<Statements *> statements;
  std::vector<BlockScope *> body_scopes;

  // We use else_line_num to determine if an else branch exists (when it's
  // non-zero) and also for error generation (if multiple else-blocks are
  // present).
  size_t else_line_num;
};

struct For : public Node {
  For();
  virtual ~For();
  VIRTUAL_METHODS_FOR_NODES;

  static Node *Build(NPtrVec &&nodes);

  virtual void VerifyReturnTypes(Type *ret_val) override;

  std::vector<InDecl *> iterators;
  Statements *statements;

  BlockScope *for_scope;
};

struct While : public Node {
  While();
  virtual ~While();
  VIRTUAL_METHODS_FOR_NODES;
  virtual void VerifyReturnTypes(Type *ret_val) override;

  static Node *Build(NPtrVec &&nodes);

  Expression *condition;
  Statements *statements;
  BlockScope *while_scope;
};

struct DummyTypeExpr : public Expression {
  DummyTypeExpr() = delete;
  EXPR_FNS(DummyTypeExpr, dummy);
  static Node *build(NPtrVec &&nodes);

  DummyTypeExpr(const Cursor &cursor, Type *t);
};

struct Jump : public Node {
  enum class JumpType { Restart, Continue, Repeat, Break, Return };
  virtual bool is_jump() const override { return true; }

  virtual void VerifyReturnTypes(Type *ret_val) override;

  static Node *build(NPtrVec &&nodes);
  virtual ~Jump();

  VIRTUAL_METHODS_FOR_NODES;

  Jump(const Cursor &new_cursor, JumpType jump_type);

  BlockScope *scope;
  JumpType jump_type;
};

struct ScopeNode : public Node {
  ScopeNode();
  virtual ~ScopeNode();
  VIRTUAL_METHODS_FOR_NODES;

  static Node *Build(NPtrVec &&nodes);
  static Node *BuildVoid(NPtrVec &&nodes);
  static Node *BuildScopeNode(Expression *scope_name, Expression *arg_expr,
                              Statements *stmt_node);
  Expression *scope_expr = nullptr;
  Expression *expr       = nullptr; // If the scope takes an argument, this is it
  Statements *stmts      = nullptr;
  BlockScope *internal   = nullptr;

  // Member variable 'type' exists only so we can have this set to Unknown, Err,
  // or some value (0x1) indicating that we have successfully passed type
  // verification.
  Type *type = nullptr;
};

struct ScopeLiteral : public Expression {
  ScopeLiteral() = delete;
  EXPR_FNS(ScopeLiteral, scope);

  Declaration *enter_fn = nullptr;
  Scope *body_scope     = nullptr;
  ScopeLiteral(const Cursor &cursor);
};

} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS
#undef ENDING
#undef OVERRIDE

#endif // ICARUS_AST_NODE_H
