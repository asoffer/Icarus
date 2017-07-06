#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <algorithm>
#include <memory>
#include <unordered_map>
#include <vector>

#include "../base/debug.h"
#include "../base/util.h"
#include "../cursor.h"
#include "../error_log.h"
#include "../ir/ir.h"
#include "../scope.h"
#include "../type/type.h"

struct Scope;

namespace Hashtag {
size_t GetOrFailValue(const std::string &tag);
} // namespace Hashtag

namespace AST {

#define ENDING = 0
#define OVERRIDE
#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void assign_scope(Scope *scope) ENDING;                              \
  virtual void lrvalue_check() ENDING;                                         \
  virtual void verify_types(std::vector<Error> *) ENDING

#define EXPR_FNS(name)                                                         \
  virtual ~name(){};                                                           \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void lrvalue_check() ENDING;                                         \
  virtual void assign_scope(Scope *scope) ENDING;                              \
  virtual void verify_types(std::vector<Error> *) ENDING

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const = 0;
  virtual void lrvalue_check() {}
  virtual void assign_scope(Scope *) {}
  virtual void verify_types(std::vector<Error> *) {}
  virtual void VerifyReturnTypes(Type *, std::vector<Error> *) {}
  virtual IR::Val EmitIR(std::vector<Error> *) { NOT_YET; }

  virtual bool is_hole() const { return false; }

  Node(Cursor cursor = Cursor()) : loc(cursor) {}
  virtual ~Node() {}

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  Cursor loc;
};

struct Expression : public Node {
  Expression();
  EXPR_FNS(Expression);
  static std::unique_ptr<Node>
  AddHashtag(std::vector<std::unique_ptr<AST::Node>> nodes);

  virtual void VerifyReturnTypes(Type *, std::vector<Error> *) {}

  virtual IR::Val EmitIR(std::vector<Error> *) {
    std::cerr << *this << std::endl;
    NOT_YET;
  }
  virtual IR::Val EmitLVal(std::vector<Error> *) {
    std::cerr << *this << std::endl;
    NOT_YET;
  }

  // Use these two functions to verify that an identifier can be declared using
  // these expressions. We pass in a string representing the identifier being
  // declared to be used in error messages.
  //
  // VerifyTypeForDeclaration verifies that the expresison represents a type and
  // returns the type it represents (or Error if the type is invalid). An
  // expression could be invalid if it doesn't represent a type or it represnts
  // void or parametric struct.
  Type *VerifyTypeForDeclaration(const std::string &id_tok,
                                 std::vector<Error> *errors);

  // VerifyValueForDeclaration verifies that the expression's type can be used
  // for a declaration. In practice, it is typically used on initial values for
  // a declaration. That is, when we see "foo := bar", we verify that the type
  // of bar is valid. This function has the same return characteristics as
  // VerifyTypeForDeclaration. Specifically, it returns the type or Error if the
  // type is invalid.
  Type *VerifyValueForDeclaration(const std::string &id_tok,
                                  std::vector<Error> *errors);

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
  Type *type = nullptr;
  IR::Val value;
};

struct TokenNode : public Node {
  virtual std::string to_string(size_t n) const;

  virtual IR::Val EmitIR(std::vector<Error> *) { NOT_YET; }

  virtual ~TokenNode() {}

  TokenNode(const Cursor &cursor = Cursor(), std::string str = "");

  std::string token;
  Language::Operator op;
};

#undef ENDING
#define ENDING override
#undef OVERRIDE
#define OVERRIDE override

struct Terminal : public Expression {
  EXPR_FNS(Terminal);
  Terminal() = default;
  Terminal(const Cursor &cursor, Language::Terminal term_type, Type *type,
           IR::Val val);

  virtual IR::Val EmitIR(std::vector<Error> *errors);

  virtual bool is_hole() const override {
    return terminal_type == Language::Terminal::Hole;
  }

  Language::Terminal terminal_type;
};

struct Identifier : public Terminal {
  Identifier() = delete;
  EXPR_FNS(Identifier);
  Identifier(const Cursor &cursor, const std::string &token_string);
  virtual IR::Val EmitIR(std::vector<Error> *errors);
  virtual IR::Val EmitLVal(std::vector<Error> *errors);

  std::string token;
  Declaration *decl = nullptr;
};

struct Binop : public Expression {
  EXPR_FNS(Binop);
  static std::unique_ptr<Node>
  BuildElseRocket(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildCallOperator(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildIndexOperator(std::vector<std::unique_ptr<AST::Node>> nodes);

  virtual IR::Val EmitIR(std::vector<Error> *errors);

  bool is_assignment() const {
    return op == Language::Operator::Assign || op == Language::Operator::OrEq ||
           op == Language::Operator::XorEq || op == Language::Operator::AndEq ||
           op == Language::Operator::AddEq || op == Language::Operator::SubEq ||
           op == Language::Operator::MulEq || op == Language::Operator::DivEq ||
           op == Language::Operator::ModEq;
  }

  Language::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration);
  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);
  IR::Val EmitIR(std::vector<Error> *errors) override;

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<Expression> type_expr;
  std::unique_ptr<Expression> init_val;
  IR::Val addr = IR::Val::None();

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
  EXPR_FNS(Generic);
  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);

  std::unique_ptr<Expression> test_fn;
};

struct InDecl : public Declaration {
  EXPR_FNS(InDecl);
  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);

  std::unique_ptr<Expression> container;
};

struct Statements : public Node {
  static std::unique_ptr<Node>
  build_one(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  build_more(std::vector<std::unique_ptr<AST::Node>> nodes);

  VIRTUAL_METHODS_FOR_NODES;
  void VerifyReturnTypes(Type *ret_val, std::vector<Error> *errors) override;
  IR::Val EmitIR(std::vector<Error> *errors) override;

  inline size_t size() { return statements.size(); }

  static std::unique_ptr<AST::Statements>
  Merge(std::vector<AST::Statements *> stmts_vec) {
    size_t num_stmts = 0;
    for (const auto stmts : stmts_vec) { num_stmts += stmts->size(); }

    auto result = std::make_unique<AST::Statements>();
    result->statements.reserve(num_stmts);

    for (auto &stmts : stmts_vec) {
      for (auto &stmt : stmts->statements) {
        result->statements.push_back(std::move(stmt));
      }
    }

    return result;
  }

  Statements() {}
  virtual ~Statements() {}

  std::vector<std::unique_ptr<AST::Node>> statements;
};

struct CodeBlock : public Expression {
  EXPR_FNS(CodeBlock);
  std::unique_ptr<Statements> stmts;
  std::string error_message; // To be used if stmts == nullptr

  virtual IR::Val EmitIR(std::vector<Error> *errors);
  static std::unique_ptr<Node>
  BuildFromStatementsSameLineEnd(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildEmpty(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildFromStatements(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildFromOneStatement(std::vector<std::unique_ptr<AST::Node>> nodes);
};

struct Unop : public Expression {
  EXPR_FNS(Unop);
  static std::unique_ptr<Node>
  BuildLeft(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildDots(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual void VerifyReturnTypes(Type *ret_val,
                                 std::vector<Error> *errors) override;
  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::unique_ptr<Expression> operand;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access);
  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(std::vector<Error> *errors);
  virtual IR::Val EmitLVal(std::vector<Error> *errors);

  std::string member_name;
  std::unique_ptr<Expression> operand;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp);
  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::vector<Language::Operator> ops;
  std::vector<std::unique_ptr<Expression>> exprs;
};

struct CommaList: public Expression {
  CommaList();
  EXPR_FNS(CommaList);

  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::vector<std::unique_ptr<Expression>> exprs;
};


struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral);
  static std::unique_ptr<Node>
  build(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildEmpty(std::vector<std::unique_ptr<AST::Node>> nodes);

  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::vector<std::unique_ptr<Expression>> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType);
  static std::unique_ptr<Node>
  build(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::unique_ptr<Expression> length;
  std::unique_ptr<Expression> data_type;
};

struct Case : public Expression {
  EXPR_FNS(Case);
  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::vector<
      std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>
      key_vals;
};

struct FunctionLiteral : public Expression {
  FunctionLiteral() {}
  EXPR_FNS(FunctionLiteral);
  static std::unique_ptr<Node>
  build(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildOneLiner(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildNoLiner(std::vector<std::unique_ptr<AST::Node>> nodes);

  virtual IR::Val EmitIR(std::vector<Error> *errors);

  std::unique_ptr<FnScope> fn_scope;
  std::unique_ptr<Expression> return_type_expr;

  std::vector<std::unique_ptr<Declaration>> inputs;
  std::unique_ptr<Statements> statements;

  IR::Func *ir_func = nullptr;

  std::set<Declaration *> captures;

  std::unordered_map<Type *, Declaration *> cache;
};

struct For : public Node {
  virtual ~For() {}
  VIRTUAL_METHODS_FOR_NODES;

  virtual IR::Val EmitIR(std::vector<Error> *errors);

  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);

  virtual void VerifyReturnTypes(Type *ret_val,
                                 std::vector<Error> *errors) override;

  std::vector<std::unique_ptr<InDecl>> iterators;
  std::unique_ptr<Statements> statements;
  std::unique_ptr<ExecScope> for_scope;
};

struct Jump : public Node {
  enum class JumpType { Restart, Continue, Repeat, Break, Return };

  virtual void VerifyReturnTypes(Type *ret_val,
                                 std::vector<Error> *errors) override;

  static std::unique_ptr<Node>
  build(std::vector<std::unique_ptr<AST::Node>> nodes);
  virtual ~Jump() {}

  VIRTUAL_METHODS_FOR_NODES;

  Jump(const Cursor &new_cursor, JumpType jump_type);

  ExecScope *scope;
  JumpType jump_type;
};

struct ScopeNode : public Expression {
  EXPR_FNS(ScopeNode);

  static std::unique_ptr<Node>
  Build(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildVoid(std::vector<std::unique_ptr<AST::Node>> nodes);
  static std::unique_ptr<Node>
  BuildScopeNode(std::unique_ptr<Expression> scope_name,
                 std::unique_ptr<Expression> arg_expr,
                 std::unique_ptr<Statements> stmt_node);
  virtual IR::Val EmitIR(std::vector<Error> *errors);

  // If the scope takes an argument, 'expr' is it. Otherwise 'expr' is null
  std::unique_ptr<Expression> expr;
  std::unique_ptr<Expression> scope_expr;
  std::unique_ptr<Statements> stmts;
  std::unique_ptr<ExecScope> internal;
};

struct ScopeLiteral : public Expression {
  ScopeLiteral() = delete;
  EXPR_FNS(ScopeLiteral);

  IR::Val EmitIR(std::vector<Error> *errors) override;

  std::unique_ptr<Declaration> enter_fn;
  std::unique_ptr<Declaration> exit_fn;
  std::unique_ptr<Scope> body_scope;
  ScopeLiteral(const Cursor &cursor);
};

} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS
#undef ENDING
#undef OVERRIDE

#endif // ICARUS_AST_AST_H
