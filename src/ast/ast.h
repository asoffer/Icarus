#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <algorithm>
#include <map>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../base/debug.h"
#include "../base/owned_ptr.h"
#include "../base/util.h"
#include "../input/cursor.h"
#include "../ir/cmd.h"
#include "../scope.h"
#include "../type/type.h"
#include "dispatch.h"
#include "fn_args.h"
#include "operators.h"

enum class Assign : char { Unset, Const, LVal, RVal };

namespace IR {
struct Func;
} // namespace IR

namespace AST {
struct StageRange {
  // Last stage completed so far.
  int low = -1;
  // Last stage you can safely compute.
  int high = std::numeric_limits<int>::max();
  static constexpr int Nothing() { return -1; }
  static constexpr int NoEmitIR() { return 2; }
};

struct BoundConstants;

#define STAGE_CHECK                                                            \
  do {                                                                         \
    if (stage_range_.high < ThisStage()) { return; }                           \
    if (stage_range_.low >= ThisStage()) { return; }                           \
    stage_range_.low = ThisStage();                                            \
  } while (false)

#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const override;                      \
  std::string to_string() const { return to_string(0); }                       \
  virtual void assign_scope(Scope *scope) override;                            \
  virtual void Validate(const BoundConstants &bound_constants) override;       \
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args)        \
      override;                                                                \
  virtual void contextualize(                                                  \
      const Node *correspondant,                                               \
      const std::unordered_map<const Expression *, IR::Val> &) override

#define EXPR_FNS(name)                                                         \
  virtual ~name() {}                                                           \
  virtual std::string to_string(size_t n) const override;                      \
  std::string to_string() const { return to_string(0); }                       \
  virtual void assign_scope(Scope *scope) override;                            \
  virtual void Validate(const BoundConstants &bound_constants) override;       \
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args)        \
      override;                                                                \
  virtual void contextualize(                                                  \
      const Node *correspondant,                                               \
      const std::unordered_map<const Expression *, IR::Val> &) override

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const = 0;
  virtual void assign_scope(Scope *) {}
  virtual void Validate(const BoundConstants &bound_constants) = 0;

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) { NOT_YET(); }
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args) = 0;
  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &) = 0;
  virtual Node *Clone() const                                            = 0;

  std::string to_string() const { return to_string(0); }

  template <typename T> void limit_to(T &&t) {
    if constexpr (std::is_same_v<std::decay_t<T>, int>) {
      stage_range_.high = t;
    } else {
      stage_range_.high =
          std::min(stage_range_.high, std::forward<T>(t)->stage_range_.high);
    }
  }

  Node(const TextSpan &span = TextSpan()) : span(span) {}
  virtual ~Node() {}

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  StageRange stage_range_;
  TextSpan span;
};

struct Expression : public Node {
  Expression(const TextSpan &span = TextSpan()) : Node(span) {}
  virtual ~Expression(){};
  virtual std::string to_string(size_t n) const                         = 0;
  virtual void assign_scope(Scope *scope)                               = 0;
  virtual void Validate(const BoundConstants &bound_constants)          = 0;
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args) = 0;
  std::string to_string() const { return to_string(0); }

  virtual Expression *Clone() const = 0;

  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &) = 0;

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) {
    NOT_YET(*this);
  }
  virtual IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &) {
    NOT_YET(*this);
  }

  // Use these two functions to verify that an identifier can be declared using
  // these expressions. We pass in a string representing the identifier being
  // declared to be used in error messages.
  //
  // VerifyTypeForDeclaration verifies that the expresison represents a type and
  // returns the type it represents (or Error if the type is invalid). An
  // expression could be invalid if it doesn't represent a type or it represents
  // void.
  Type *VerifyTypeForDeclaration(const std::string &id_tok);

  // VerifyValueForDeclaration verifies that the expression's type can be used
  // for a declaration. In practice, it is typically used on initial values for
  // a declaration. That is, when we see "foo := bar", we verify that the type
  // of bar is valid. This function has the same return characteristics as
  // VerifyTypeForDeclaration. Specifically, it returns the type or Error if the
  // type is invalid.
  Type *VerifyValueForDeclaration(const std::string &id_tok);

  size_t precedence = std::numeric_limits<size_t>::max();
  Assign lvalue     = Assign::Unset;
  Type *type        = nullptr;
  IR::Val value     = IR::Val::None();
};

struct TokenNode : public Node {
  virtual std::string to_string(size_t n) const;

  void SaveReferences(Scope *, std::vector<IR::Val> *) { UNREACHABLE(); }
  TokenNode *Clone() const override;
  virtual void Validate(const BoundConstants &) {}

  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &);

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) {
    UNREACHABLE();
  }

  virtual ~TokenNode() {}

  TokenNode(const TextSpan &span = TextSpan(), std::string str = "");

  std::string token;
  Language::Operator op;
};

struct Terminal : public Expression {
  EXPR_FNS(Terminal);
  Terminal() = default;
  Terminal(const TextSpan &span, IR::Val val);

  Terminal *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
};

struct Identifier : public Terminal {
  Identifier() = delete;
  EXPR_FNS(Identifier);
  Identifier(const TextSpan &span, const std::string &token_string);
  Identifier *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  virtual IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &);

  std::string token;
  Declaration *decl = nullptr;
};

struct Hole : public Terminal {
  Hole() = delete;
  EXPR_FNS(Hole);
  Hole(const TextSpan &span) : Terminal(span, IR::Val::None()) {}
  Hole *Clone() const override;

  IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) override {
    return IR::Val::None();
  }
  IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &) override {
    return IR::Val::None();
  }
};

struct Binop : public Expression {
  EXPR_FNS(Binop);

  Binop *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  virtual IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &);

  bool is_assignment() const {
    return op == Language::Operator::Assign || op == Language::Operator::OrEq ||
           op == Language::Operator::XorEq || op == Language::Operator::AndEq ||
           op == Language::Operator::AddEq || op == Language::Operator::SubEq ||
           op == Language::Operator::MulEq || op == Language::Operator::DivEq ||
           op == Language::Operator::ModEq;
  }

  Language::Operator op;
  base::owned_ptr<Expression> lhs, rhs;
};

struct Call : public Expression {
  EXPR_FNS(Call);
  Call *Clone() const override;

  IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) override;

  base::owned_ptr<Expression> fn_;
  FnArgs<base::owned_ptr<Expression>> args_;

  // Filled in after type verification
  DispatchTable dispatch_table_;
  std::optional<DispatchTable>
  ComputeDispatchTable(std::vector<Expression *> fn_options);
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration);
  Declaration(bool is_const = false) : const_(is_const) {}

  Declaration *Clone() const override;
  IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) override;

  base::owned_ptr<Identifier> identifier;
  base::owned_ptr<Expression> type_expr;
  base::owned_ptr<Expression> init_val;

  // For non-const declarations, holds the address at which the value is being
  // stored. For const values (declared with :: or ::=), holds the actual
  // constant value.
  IR::Val addr = IR::Val::None();

  bool const_ = false;

  // If it's an argument or return value, this points to the function for which
  // it's an argument. Otherwise this field is null.
  // TODO rename this now that it no longer is just for function arguments
  Expression *arg_val = nullptr;

  inline bool IsInferred() const { return !type_expr; }
  inline bool IsDefaultInitialized() const { return !init_val; }
  inline bool IsCustomInitialized() const {
    return init_val && !init_val->is<Hole>();
  }
  inline bool IsUninitialized() const {
    return init_val && init_val->is<Hole>();
  }
};

struct InDecl : public Declaration {
  EXPR_FNS(InDecl);
  InDecl *Clone() const override;
  base::owned_ptr<Expression> container;
};

struct Statements : public Node {
  VIRTUAL_METHODS_FOR_NODES;
  Statements *Clone() const override;
  IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) override;

  inline size_t size() { return statements.size(); }

  static base::owned_ptr<AST::Statements>
  Merge(std::vector<base::owned_ptr<AST::Statements>> &&stmts_vec) {
    size_t num_stmts = 0;
    for (const auto &stmts : stmts_vec) { num_stmts += stmts->size(); }

    auto result = base::make_owned<AST::Statements>();
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

  std::vector<base::owned_ptr<AST::Node>> statements;
};

struct CodeBlock : public Expression {
  EXPR_FNS(CodeBlock);
  base::owned_ptr<Statements> stmts;
  std::string error_message; // To be used if stmts == nullptr

  CodeBlock *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
};

struct Unop : public Expression {
  EXPR_FNS(Unop);
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  virtual IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &);

  Unop *Clone() const override;
  base::owned_ptr<Expression> operand;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access);
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  virtual IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &);

  Access *Clone() const override;
  std::string member_name;
  base::owned_ptr<Expression> operand;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp);
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);

  ChainOp *Clone() const override;
  std::vector<Language::Operator> ops;
  std::vector<base::owned_ptr<Expression>> exprs;
};

struct CommaList : public Expression {
  CommaList();
  EXPR_FNS(CommaList);

  CommaList *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  virtual IR::Val EmitLVal(IR::Cmd::Kind, const BoundConstants &);

  std::vector<base::owned_ptr<Expression>> exprs;
};

struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral);
  ArrayLiteral *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);

  std::vector<base::owned_ptr<Expression>> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType);
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  ArrayType *Clone() const override;

  base::owned_ptr<Expression> length;
  base::owned_ptr<Expression> data_type;
};

struct Case : public Expression {
  EXPR_FNS(Case);
  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);
  Case *Clone() const override;

  std::vector<
      std::pair<base::owned_ptr<Expression>, base::owned_ptr<Expression>>>
      key_vals;
};

struct BoundConstants {
  static std::optional<BoundConstants>
  Make(const std::vector<base::owned_ptr<Declaration>> &inputs,
       const AST::FnArgs<base::owned_ptr<Expression>> &args);

  std::optional<IR::Val> operator()(Declaration *decl) const {
    auto iter = vals_.find(decl);
    if (iter == vals_.end()) { return std::nullopt; }
    return std::move(iter->second);
  }

  std::map<Declaration *, IR::Val> vals_;
};
} // namespace AST

namespace std {
template <> struct less<AST::BoundConstants> {
  bool operator()(const AST::BoundConstants &lhs,
                  const AST::BoundConstants &rhs) const {
    if (lhs.vals_.size() < rhs.vals_.size()) { return true; }
    if (lhs.vals_.size() > rhs.vals_.size()) { return false; }
    auto lhs_iter = lhs.vals_.begin();
    auto rhs_iter = rhs.vals_.begin();
    while (lhs_iter != lhs.vals_.end()) {
      ASSERT_EQ(lhs_iter->first, rhs_iter->first);
      if (lhs_iter->second.value < rhs_iter->second.value) { return true; }
      if (lhs_iter->second.value > rhs_iter->second.value) { return false; }
      ++lhs_iter;
      ++rhs_iter;
    }

    return false;
  }
};
} // namespace std

namespace AST {
struct FunctionLiteral : public Expression {
  FunctionLiteral() {}
  EXPR_FNS(FunctionLiteral);
  static base::owned_ptr<Node>
  BuildOneLiner(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildNoLiner(std::vector<base::owned_ptr<AST::Node>> nodes);
  FunctionLiteral *Clone() const override;

  IR::Func *Materialize(const FnArgs<base::owned_ptr<Expression>> &args);

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);

  base::owned_ptr<FnScope> fn_scope;
  base::owned_ptr<Expression> return_type_expr;

  std::vector<base::owned_ptr<Declaration>> inputs;
  base::owned_ptr<Statements> statements;

  std::map<BoundConstants, IR::Func *> ir_fns_;
  std::unordered_set<Declaration *> captures;
};

struct For : public Node {
  VIRTUAL_METHODS_FOR_NODES;
  virtual ~For() {}
  For *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);

  std::vector<base::owned_ptr<InDecl>> iterators;
  base::owned_ptr<Statements> statements;
  base::owned_ptr<ExecScope> for_scope;
};

struct Jump : public Node {
  VIRTUAL_METHODS_FOR_NODES;
  virtual ~Jump() {}
  Jump *Clone() const override;

  enum class JumpType { Restart, Continue, Repeat, Break, Return };

  Jump(const TextSpan &span, JumpType jump_type);

  ExecScope *scope;
  JumpType jump_type;
};

struct ScopeNode : public Expression {
  EXPR_FNS(ScopeNode);
  ScopeNode *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &);

  // If the scope takes an argument, 'expr' is it. Otherwise 'expr' is null
  base::owned_ptr<Expression> expr;
  base::owned_ptr<Expression> scope_expr;
  base::owned_ptr<Statements> stmts;
  base::owned_ptr<ExecScope> internal;
};

struct ScopeLiteral : public Expression {
  ScopeLiteral() = delete;
  EXPR_FNS(ScopeLiteral);
  ScopeLiteral *Clone() const override;

  IR::Val EmitIR(IR::Cmd::Kind, const BoundConstants &) override;

  base::owned_ptr<Declaration> enter_fn;
  base::owned_ptr<Declaration> exit_fn;
  base::owned_ptr<Scope> body_scope;
  ScopeLiteral(const TextSpan &span);
};

template <int N>
decltype(auto) DoStage(Node *node, Scope *scope, const BoundConstants &);
template <>
inline decltype(auto) DoStage<0>(Node *node, Scope *scope,
                                 const BoundConstants &) {
  node->assign_scope(scope);
}
template <>
inline decltype(auto) DoStage<1>(Node *node, Scope *,
                                 const BoundConstants &bound_constants) {
  node->Validate(bound_constants);
}

template <>
inline decltype(auto) DoStage<2>(Node *node, Scope *,
                                 const BoundConstants &bound_constants) {
  return node->EmitIR(IR::Cmd::Kind::Exec, bound_constants);
}

template <int Low, int High> struct ApplyStageRange {
  decltype(auto) operator()(Node *node, Scope *scope,
                            const BoundConstants &bound_constants) const {
    DoStage<Low>(node, scope, bound_constants);
    return ApplyStageRange<Low + 1, High>{}(node, scope, bound_constants);
  }
};
template <int N> struct ApplyStageRange<N, N> {
  decltype(auto) operator()(Node *node, Scope *scope,
                            const BoundConstants &bound_constants) const {
    return DoStage<N>(node, scope, bound_constants);
  }
};
template <int Low, int High>
decltype(auto) DoStages(Node *node, Scope *scope,
                        const BoundConstants &bound_constants) {
  return ApplyStageRange<Low, High>{}(node, scope, bound_constants);
}
} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS

#endif // ICARUS_AST_AST_H
