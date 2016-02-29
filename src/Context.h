#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

#include <iostream>
#include <map>

struct Type;

namespace AST {
  struct Identifier;
  struct Expression;
}  // namespae AST

class Context {
  public:
    // TODO make to use types of the right size.
    union Value {
      bool             as_bool;
      char             as_char;
      int              as_int;
      double           as_real;
      size_t           as_uint;
      void*            as_null;
      Type*            as_type;
      AST::Expression* as_expr;

      Value(std::nullptr_t ptr = nullptr) { as_null = nullptr; }
      explicit Value(             bool b) { as_bool = b; }
      explicit Value(             char c) { as_char = c; }
      explicit Value(              int n) { as_int  = n; }
      explicit Value(           double d) { as_real = d; }
      explicit Value(           size_t n) { as_uint = n; }
      explicit Value(            Type* t) { as_type = t; }
      explicit Value( AST::Expression* e) { as_expr = e; }
    };

    void set_parent(Context* parent_ctx) { parent_ = parent_ctx; }
    bool has_return() { return has_ret_; }
    Value return_value() { return ret_val_; }
    void set_return_value(Value v);
    void bind(Value v, AST::Identifier *idptr);

    Context spawn();
    Value get(AST::Identifier *idptr);

    Context(Context* parent = nullptr) :
      ret_val_(nullptr), has_ret_(false), parent_(parent) {
    }
    size_t bindsize() const { return bindings_.size(); }

  private:
    Value ret_val_;
    bool has_ret_;
    std::map<AST::Identifier *, Value> bindings_;
    Context* parent_ = nullptr;
};

bool operator==(Context::Value lhs, Context::Value rhs);
bool operator!=(Context::Value lhs, Context::Value rhs);

#endif  // ICARUS_CONTEXT_H
