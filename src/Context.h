#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

#include <iostream>
#include <map>

#include "typedefs.h"

class Context {
  public:
    // TODO make to use types of the right size.
    union Value {
      bool   as_bool;
      char   as_char;
      int    as_int;
      double as_real;
      size_t as_uint;
      void*  as_null;

      explicit Value(        ) { as_null = nullptr; }
      explicit Value(  bool b) { as_bool = b; }
      explicit Value(  char c) { as_char = c; }
      explicit Value(   int n) { as_int  = n; }
      explicit Value(double d) { as_real = d; }
      explicit Value(size_t n) { as_uint = n; }
    };

    bool has_return() { return has_ret_; }
    Value return_value() { return ret_val_; }
    void set_return_value(Value v);
    void bind(EPtr eptr, IdPtr idptr);

    Context spawn();
    EPtr get(IdPtr idptr);

    static Context GlobalContext;

    Context() : has_ret_(false), parent_(nullptr) {
    }

  private:
    Value ret_val_;
    bool has_ret_;
    std::map<IdPtr, EPtr> bindings_;
    Context* parent_ = nullptr;
};

#endif  // ICARUS_CONTEXT_H
