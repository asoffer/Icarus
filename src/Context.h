#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

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

      Value(        ) { as_null = nullptr; }
      Value(  bool b) { as_bool = b; }
      Value(  char c) { as_char = c; }
      Value(   int n) { as_int  = n; }
      Value(double d) { as_real = d; }
      Value(size_t n) { as_uint = n; }
    };

    bool has_return() { return has_ret; }
    Value return_value() { return ret_val; }
    void set_return_value(Value v) { ret_val = v; }

    Context() : has_ret(false) {
    }

  private:
    Value ret_val;
    bool has_ret;
};

#endif  // ICARUS_CONTEXT_H
