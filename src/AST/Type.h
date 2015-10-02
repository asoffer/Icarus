#ifndef ICARUS_AST_TYPE_H
#define ICARUS_AST_TYPE_H

namespace AST {
  enum Type {
    type_error, t_unknown, t_bool, t_char, t_int, t_real, t_string, t_type, t_void
  };
}  // namespace AST

#endif  // ICARUS_AST_TYPE_H
