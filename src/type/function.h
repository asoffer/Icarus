#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include "type.h"

namespace type {
struct Function : public Type {
  TYPE_FNS(Function);
  Function(std::vector<const Type *> in, std::vector<const Type *> out)
      : input(in), output(out) {}

  const Function* ToIR() const;
  std::vector<const Type *> input, output;
};

const Function *Func(const Type *in, const Type *out);
const Function *Func(std::vector<const Type *> in, const Type *out);
const Function *Func(const Type *in, std::vector<Type *> out);
const Function *Func(std::vector<const Type *> in,
                     std::vector<const Type *> out);

} // namespace type

#endif // ICARUS_TYPE_FUNCTION_H
