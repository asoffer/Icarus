#ifndef ICARUS_TYPE_GENERIC_FUNCTION_H
#define ICARUS_TYPE_GENERIC_FUNCTION_H

#include "ir/function.h"
#include "type/basic.h"
#include "type/function.h"
#include "type/type_system.pb.h"

namespace ic::type {

struct GenericFunctionType : internal_type::BasicType {
  Evaluation evaluation() const;
  IrFunction const& function() const;

 private:
  friend Type;

  friend GenericFunctionType GenericFunction(Evaluation e,
                                             IrFunction const* fn);

  explicit GenericFunctionType() = default;
  explicit constexpr GenericFunctionType(uint64_t n)
      : BasicType(Type::Kind::GenericFunction, n) {}
};

GenericFunctionType GenericFunction(Evaluation e, IrFunction const* fn);

}  // namespace ic::type

#endif  // ICARUS_TYPE_GENERIC_FUNCTION_H
