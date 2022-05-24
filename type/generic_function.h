#ifndef ICARUS_TYPE_GENERIC_FUNCTION_H
#define ICARUS_TYPE_GENERIC_FUNCTION_H

#include "ir/byte_code/byte_code.h"
#include "ir/interpreter/interpreter.h"
#include "ir/subroutine.h"
#include "ir/value/slice.h"
#include "type/function.h"
#include "type/qual_type.h"
#include "type/slice.h"
#include "type/type.h"

namespace type {

struct GenericFunction : LegacyType {
  explicit GenericFunction(ir::ByteCode byte_code)
      : LegacyType(IndexOf<GenericFunction>(), {}) {}

  Function const *Instantiate(absl::Span<Argument> arguments) {
    static Function const *fn_type =
        Func({core::AnonymousParameter(QualType::NonConstant(Slc(Argument_)))},
             {Type_});
    ir::CompleteResultBuffer argument_buffer;
    argument_buffer.append(ir::Slice(
        reinterpret_cast<ir::addr_t>(arguments.data()), arguments.size()));
    NOT_YET("Interpret");
    // TODO: Diagnostics.
    // return &result[0].get<Type>().as<Function>();
  }

 private:
  ir::NativeFunctionInformation info_;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_FUNCTION_H
