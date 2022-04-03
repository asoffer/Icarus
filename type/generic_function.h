#ifndef ICARUS_TYPE_GENERIC_FUNCTION_H
#define ICARUS_TYPE_GENERIC_FUNCTION_H

#include "ir/byte_code/byte_code.h"
#include "ir/interpreter/evaluate.h"
#include "ir/subroutine.h"
#include "ir/value/slice.h"
#include "type/function.h"
#include "type/qual_type.h"
#include "type/slice.h"
#include "type/type.h"

namespace type {

struct GenericFunction : LegacyType {
  explicit GenericFunction(ir::Subroutine subroutine, ir::ByteCode byte_code)
      : LegacyType(IndexOf<GenericFunction>(), {}),
        subroutine_(std::move(subroutine)),
        byte_code_(std::move(byte_code)) {}

  template <typename InstructionSet>
  Function const *Instantiate(absl::Span<Argument> arguments) {
    static Function const *fn_type =
        Func({core::AnonymousParameter(QualType::NonConstant(Slc(Argument_)))},
             {Type_});
    ir::NativeFn::Data data{
        .fn        = &subroutine_,
        .type      = fn_type,
        .byte_code = &byte_code_,
    };
    ir::CompleteResultBuffer argument_buffer;
    argument_buffer.append(ir::Slice(
        reinterpret_cast<ir::addr_t>(arguments.data()), arguments.size()));
    ir::CompleteResultBuffer result =
        interpreter::EvaluateToBuffer<InstructionSet>(ir::NativeFn(&data),
                                                      argument_buffer);
    // TODO: Diagnostics.
    return &result[0].get<Type>().as<Function>();
  }

 private:
   ir::Subroutine subroutine_;
 ir::ByteCode byte_code_;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_FUNCTION_H
