#ifndef ICARUS_IR_FOREIGN_FUNCTION_H
#define ICARUS_IR_FOREIGN_FUNCTION_H

#include <string>

#include "ir/function.h"
#include "type/type.h"

namespace ic {

IrFunction const* ForeignFunction(std::string const& name,
                                  type::FunctionType t);

}  // namespace ic

#endif  // ICARUS_IR_FOREIGN_FUNCTION_H
