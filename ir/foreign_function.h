#ifndef ICARUS_IR_FOREIGN_FUNCTION_H
#define ICARUS_IR_FOREIGN_FUNCTION_H

#include <deque>
#include <string>
#include <string_view>
#include <utility>

#include "ir/function.h"
#include "ir/function_id.h"
#include "nth/container/flyweight_map.h"
#include "type/function.h"

namespace ic {

IrFunction const& InsertForeignFunction(StringLiteral str, type::FunctionType t,
                                        bool implement);

void* InsertForeignPointer(std::string_view name, type::PointerType t);

}  // namespace ic

#endif  // ICARUS_IR_FOREIGN_FUNCTION_H
