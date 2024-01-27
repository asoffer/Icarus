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

IrFunction const* ForeignFunction(std::string const& name,
                                  type::FunctionType t);

nth::flyweight_map<std::pair<size_t, type::FunctionType>,
                   std::pair<type::FunctionType, IrFunction const*>> const&
AllForeignFunctions();

std::pair<type::FunctionType, IrFunction const*> const& LookupForeignFunction(
    LocalFunctionId id);

IrFunction const& InsertForeignFunction(std::string_view name,
                                        type::FunctionType t, bool implement);

size_t ForeignFunctionIndex(std::string_view name, type::FunctionType t);

void* InsertForeignPointer(std::string_view name, type::PointerType t);

nth::flyweight_map<std::pair<size_t, type::PointerType>,
                   std::pair<type::PointerType, void*>> const&
AllForeignPointers();

size_t ForeignPointerIndex(std::string_view name, type::PointerType t);

std::pair<type::PointerType, void*> const& LookupForeignPointer(size_t index);

}  // namespace ic

#endif  // ICARUS_IR_FOREIGN_FUNCTION_H
