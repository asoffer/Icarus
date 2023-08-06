#ifndef ICARUS_MODULE_SERIALIZE_H
#define ICARUS_MODULE_SERIALIZE_H

#include <deque>
#include <istream>
#include <optional>
#include <ostream>

#include "base/ptr_span.h"
#include "data_types/integer.h"
#include "module/global_function_map.h"
#include "module/global_index_map.h"
#include "module/module.h"
#include "module/module_map.h"
#include "module/symbol.h"
#include "semantic_analysis/type_system.h"
#include "serialization/foreign_symbol_map.h"
#include "serialization/module.pb.h"
#include "serialization/module_index.h"
#include "serialization/read_only_data.h"
#include "serialization/unique_type_table.h"
#include "vm/function.h"
#include "vm/function_table.h"

namespace module {

bool SerializeModule(Module const &module, std::ostream &output,
                     serialization::UniqueTypeTable const &unique_type_table,
                     ModuleMap &module_map, GlobalFunctionMap &function_map);

bool DeserializeModuleInto(serialization::Module const &proto,
                           base::PtrSpan<Module const> dependencies,
                           serialization::ModuleIndex module_index,
                           Module &module,
                           semantic_analysis::TypeSystem &current_type_system,
                           serialization::UniqueTypeTable &unique_type_table,
                           ModuleMap &module_map,
                           GlobalFunctionMap &function_map,
                           GlobalIndexMap &opaque_map);

}  // namespace module

#endif  // ICARUS_MODULE_SERIALIZE_H
