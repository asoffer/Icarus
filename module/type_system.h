#ifndef ICARUS_MODULE_TYPE_SYSTEM_H
#define ICARUS_MODULE_TYPE_SYSTEM_H

#include "semantic_analysis/type_system.h"
#include "serialization/type_system.pb.h"
#include "serialization/unique_type_table.h"

namespace module {

void SerializeTypeSystem(
    semantic_analysis::TypeSystem& type_system,
    serialization::UniqueTypeTable const& unique_type_table,
    serialization::TypeSystem& proto);

}  // namespace module

#endif  // ICARUS_MODULE_TYPE_SYSTEM_H
