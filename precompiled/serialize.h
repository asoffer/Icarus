#ifndef ICARUS_PRECOMPILED_SERIALIZE_H
#define ICARUS_PRECOMPILED_SERIALIZE_H

#include "ir/value/result_buffer.h"
#include "module/module.h"
#include "precompiled/module.pb.h"
#include "precompiled/value.pb.h"
#include "type/system.h"
#include "type/type.h"

namespace precompiled {

// Serializes the value of type `t` held in `ref`  into `out`.
void SerializeValue(type::TypeSystem const& system, type::Type t,
                    ir::CompleteResultRef ref, Value& value);

ir::CompleteResultBuffer DeserializeValue(type::TypeSystem const& system,
                                          Value const& value);

TypeSystem ToProto(type::TypeSystem const& system);
void FromProto(TypeSystem const& proto, type::TypeSystem& system);

SymbolInformation ToProto(type::TypeSystem const& system,
                          module::Module::SymbolInformation const& info);

}  // namespace precompiled

#endif  // ICARUS_PRECOMPILED_SERIALIZE_H
