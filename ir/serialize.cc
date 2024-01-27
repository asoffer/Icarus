#include "ir/serialize.h"

#include "common/resources.h"
#include "ir/foreign_function.h"
#include "ir/module.h"
#include "ir/function.h"
#include "jasmin/serialize/serialize.h"
#include "jasmin/serialize/string_writer.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/serialize.h"

namespace ic {

void Serializer::Serialize(Module& module, ModuleProto& proto) {
  for (auto const& m : resources.module_map) { *proto.add_modules() = m; }
  SerializeTypeSystem(*proto.mutable_type_system());

  jasmin::StringWriter writer(global_program.functions(),
                              *proto.mutable_program());
  jasmin::Serialize(global_program, writer);

  for (auto const& s : resources.strings) { *proto.add_string_literals() = s; }
  for (auto const& [name_and_type, unused_value] : AllForeignFunctions()) {
    auto const& [name, type] = name_and_type;
    auto& f                  = *proto.add_foreign_functions();
    f.set_name(name);
    type::SerializeFunctionType(type, *f.mutable_type());
  }

  for (auto const& [name_and_type, unused_value] : AllForeignPointers()) {
    auto const& [name, type] = name_and_type;
    auto& p                  = *proto.add_foreign_pointers();
    p.set_name(name);
    type::Serialize(type.pointee(), *p.mutable_pointee());
  }

  for (auto const& [id, entry] : module.entries()) {
    (*proto.mutable_identifiers())[id.value()] = std::string_view(id);
    auto& symbol = (*proto.mutable_exported_symbols())[id.value()];
    type::Serialize(entry.qualified_type.type(), *symbol.mutable_type());
    for (jasmin::Value v : entry.value) {
      uint64_t raw_value;
      if (entry.qualified_type.type().kind() == type::Type::Kind::Function) {
        raw_value =
            global_function_registry.id(v.as<IrFunction const*>()).value();
      } else if (entry.qualified_type.type().kind() == type::Type::Kind::Pointer) {
        raw_value = global_pointer_registry.id(v.as<void*>());
      } else {
        raw_value = v.raw_value();
      }
      symbol.add_content(raw_value);
    }
  }
}

}  // namespace ic
