#include "precompiled/module.h"

#include "core/parameters.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/generic_function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace precompiled {
namespace {

struct TypeSystemSerializingVisitor {
  using signature = void(TypeDefinition& out);

  explicit TypeSystemSerializingVisitor(type::TypeSystem const* system)
      : system_(*ASSERT_NOT_NULL(system)) {}

  void operator()(type::Type t, TypeDefinition& out) { t.visit(*this, out); }

  void operator()(auto const* t, TypeDefinition& out) { Visit(t, out); }

 private:
  void Visit(type::Primitive const* p, TypeDefinition& out) {
    out.set_primitive(static_cast<int>(p->kind()));
  }

  void Visit(type::Array const* a, TypeDefinition& out) { ; }
  void Visit(type::Pointer const* p, TypeDefinition& out) {
    out.set_pointer(system_.index(p->pointee()));
  }
  void Visit(type::BufferPointer const* p, TypeDefinition& out) {
    out.set_buffer_pointer(system_.index(p->pointee()));
  }
  void Visit(type::Function const* f, TypeDefinition& out) {
    auto& fn = *out.mutable_function();
    fn.set_eager(f->eager());
    for (auto const& param : f->params()) {
      auto& p = *fn.add_parameter();
      p.set_name(param.name);
      p.set_type(system_.index(param.value.type()));
      p.set_flags((param.flags.value() << uint8_t{8}) |
                  param.value.quals().value());
    }
    for (type::Type t : f->return_types()) {
      fn.add_return_type(system_.index(t));
    }
  }
  void Visit(type::Slice const* s, TypeDefinition& out) {
    out.set_slice(system_.index(s->data_type()));
  }
  void Visit(auto const* s, TypeDefinition& out) { NOT_YET(); }

  type::TypeSystem const& system_;
};

}  // namespace

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>>
PrecompiledModule::Make(std::string const& file_content,
                        module::SharedContext& context) {
  ModuleProto module_proto;

  if (not module_proto.ParseFromString(file_content)) {
    return absl::InvalidArgumentError(
        "Failed to deserialize module identifier.");
  }

  std::string identifier = module_proto.identifier();

  auto [id, m] = context.module_table().add_module<PrecompiledModule>(
      std::move(identifier), std::move(module_proto));

  return std::pair<ir::ModuleId, PrecompiledModule const*>(id, m);
}

absl::Span<module::Module::SymbolInformation const> PrecompiledModule::Symbols(
    std::string_view name) const {
  auto const& symbols = proto_.symbols();
  auto iter           = symbols.find(name);
  if (iter == symbols.end()) { return {}; }
  // TODO:
  // return iter->second;
  return {};
}

TypeSystem ToProto(type::TypeSystem const& system) {
  TypeSystem proto;
  TypeSystemSerializingVisitor v(&system);
  for (type::Type t : system.types()) { v(t, *proto.add_type()); }
  return proto;
}

void FromProto(TypeSystem& proto, type::TypeSystem& system) {
  for (auto const& t : proto.type()) {
    switch (t.type_case()) {
      case TypeDefinition::kPrimitive:
        system.insert(
            MakePrimitive(static_cast<type::Primitive::Kind>(t.primitive())));
        break;
      case TypeDefinition::kPointer:
        system.insert(Ptr(system.from_index(t.pointer())));
        break;
      case TypeDefinition::kBufferPointer:
        system.insert(BufPtr(system.from_index(t.buffer_pointer())));
        break;
      case TypeDefinition::kFunction: {
        core::Parameters<type::QualType> parameters;
        for (auto const& p : t.function().parameter()) {
          parameters.append(
              p.name(),
              type::QualType(system.from_index(p.type()),
                             type::Quals::FromValue(p.flags() & 0xff)),
              core::ParameterFlags::FromValue(p.flags() >> uint8_t{8}));
        }
        std::vector<type::Type> return_types;
        return_types.reserve(t.function().return_type().size());
        for (int64_t n : t.function().return_type()) {
          return_types.push_back(system.from_index(n));
        }

        auto* make_func = (t.function().eager() ? type::EagerFunc : type::Func);
        system.insert(
            make_func(std::move(parameters), std::move(return_types)));
      } break;
      case TypeDefinition::kSlice:
        system.insert(Slc(system.from_index(t.slice())));
        break;
      case TypeDefinition::TYPE_NOT_SET: UNREACHABLE();
    }
  }
}

}  // namespace precompiled
