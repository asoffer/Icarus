#include "ir/function.h"

#include <ffi.h>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/container/node_hash_map.h"
#include "absl/strings/str_cat.h"
#include "common/constant/manifest.h"
#include "common/foreign_function.h"
#include "common/interface.h"
#include "common/pattern.h"
#include "common/slice.h"
#include "ir/function_id.h"
#include "ir/program_arguments.h"
#include "jasmin/core/program_fragment.h"
#include "jasmin/core/value.h"
#include "nth/utility/no_destructor.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ic {
namespace {

[[maybe_unused]] ffi_type* FfiType(type::Type t) {
  if (t == type::Char) {
    return std::is_signed_v<char> ? &ffi_type_schar : &ffi_type_uchar;
  }
  if (t == type::I8) { return &ffi_type_sint8; }
  if (t == type::I16) { return &ffi_type_sint16; }
  if (t == type::I32) { return &ffi_type_sint32; }
  if (t == type::I64) { return &ffi_type_sint64; }
  if (t == type::U8) { return &ffi_type_uint8; }
  if (t == type::U16) { return &ffi_type_uint16; }
  if (t == type::U32) { return &ffi_type_uint32; }
  if (t == type::U64) { return &ffi_type_uint64; }
  if (t == type::F32) { return &ffi_type_float; }
  if (t == type::F64) { return &ffi_type_double; }
  return &ffi_type_pointer;
}

template <typename T>
T Read(ffi_arg const& value) {
  T result;
  std::memcpy(&result, &value, sizeof(T));
  return result;
}

}  // namespace

void LoadProgramArguments::execute(
    jasmin::Input<> in, jasmin::Output<std::byte const*, uint64_t> out) {
  Slice arguments = ProgramArguments();
  out.set(arguments.data(), arguments.count());
}

void RegisterForeignFunction::consume(
    jasmin::Input<char const*, size_t, type::Type> in,
    jasmin::Output<void const*> out) {
  auto [data, length, t] = in;
  if (t.kind() == type::Type::Kind::Function) {
    ForeignFunction f(std::string_view(data, length),
                      t.as<type::FunctionType>());
    size_t params = 0;

    // TODO: Come up with a better mangling
    std::string mangled_name(f.name());
    for (auto const& pt : t.as<type::FunctionType>().parameters().types()) {
      params += type::JasminSize(pt);
      absl::StrAppend(&mangled_name, "_", static_cast<int>(pt.kind()), ".",
                      pt.index());
    }
    size_t rets = 0;
    mangled_name.append("_");
    for (type::Type ret : t.as<type::FunctionType>().returns()) {
      rets += type::JasminSize(ret);
      absl::StrAppend(&mangled_name, "_", static_cast<int>(ret.kind()), ".",
                      ret.index());
    }
    auto& fn =
        shared_context.foreign.declare(mangled_name, params, rets).function;
    out.set(&fn);
  } else {
    NTH_UNIMPLEMENTED();
    out.set(nullptr);
    //   InsertForeignPointer(std::string_view(data, length),
    //   t.as<type::PointerType>()));
  }
}

void InvokeForeignFunction::consume(std::span<jasmin::Value> input,
                                    std::span<jasmin::Value> output,
                                    type::FunctionType type,
                                    VoidConstPtr fn_ptr) {
  auto returns = type.returns();
  // TODO: There's no need to allocate this, but lifetimes are tricky and
  // index-based iterators are annoying to implement so I'm not doing it yet.
  std::vector<type::Type> parameter_types = type.parameters().types();
  auto parameter_iter                     = parameter_types.begin();
  NTH_REQUIRE((v.debug), returns.size() <= 1);

  ffi_cif call_interface;

  ffi_type* return_type =
      returns.empty() ? &ffi_type_void : FfiType(returns[0]);
  std::vector<ffi_type*> argument_types;
  std::vector<void*> argument_values;
  argument_types.reserve(parameter_types.size());
  argument_values.reserve(parameter_types.size());

  for (auto iter = input.begin(); iter != input.end();
       ++iter, ++parameter_iter) {
    argument_types.push_back(FfiType(*parameter_iter));
    argument_values.push_back(static_cast<void*>(&*iter));
  }

  ffi_status status =
      ffi_prep_cif(&call_interface, FFI_DEFAULT_ABI, argument_types.size(),
                   return_type, argument_types.data());
  NTH_REQUIRE(status == FFI_OK);

  ffi_arg return_value;
  ffi_call(&call_interface,
           reinterpret_cast<void (*)()>(const_cast<void*>(fn_ptr.ptr())),
           &return_value, argument_values.data());

  if (not returns.empty()) {
    type::Type t = returns[0];
    if (t == type::Char) {
      output[0] = Read<char>(return_value);
    } else if (t == type::I8) {
      output[0] = Read<int8_t>(return_value);
    } else if (t == type::I16) {
      output[0] = Read<int16_t>(return_value);
    } else if (t == type::I32) {
      output[0] = Read<int32_t>(return_value);
    } else if (t == type::I64) {
      output[0] = Read<int64_t>(return_value);
    } else if (t == type::U8) {
      output[0] = Read<uint8_t>(return_value);
    } else if (t == type::U16) {
      output[0] = Read<uint16_t>(return_value);
    } else if (t == type::U32) {
      output[0] = Read<uint32_t>(return_value);
    } else if (t == type::U64) {
      output[0] = Read<uint64_t>(return_value);
    } else if (t == type::F32) {
      output[0] = Read<float>(return_value);
    } else if (t == type::F64) {
      output[0] = Read<double>(return_value);
    } else {
      output[0] = Read<char const*>(return_value);
    }
  }
}

void ConstructFunctionType::consume(jasmin::Input<type::Type, type::Type> in,
                                    jasmin::Output<type::Type> out) {
  auto [parameter, return_type] = in;
  std::vector<type::Type> returns;
  if (return_type != type::Bottom) { returns.push_back(return_type); }
  if (parameter.kind() == type::Type::Kind::Parameters) {
    out.set(type::Function(parameter.as<type::ParametersType>(),
                           std::move(returns)));
  } else {
    out.set(type::Function(
        type::Parameters({{.name = Identifier(""), .type = parameter}}),
        std::move(returns)));
  }
}

absl::node_hash_map<Interface, IrFunction> satisfaction;
absl::flat_hash_map<type::Type, absl::flat_hash_set<Interface>> extensions;

void CheckInterfaceSatisfaction::consume(jasmin::Input<type::Type> in,
                                         jasmin::Output<bool> out,
                                         Interface intf) {
  auto iter = extensions.find(in.get<0>());
  if (iter == extensions.end()) { out.set(false); }
  out.set(iter->second.contains(intf));
}

void Extend(Interface intf, type::Type t) { extensions[t].insert(intf); }

IrFunction SatisfiesInterface(Interface intf) {
  IrFunction f(1, 1);
  f.append<CheckInterfaceSatisfaction>(intf);
  f.append<jasmin::Return>();
  return f;
}

void ConstructInterface::consume(std::span<jasmin::Value> inputs,
                                 std::span<jasmin::Value> outputs) {
  Interface intf(Interface::construct_new);
  auto [iter, inserted] = satisfaction.emplace(intf, SatisfiesInterface(intf));
  outputs[0]            = intf;  // Pattern(&iter->second);
}

}  // namespace ic
