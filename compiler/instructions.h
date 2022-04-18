#ifndef ICARUS_COMPILER_INSTRUCTIONS_H
#define ICARUS_COMPILER_INSTRUCTIONS_H

#include "base/no_destructor.h"
#include "base/untyped_buffer.h"
#include "ir/byte_code/byte_code.h"
#include "ir/module.h"
#include "ir/value/addr.h"
#include "ir/value/char.h"
#include "ir/value/generic_fn.h"
#include "ir/value/module_id.h"
#include "ir/value/reg.h"
#include "ir/value/result_buffer.h"
#include "ir/value/scope.h"
#include "type/array.h"
#include "type/block.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/scope.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace internal_instructions {

ir::CompleteResultBuffer EvaluateAtCompileTimeToBufferImpl(
    module::SharedContext const &shared_context,
    module::Module::FunctionInformation const &info,
    ir::CompleteResultBuffer const &);

}  // namespace internal_instructions

void InterpretAtCompileTime(module::SharedContext const &shared_context,
                            ir::Fn f,
                            ir::CompleteResultBuffer const &arguments = {});
void InterpretAtCompileTime(module::SharedContext const &shared_context,
                            ir::Subroutine const &fn,
                            ir::CompleteResultBuffer const &arguments = {});

std::vector<ir::Block> InterpretScopeAtCompileTime(
    module::SharedContext const &shared_context, ir::Scope s,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments);
ir::ByteCode EmitByteCode(ir::Subroutine const &sr);

template <typename... Args>
ir::CompleteResultBuffer EvaluateAtCompileTimeToBuffer(
    module::SharedContext const &shared_context,
    module::Module::FunctionInformation const &info, Args const &... args) {
  ir::CompleteResultBuffer buffer;
  (buffer.append(args), ...);
  return internal_instructions::EvaluateAtCompileTimeToBufferImpl(
      shared_context, info, buffer);
}

template <typename... Args>
ir::CompleteResultBuffer EvaluateAtCompileTimeToBuffer(
    module::SharedContext const &shared_context, ir::Fn f,
    Args const &... args) {
  return EvaluateAtCompileTimeToBuffer(shared_context,
                                       shared_context.Function(f), args...);
}

namespace internal_type {
template <typename T>
bool Compare(::type::Type t) {
  if constexpr (base::meta<T> == base::meta<type::Enum::underlying_type>) {
    if (t.is<type::Enum>()) { return true; }
  }

  if constexpr (base::meta<T> == base::meta<type::Flags::underlying_type>) {
    if (t.is<type::Flags>()) { return true; }
  }

  if constexpr (base::meta<T> == base::meta<bool>) {
    return t == ::type::Bool;
  } else if constexpr (base::meta<T> == base::meta<ir::Char>) {
    return t == ::type::Char;
  } else if constexpr (base::meta<T> == base::meta<ir::Integer>) {
    return t == ::type::Integer;
  } else if constexpr (base::meta<T> == base::meta<int8_t>) {
    return t == ::type::I8;
  } else if constexpr (base::meta<T> == base::meta<int16_t>) {
    return t == ::type::I16;
  } else if constexpr (base::meta<T> == base::meta<int32_t>) {
    return t == ::type::I32;
  } else if constexpr (base::meta<T> == base::meta<int64_t>) {
    return t == ::type::I64;
  } else if constexpr (base::meta<T> == base::meta<uint8_t>) {
    return t == ::type::U8;
  } else if constexpr (base::meta<T> == base::meta<uint16_t>) {
    return t == ::type::U16;
  } else if constexpr (base::meta<T> == base::meta<uint32_t>) {
    return t == ::type::U32;
  } else if constexpr (base::meta<T> == base::meta<uint64_t>) {
    return t == ::type::U64;
  } else if constexpr (base::meta<T> == base::meta<float>) {
    return t == ::type::F32;
  } else if constexpr (base::meta<T> == base::meta<double>) {
    return t == ::type::F64;
  } else if constexpr (base::meta<T> == base::meta<::type::Type>) {
    return t == ::type::Type_;
  } else if constexpr (base::meta<T> == base::meta<::type::Struct const *>) {
    return t.is<::type::Struct>();
  } else if constexpr (base::meta<T> == base::meta<ir::addr_t>) {
    return t.is<::type::Pointer>() or t == type::NullPtr or t.is<type::Slice>();
  } else if constexpr (base::meta<T> == base::meta<ir::Scope>) {
    return t == ::type::Type(::type::Scp({}));
  } else if constexpr (base::meta<T> == base::meta<::type::Struct const *>) {
    return t.is<::type::Struct>();
  } else if constexpr (base::meta<T> == base::meta<ir::Fn>) {
    return t.is<::type::Function>();
  } else if constexpr (base::meta<T> == base::meta<ir::Slice>) {
    return t.is<::type::Slice>();
  } else if constexpr (base::meta<T> == base::meta<ir::GenericFn>) {
    return t.is<::type::Generic<type::Function>>() or
           t.is<::type::Generic<type::Struct>>();
  } else if constexpr (base::meta<T> == base::meta<ir::ModuleId>) {
    return t == ::type::Module;
  } else if constexpr (base::meta<T> == base::meta<ir::ScopeContext>) {
    return t == type::ScopeContext;
  } else if constexpr (base::meta<T> == base::meta<ir::UnboundScope>) {
    return t == type::UnboundScope or t.is<type::Scope>();
  } else if constexpr (base::meta<T> == base::meta<ir::Block>) {
    return t.is<::type::Block>();
  } else {
    UNREACHABLE(t.to_string(), " vs ", typeid(T).name());
  }
}
}  // namespace internal_type

template <typename... Ts, typename Fn>
auto ApplyTypes(type::Type t, Fn &&fn) {
  using return_type = decltype(fn.template operator()<base::first_t<Ts...>>());

  // Create a static array of funtions that may be called depending on which
  // type matches.
  std::array<return_type (*)(Fn &&), sizeof...(Ts)> kFnToCall{
      [](Fn &&f) { return f.template operator()<Ts>(); }...};

  // Using fold expressions, take the disjunction of
  // `internal_type::Compare<T>(t)` over all T. This will compute this boolean
  // value until it returns true. However, in each folded expression, we
  // actually use the comma operator to first increment `index`, which means
  // that `index` will be incremented the number until
  // `internal_type::Compare<T>(t)` returns true. This means that after the
  // computation, the value of `index` is one more than the array index for the
  // function we want to call.
  size_t index = 0;
  bool found   = ((++index, ::compiler::internal_type::Compare<Ts>(t)) or ...);
  ASSERT(found == true);

  return kFnToCall[index - 1](std::forward<Fn>(fn));
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_INSTRUCTIONS_H
