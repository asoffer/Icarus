#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/store.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "ir/results.h"
#include "type/primitive.h"

namespace compiler {

void Compiler::Visit(type::Array const *t, ir::Reg reg, EmitDefaultInitTag) {
  t->init_func_.init([=]() {
    // TODO special function?
    auto const *fn_type = type::Func({type::Ptr(t)}, {});
    auto *fn            = AddFunc(fn_type, fn_type->AnonymousFnParams());
    ir::OnEachArrayElement(t, fn, [=](ir::Reg r) {
      Visit(t->data_type, r, EmitDefaultInitTag{});
    });
    return fn;
  });

  ir::Init(t, reg);
}

void Compiler::Visit(type::Flags const *t, ir::Reg reg, EmitDefaultInitTag) {
  ir::Store(ir::FlagsVal{0}, reg);
}

void Compiler::Visit(type::Pointer const *t, ir::Reg reg, EmitDefaultInitTag) {
  ir::Store(ir::Addr::Null(), reg);
}

void Compiler::Visit(type::Primitive const *t, ir::Reg reg,
                     EmitDefaultInitTag) {
  switch (t->type_) {
    case type::BasicType::Type_: ir::Store(type::Void(), reg); break;
    case type::BasicType::NullPtr: UNREACHABLE();
    case type::BasicType::EmptyArray: UNREACHABLE();
    case type::BasicType::Bool: ir::Store(false, reg); break;
    case type::BasicType::Int8: ir::Store(static_cast<int8_t>(0), reg); break;
    case type::BasicType::Int16: ir::Store(static_cast<int16_t>(0), reg); break;
    case type::BasicType::Int32: ir::Store(static_cast<int32_t>(0), reg); break;
    case type::BasicType::Int64: ir::Store(static_cast<int64_t>(0), reg); break;
    case type::BasicType::Nat8: ir::Store(static_cast<uint8_t>(0), reg); break;
    case type::BasicType::Nat16:
      ir::Store(static_cast<uint16_t>(0), reg);
      break;
    case type::BasicType::Nat32:
      ir::Store(static_cast<uint32_t>(0), reg);
      break;
    case type::BasicType::Nat64:
      ir::Store(static_cast<uint64_t>(0), reg);
      break;
    case type::BasicType::Float32: ir::Store(0.0f, reg); break;
    case type::BasicType::Float64: ir::Store(0.0, reg); break;
    default: UNREACHABLE();
  }
}

void Compiler::Visit(type::Struct const *t, ir::Reg reg, EmitDefaultInitTag) {
  ir::Init(t, reg);
}

void Compiler::Visit(type::Tuple const *t, ir::Reg reg, EmitDefaultInitTag) {
  t->init_func_.init([=]() {
    auto const *fn_type = type::Func({type::Ptr(t)}, {});
    auto *fn            = AddFunc(fn_type, fn_type->AnonymousFnParams());

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        Visit(t->entries_.at(i), ir::Field(var, t, i).get(),
              EmitDefaultInitTag{});
      }

      builder().ReturnJump();
    }
    return fn;
  });

  ir::Init(t, reg);
}

}  // namespace compiler
