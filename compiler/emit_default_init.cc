#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/lazy_convert.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/results.h"
#include "type/primitive.h"

namespace compiler {

void Compiler::Visit(type::Array const *t, ir::Reg reg, EmitDefaultInitTag) {
  data_.init_.emplace(
      t, base::lazy_convert{[&] {
        auto const *fn_type = type::Func(
            core::Params<type::Type const *>{
                core::AnonymousParam(type::Ptr(t))},
            {});
        ir::NativeFn fn = AddFunc(
            fn_type, fn_type->params().Transform([](type::Type const *p) {
              return type::Typed<ast::Declaration const *>(nullptr, p);
            }));
        ICARUS_SCOPE(ir::SetCurrent(fn)) {
          builder().CurrentBlock() = fn->entry();
          builder().OnEachArrayElement(t, ir::Reg::Arg(0), [=](ir::Reg r) {
            Visit(t->data_type(), r, EmitDefaultInitTag{});
          });
          builder().ReturnJump();
        }
        fn->WriteByteCode();
        return fn;
      }});

  builder().Init(t, reg);
}

void Compiler::Visit(type::Flags const *t, ir::Reg reg, EmitDefaultInitTag) {
  builder().Store(ir::FlagsVal{0}, reg);
}

void Compiler::Visit(type::Pointer const *t, ir::Reg reg, EmitDefaultInitTag) {
  builder().Store(ir::Addr::Null(), reg);
}

void Compiler::Visit(type::Primitive const *t, ir::Reg reg,
                     EmitDefaultInitTag) {
  switch (t->type_) {
    case type::BasicType::Type_: builder().Store(type::Void(), reg); break;
    case type::BasicType::NullPtr: UNREACHABLE();
    case type::BasicType::EmptyArray: UNREACHABLE();
    case type::BasicType::Bool: builder().Store(false, reg); break;
    case type::BasicType::Int8: builder().Store(int8_t{0}, reg); break;
    case type::BasicType::Int16: builder().Store(int16_t{0}, reg); break;
    case type::BasicType::Int32: builder().Store(int32_t{0}, reg); break;
    case type::BasicType::Int64: builder().Store(int64_t{0}, reg); break;
    case type::BasicType::Nat8: builder().Store(uint8_t{0}, reg); break;
    case type::BasicType::Nat16: builder().Store(uint16_t{0}, reg); break;
    case type::BasicType::Nat32: builder().Store(uint32_t{0}, reg); break;
    case type::BasicType::Nat64: builder().Store(uint64_t{0}, reg); break;
    case type::BasicType::Float32: builder().Store(float{0}, reg); break;
    case type::BasicType::Float64: builder().Store(double{0}, reg); break;
    default: UNREACHABLE();
  }
}

void Compiler::Visit(type::Struct const *t, ir::Reg reg, EmitDefaultInitTag) {
  t->init_func_.init([=]() {
    type::Pointer const *pt = type::Ptr(t);
    auto const *fn_type     = type::Func(
        core::Params<type::Type const *>{core::AnonymousParam(pt)}, {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::Type const *p) {
          return type::Typed<ast::Declaration const *>(nullptr, p);
        }));

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i = 0; i < t->fields_.size(); ++i) {
        auto &field = t->fields_[i];
        if (field.initial_value) {
          if (field.type == type::Int64) {
            EmitCopyInit(field.type,
                         ir::Results{field.initial_value->get<int64_t>()},
                         builder().Field(var, t, i));
          } else {
            NOT_YET();
          }
        } else {
          Visit(field.type, builder().Field(var, t, i).get(),
                EmitDefaultInitTag{});
        }
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode();
    return fn;
  });

  builder().Init(t, reg);
}

void Compiler::Visit(type::Tuple const *t, ir::Reg reg, EmitDefaultInitTag) {
  t->init_func_.init([=]() {
    auto const *fn_type = type::Func(
        core::Params<type::Type const *>{core::AnonymousParam(type::Ptr(t))},
        {});

    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::Type const *p) {
          return type::Typed<ast::Declaration const *>(nullptr, p);
        }));

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        Visit(t->entries_.at(i), builder().Field(var, t, i).get(),
              EmitDefaultInitTag{});
      }

      builder().ReturnJump();
    }
    return fn;
  });
  builder().Init(t, reg);
}

}  // namespace compiler
