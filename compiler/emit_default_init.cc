#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/lazy_convert.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/value/value.h"
#include "type/primitive.h"

namespace compiler {

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Array> const &r) {
  context().init_.emplace(
      r.type(), base::lazy_convert{[&] {
        auto const *fn_type =
            type::Func(core::Params<type::QualType>{core::AnonymousParam(
                           type::QualType::NonConstant(type::Ptr(r.type())))},
                       {});
        ir::NativeFn fn =
            AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
              return type::Typed<ast::Declaration const *>(nullptr, q.type());
            }));
        ICARUS_SCOPE(ir::SetCurrent(fn)) {
          builder().CurrentBlock() = fn->entry();
          builder().OnEachArrayElement(
              r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
                EmitDefaultInit(
                    type::Typed<ir::Reg>(reg, r.type()->data_type()));
              });
          builder().ReturnJump();
        }
        fn->WriteByteCode<interpretter::instruction_set_t>();
        return fn;
      }});

  builder().Init(r.type(), *r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Flags> const &r) {
  builder().Store(ir::FlagsVal{0}, *r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Pointer> const &r) {
  builder().Store(ir::Addr::Null(), *r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Primitive> const &r) {
  r.type()->Apply([&](auto tag) {
    using T = typename decltype(tag)::type;
    builder().Store(T{}, *r);
  });
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Struct> const &r) {
  r.type()->init_func_.init([=]() {
    type::Pointer const *pt = type::Ptr(r.type());
    auto const *fn_type     = type::Func(
        core::Params<type::QualType>{
            core::AnonymousParam(type::QualType::NonConstant(pt))},
        {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i = 0; i < r.type()->fields_.size(); ++i) {
        auto &field = r.type()->fields_[i];
        if (not field.initial_value.empty()) {
          if (field.type == type::Int64) {
            EmitCopyInit(
                type::Typed<ir::Value>(field.initial_value, field.type),
                builder().Field(var, r.type(), i));
          } else {
            NOT_YET();
          }
        } else {
          EmitDefaultInit(type::Typed<ir::Reg>(
              builder().Field(var, r.type(), i).get(), field.type));
        }
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
    return fn;
  });

  builder().Init(r.type(), *r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Tuple> const &r) {
  r.type()->init_func_.init([=]() {
    auto const *fn_type =
        type::Func(core::Params<type::QualType>{core::AnonymousParam(
                       type::QualType::NonConstant(type::Ptr(r.type())))},
                   {});

    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i : base::make_random_permutation(
               absl::BitGen{}, r.type()->entries_.size())) {
        EmitDefaultInit(type::Typed<ir::Reg>(
            builder().Field(var, r.type(), i).get(), r.type()->entries_.at(i)));
      }

      builder().ReturnJump();
    }
    return fn;
  });
  builder().Init(r.type(), *r);
}

}  // namespace compiler
