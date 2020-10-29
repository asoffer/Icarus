#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

// TODO: Currently inserting these always at the root. There are a couple of
// reason why this is wrong. First, If this is generated due to a temporary
// subcontext, we probably want to drop it, as we can't verify the constructed
// type is valid in any way. Second, For types like arrays of primitives, we
// only need to generate the code for them once, rather than per module.

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Array> const &r) {
  auto [fn, inserted] = context().root().InsertInit(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn.get())) {
      builder().CurrentBlock() = fn->entry();
      builder().OnEachArrayElement(r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
        EmitDefaultInit(type::Typed<ir::Reg>(reg, r.type()->data_type()));
      });
      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }

  builder().Init(r);
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Array> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  auto [fn, inserted] = context().root().InsertDestroy(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = fn->entry();
      builder().OnEachArrayElement(r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
        EmitDestroy(type::Typed<ir::Reg>(reg, r.type()->data_type()));
      });
      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  builder().Destroy(r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Flags> const &r) {
  builder().Store(ir::FlagsVal{0}, *r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Pointer> const &r) {
  builder().Store(ir::Addr::Null(), *r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Primitive> const &r) {
  r.type()->Apply([&]<typename T>() { builder().Store(T{}, *r); });
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Struct> const &r) {
  auto [fn, inserted] = context().root().InsertInit(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn.get())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i = 0; i < r.type()->fields().size(); ++i) {
        auto &field = r.type()->fields()[i];
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
    // TODO: Remove this hack.
    const_cast<type::Struct *>(r.type())->init_ = fn;
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  builder().Init(r);
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Struct> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  auto [fn, inserted] = context().root().InsertDestroy(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn.get())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i = 0; i < r.type()->fields().size(); ++i) {
        EmitDestroy(
            type::Typed<ir::Reg>(builder().Field(var, r.type(), i).get(),
                                 r.type()->fields()[i].type));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  // TODO: Remove this hack.
  const_cast<type::Struct *>(r.type())->SetDestructor(fn);
  builder().Destroy(r);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Tuple> const &r) {
  auto [fn, inserted] = context().root().InsertInit(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn.get())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();

      for (size_t i = 0; i < r.type()->size(); ++i) {
        EmitDefaultInit(type::Typed<ir::Reg>(
            builder().Field(ir::Reg::Arg(0), r.type(), i).get(),
            r.type()->entries()[i]));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  builder().Init(r);
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Tuple> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  auto [fn, inserted] = context().root().InsertDestroy(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn.get())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();

      for (size_t i = 0; i < r.type()->size(); ++i) {
        EmitDestroy(type::Typed<ir::Reg>(
            builder().Field(ir::Reg::Arg(0), r.type(), i).get(),
            r.type()->entries()[i]));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  builder().Destroy(r);
}

}  // namespace compiler
