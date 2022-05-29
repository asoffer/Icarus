#ifndef ICARUS_IR_INSTRUCTION_BASE_H
#define ICARUS_IR_INSTRUCTION_BASE_H

#include <concepts>
#include <string>

#include "base/cast.h"
#include "base/meta.h"
#include "base/serialize.h"
#include "base/traverse.h"
#include "base/untyped_buffer.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/serializer.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/value/reg.h"

// TODO rename this file so that when you forget that for dependency reasons you
// should try to include this instead of instructions.h, you reach for this one
// anyway.
namespace ir {
namespace interpreter {
struct Interpreter;
}  // namespace interpreter

template <typename T>
concept Inlinable = base::TraversableBy<T, Inliner>;

template <typename T>
concept Instruction = (std::copyable<T> and std::destructible<T> and
                       Inlinable<T>);

// clang-format off
template <typename T>
concept ReturningInstruction = Instruction<T> and requires (T t) {
  { t.result } -> std::same_as<Reg>;
};
// clang-format on

template <typename T>
concept VoidReturningInstruction = (Instruction<T> and
                                    not ReturningInstruction<T>);

struct InstructionVTable {
  void* (*copy_construct)(void const*) = [](void const*) -> void* {
    return nullptr;
  };
  void* (*move_construct)(void*) = [](void*) -> void* { return nullptr; };
  void (*copy_assign)(void const*, void*) = [](void const*, void*) {};
  void (*move_assign)(void*, void*)       = [](void*, void*) {};
  void (*destroy)(void*)                  = [](void*) {};

  bool (*Interpret)(interpreter::Interpreter&, void const*) =
      [](interpreter::Interpreter&, void const*) { return false; };

  void (*ToProto)(void const*, InstructionSerializer&) =
      [](void const*, InstructionSerializer&) {
        UNREACHABLE("ToProto is unimplemented");
      };

  std::string (*to_string)(void const*) = [](void const*) -> std::string {
    UNREACHABLE("to_string is unimplemented");
  };

  void (*Inline)(void*, Inliner&) = [](void*, Inliner&) {
    UNREACHABLE("Inline is unimplemented");
  };

  base::MetaValue rtti;
};
inline constexpr InstructionVTable DefaultInstructionVTable;

template <typename T>
InstructionVTable InstructionVTableFor{
    .copy_construct = [](void const* from) -> void* {
      return new T(*reinterpret_cast<T const*>(from));
    },
    .move_construct = [](void* from) -> void* {
      return new T(std::move(*reinterpret_cast<T*>(from)));
    },
    .copy_assign =
        [](void const* from, void* to) {
          *reinterpret_cast<T*>(to) = *reinterpret_cast<T const*>(from);
        },
    .move_assign =
        [](void* from, void* to) {
          *reinterpret_cast<T*>(to) = std::move(*reinterpret_cast<T*>(from));
        },
    .destroy = [](void* self) { delete reinterpret_cast<T*>(self); },

    .Interpret = [](interpreter::Interpreter& interpreter,
                    void const* self) -> bool {
      if constexpr (requires {
                      InterpretInstruction(interpreter,
                                           std::declval<T const&>());
                    }) {
        return InterpretInstruction(interpreter,
                                    *reinterpret_cast<T const*>(self));
      } else {
        UNREACHABLE(typeid(T).name());
        return false;
      }
    },

    .ToProto =
        [](void const* self, InstructionSerializer& serializer) {
          serializer.SetIdentifier<T>();
          base::Serialize(serializer, *reinterpret_cast<T const*>(self));
        },

    .to_string =
        [](void const* self) {
          return reinterpret_cast<T const*>(self)->to_string();
        },

    .Inline =
        [](void* self, Inliner& inliner) {
          base::Traverse(inliner, *reinterpret_cast<T*>(self));
        },
    .rtti = base::meta<T>,
};

struct Inst {
  template <typename T>
  Inst(T&& inst) noexcept
      : data_(new std::decay_t<T>(std::forward<T>(inst))),
        vtable_(&InstructionVTableFor<std::decay_t<T>>) {}

  Inst(Inst const& inst) noexcept
      : data_(inst.vtable_->copy_construct(inst.data_)),
        vtable_(inst.vtable_) {}

  Inst(Inst&& inst) noexcept
      : data_(std::exchange(inst.data_, nullptr)),
        vtable_(std::exchange(inst.vtable_, &DefaultInstructionVTable)) {}

  Inst& operator=(Inst const& inst) noexcept {
    if (vtable_ == inst.vtable_) {
      vtable_->copy_assign(inst.data_, data_);
    } else {
      vtable_->destroy(data_);
      vtable_ = inst.vtable_;
      data_   = vtable_->copy_construct(inst.data_);
    }
    return *this;
  }

  Inst& operator=(Inst&& inst) noexcept {
    if (vtable_ == inst.vtable_) {
      vtable_->move_assign(inst.data_, data_);
    } else {
      vtable_->destroy(data_);
      vtable_ = inst.vtable_;
      data_   = vtable_->move_construct(inst.data_);
    }
    return *this;
  }

  // TODO: Probably better to just have a no-op instruction.
  Inst& operator=(std::nullptr_t) {
    vtable_->destroy(data_);
    data_   = nullptr;
    vtable_ = &DefaultInstructionVTable;
    return *this;
  }

  // Returns `false` if this `Inst` is in it's moved-from state.
  operator bool() const { return data_ != nullptr; }

  template <typename T>
  T* if_as() {
    if (vtable_ == &InstructionVTableFor<T>) {
      return reinterpret_cast<T*>(data_);
    } else {
      return nullptr;
    }
  }

  template <typename T>
  T const* if_as() const {
    if (vtable_ == &InstructionVTableFor<T>) {
      return reinterpret_cast<T const*>(data_);
    } else {
      return nullptr;
    }
  }

  template <typename T>
  T& as() {
    ASSERT(vtable_ == &InstructionVTableFor<T>);
    return *reinterpret_cast<T*>(data_);
  }

  template <typename T>
  T const& as() const {
    ASSERT(vtable_ == &InstructionVTableFor<T>);
    return *reinterpret_cast<T const*>(data_);
  }

  ~Inst() { vtable_->destroy(data_); }

  friend void BaseSerialize(InstructionSerializer& serializer, Inst const& i) {
    i.vtable_->ToProto(i.data_, serializer);
  }

  friend void BaseTraverse(Inliner& inliner, Inst& inst) {
    inst.vtable_->Inline(inst.data_, inliner);
  }

  std::string to_string() const { return vtable_->to_string(data_); }

  base::MetaValue rtti() const { return vtable_->rtti; }

  Inst* operator->() { return this; }
  Inst const* operator->() const { return this; }

  Inst& operator*() { return *this; }
  Inst const& operator*() const { return *this; }

 private:
  friend bool InterpretInstruction(interpreter::Interpreter&,
                                   std::same_as<Inst> auto const&);

  void* data_;
  InstructionVTable const* vtable_;
};

inline bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                 std::same_as<Inst> auto const& inst) {
  return inst.vtable_->Interpret(interpreter, inst.data_);
}

}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTION_BASE_H
