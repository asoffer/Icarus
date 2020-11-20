#ifndef ICARUS_IR_INSTRUCTION_BASE_H
#define ICARUS_IR_INSTRUCTION_BASE_H

#include <concepts>
#include <string>

#include "base/cast.h"
#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "ir/value/reg.h"

// TODO rename this file so that when you forget that for dependency reasons you
// should try to include this instead of instructions.h, you reach for this one
// anyway.
namespace ir {

struct InstructionInliner;
struct ByteCodeWriter;

// clang-format off
template <typename T>
concept Instruction = std::copy_constructible<T> and 
                      std::assignable_from<T&, T const&> and
                      std::assignable_from<T&, T&&> and 
                      std::destructible<T> and
                      requires(T t) {
  { t.WriteByteCode(std::declval<ByteCodeWriter*>()) } -> std::same_as<void>;
  { t.Inline(std::declval<InstructionInliner const&>()) } -> std::same_as<void>;
};

template <typename T>
concept ReturningInstruction = Instruction<T> and requires (T t) {
  { t.result } -> std::same_as<Reg>;
};

template <typename T>
concept VoidReturningInstruction = Instruction<T> and not ReturningInstruction<T>;
// clang-format on

struct InstructionVTable {
  void* (*copy_construct)(void const*) = [](void const*) -> void* {
    return nullptr;
  };
  void* (*move_construct)(void*) = [](void*) -> void* { return nullptr; };
  void (*copy_assign)(void const*, void*) = [](void const*, void*) {};
  void (*move_assign)(void*, void*)       = [](void*, void*) {};
  void (*destroy)(void*)                  = [](void*) {};

  void (*WriteByteCode)(void*, ByteCodeWriter*) = [](void*, ByteCodeWriter*) {
    UNREACHABLE("WriteByteCode is unimplemented");
  };

  std::string (*to_string)(void const*) = [](void const*) -> std::string {
    UNREACHABLE("to_string is unimplemented");
  };

  void (*Inline)(void*, InstructionInliner const&) =
      [](void*, InstructionInliner const&) {
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

    .WriteByteCode =
        [](void* self, ByteCodeWriter* writer) {
          reinterpret_cast<T*>(self)->WriteByteCode(writer);
        },

    .to_string =
        [](void const* self) {
          return reinterpret_cast<T const*>(self)->to_string();
        },

    .Inline =
        [](void* self, InstructionInliner const& inliner) {
          return reinterpret_cast<T*>(self)->Inline(inliner);
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

  void WriteByteCode(ByteCodeWriter* writer) const {
    vtable_->WriteByteCode(data_, writer);
  }

  std::string to_string() const { return vtable_->to_string(data_); }

  void Inline(InstructionInliner const& inliner) {
    vtable_->Inline(data_, inliner);
  }

  base::MetaValue rtti() const { return vtable_->rtti; }

  Inst* operator->() { return this; }
  Inst const* operator->() const { return this; }

  Inst& operator*() { return *this; }
  Inst const& operator*() const { return *this; }

 private:
  void* data_;
  InstructionVTable const* vtable_;
};

}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTION_BASE_H
