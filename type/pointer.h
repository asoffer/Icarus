#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "absl/container/flat_hash_map.h"
#include "common/constant/entry.h"
#include "type/type.h"

namespace ic::type {

struct PointerType : Type {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, PointerType ptr) {
    p.write("*");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend PointerType Ptr(Type t);

  explicit PointerType() = default;
  explicit constexpr PointerType(uint32_t n) : Type(Type::Kind::Pointer, n) {}
};

PointerType Ptr(Type t);

absl::flat_hash_map<Type, Constant> const& Pointers();
absl::flat_hash_map<Type, Constant> const& BufferPointers();
absl::flat_hash_map<Type, Constant> const& Slices();

struct BufferPointerType : Type {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, BufferPointerType ptr) {
    p.write("[*]");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend BufferPointerType BufPtr(Type t);

  explicit BufferPointerType() = default;
  explicit constexpr BufferPointerType(uint32_t n)
      : Type(Type::Kind::BufferPointer, n) {}
};

BufferPointerType BufPtr(Type t);

struct SliceType : Type {
  Type element_type() const;

  friend void NthPrint(auto& p, auto& f, SliceType s) {
    p.write("\\");
    f(p, s.element_type());
  }

 private:
  friend Type;
  friend SliceType Slice(Type t);

  explicit SliceType() = default;
  explicit constexpr SliceType(uint32_t n) : Type(Type::Kind::Slice, n) {}
};

SliceType Slice(Type t);

}  // namespace ic::type

#endif  // ICARUS_TYPE_POINTER_H
