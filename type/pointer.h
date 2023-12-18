#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include <cstdint>

#include "type/basic.h"

namespace ic::type {

struct PointerType : internal_type::BasicType {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, PointerType ptr) {
    p.write("*");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend PointerType Ptr(Type);

  explicit PointerType() = default;
  explicit constexpr PointerType(uint64_t n)
      : BasicType(Type::Kind::Pointer, n) {}
};

PointerType Ptr(Type t);

struct BufferPointerType : internal_type::BasicType {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, BufferPointerType ptr) {
    p.write("[*]");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend BufferPointerType BufPtr(Type);

  explicit BufferPointerType() = default;
  explicit constexpr BufferPointerType(uint64_t n)
      : BasicType(Type::Kind::BufferPointer, n) {}
};

BufferPointerType BufPtr(Type t);

struct SliceType : internal_type::BasicType {
  Type element_type() const;

  friend void NthPrint(auto& p, auto& f, SliceType s) {
    p.write("\\");
    f(p, s.element_type());
  }

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto& );
  friend void DeserializeTypeSystem(TypeSystemProto const& );
  friend SliceType Slice(Type);

  explicit SliceType() = default;
  explicit constexpr SliceType(uint64_t n) : BasicType(Type::Kind::Slice, n) {}
};

SliceType Slice(Type t);

}  // namespace ic::type

#endif // ICARUS_TYPE_POINTER_H
