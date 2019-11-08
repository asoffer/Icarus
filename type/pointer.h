#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type/type.h"

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(Type const *t) : pointee(t) {}

  void ExtractDefiningModules(absl::flat_hash_set<module::BasicModule const *>
                                  *modules) const override {
    return module::ExtractDefiningModules::Extract(this, modules);
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Type const *pointee;
};

// Like Pointer but allows indexing and pointer arithmetic.
struct BufferPointer : public Pointer {
  BufferPointer() = delete;

  void ExtractDefiningModules(absl::flat_hash_set<module::BasicModule const *>
                                  *modules) const override {
    return module::ExtractDefiningModules::Extract(this, modules);
  }

  void WriteTo(std::string *result) const override;
  BufferPointer(Type const *t) : Pointer(t) {}
};

Pointer const *Ptr(Type const *t);
BufferPointer const *BufPtr(Type const *t);
}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
