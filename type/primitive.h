#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "type/basic_type.h"
#include "type/type.h"

namespace type {

struct Primitive : public Type {
 public:
  TYPE_FNS(Primitive);
  Primitive(BasicType pt) : type_(pt) {}

  void ExtractDefiningModules(absl::flat_hash_set<module::BasicModule const *>
                                  *modules) const override {
    return module::ExtractDefiningModules::Extract(this, modules);
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  bool TestEquality(void const *lhs, void const *rhs) const override;

  bool is_integral() const;

  BasicType type_;
};

}  // namespace type
#endif  // ICARUS_TYPE_PRIMITIVE_H
