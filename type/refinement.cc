#include "type/refinement.h"

namespace ic::type {

Type RefinementType::underlying() const {
  NTH_UNIMPLEMENTED();
  // return Type(Type::from_index, constants->constants_[index()].value());
}


bool RefinementType::operator()(AnyValue const &v) const {
  NTH_UNIMPLEMENTED();
  // auto const& pattern = constants->constants_[index() + 1];
  // return pattern(v);
}

RefinementType Refinement(Type t, ::ic::Pattern p) {
  NTH_UNIMPLEMENTED();
  //   return RefinementType(type_system->refinements.index(
  //       type_system->refinements.insert({t, p}).first));
}

}  // namespace ic::type
