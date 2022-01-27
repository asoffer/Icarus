#include "module/module.h"

namespace type {

// Returns a pointer to the module which defines this type (or null if the type
// is constructed from entirely built-in types and type-constructors).
module::Module const *Provenance(Type t);

}  // namespace type
