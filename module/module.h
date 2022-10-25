#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <istream>
#include <optional>
#include <ostream>

#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"

namespace module {

struct Module {
  bool Serialize(std::ostream &output) const;
  static std::optional<Module> Deserialize(std::istream &input);

  semantic_analysis::IrFunction &initializer() { return initializer_; }
  semantic_analysis::IrFunction const &initializer() const {
    return initializer_;
  }

  semantic_analysis::TypeSystem &type_system() { return type_system_; }
  semantic_analysis::TypeSystem &type_system() const { return type_system_; }

 private:
  // Accepts two arguments (a slice represented as data followed by length).
  semantic_analysis::IrFunction initializer_{2, 0};

  // The type-system containing all types referenceable in this module.
  mutable semantic_analysis::TypeSystem type_system_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
