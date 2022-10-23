#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <istream>
#include <optional>
#include <ostream>

#include "semantic_analysis/instruction_set.h"

namespace module {

struct Module {
  bool Serialize(std::ostream &output) const;
  static std::optional<Module> Deserialize(std::istream &input);

  semantic_analysis::IrFunction &initializer() { return initializer_; }
  semantic_analysis::IrFunction const &initializer() const {
    return initializer_;
  }

 private:
  // Accepts two arguments (a slice represented as data followed by length).
  semantic_analysis::IrFunction initializer_{2, 0};
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
