#ifndef ICARUS_SEMANTIC_ANALYSIS_COMPILED_MODULE_H
#define ICARUS_SEMANTIC_ANALYSIS_COMPILED_MODULE_H

#include <ostream>

#include "semantic_analysis/instruction_set.h"

namespace module {

struct Module {
  bool Serialize(std::ostream &output) const;

  semantic_analysis::IrFunction &initializer() { return initializer_; }
  semantic_analysis::IrFunction const &initializer() const {
    return initializer_;
  }

 private:
  // Accepts two arguments (a slice represented as data followed by length).
  semantic_analysis::IrFunction initializer_{2, 0};
};

}  // namespace module

#endif  // ICARUS_SEMANTIC_ANALYSIS_COMPILED_MODULE_H
