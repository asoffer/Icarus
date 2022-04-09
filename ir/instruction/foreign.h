#ifndef ICARUS_IR_INSTRUCTION_FOREIGN_H
#define ICARUS_IR_INSTRUCTION_FOREIGN_H

#include <string>

#include "absl/status/statusor.h"

namespace ir {

absl::StatusOr<void *> LoadDataSymbol(std::string const &name);
absl::StatusOr<void (*)()> LoadFunctionSymbol(std::string const &name);

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_FOREIGN_H
