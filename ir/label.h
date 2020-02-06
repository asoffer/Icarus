#ifndef ICARUS_IR_LABEL_H
#define ICARUS_IR_LABEL_H

#include <string_view>
#include "base/strong_types.h"

namespace ir {

ICARUS_BASE_DEFINE_STRONG_TYPE(Label, std::string_view(""), base::EnableHashing);

}  // namespace ir

#endif  // ICARUS_IR_LABEL_H
