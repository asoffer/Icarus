#include "ast/access.h"

#include "ast/declaration.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "misc/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"

namespace ast {
namespace {
using ::matcher::InheritsFrom;

}  // namespace

}  // namespace ast
