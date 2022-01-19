#ifndef ICARUS_OPT_OPT_H
#define ICARUS_OPT_OPT_H

#include "opt/combine_blocks.h"

namespace opt {

void RunAllOptimizations(ir::Subroutine *fn);

}  // namespace opt

#endif  // ICARUS_OPT_OPT_H
