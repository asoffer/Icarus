#ifndef ICARUS_OPT_COMBINE_BLOCKS_H
#define ICARUS_OPT_COMBINE_BLOCKS_H

#include "ir/subroutine.h"

namespace opt {

void ReduceEmptyBlocks(ir::Subroutine* fn);
void CombineBlocks(ir::Subroutine* fn);

// TODO figure out where to organize this.
void RemoveTrivialFunctionCalls(ir::Subroutine* fn);

}  // namespace opt

#endif  // ICARUS_OPT_COMBINE_BLOCKS_H
