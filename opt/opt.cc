#include "opt/opt.h"

#include "ir/subroutine.h"

namespace opt {

void RunAllOptimizations(ir::Subroutine *fn) {
  ReduceEmptyBlocks(fn);
  CombineBlocks(fn);
  RemoveTrivialFunctionCalls(fn);
}

}  // namespace opt
