#include "opt/opt.h"

#include "ir/compiled_fn.h"

namespace opt {

void RunAllOptimizations(ir::CompiledFn *fn) {
  ReduceEmptyBlocks(fn);
  CombineBlocks(fn);
  RemoveTrivialFunctionCalls(fn);
}

}  // namespace opt
