#ifndef ICARUS_OPT_COMBINE_BLOCKS_H
#define ICARUS_OPT_COMBINE_BLOCKS_H

namespace ir {
struct CompiledFn;
}  // namespace ir

namespace opt {

void ReduceEmptyBlocks(ir::CompiledFn* fn);
void CombineBlocks(ir::CompiledFn* fn);

// TODO figure out where to organize this.
void RemoveTrivialFunctionCalls(ir::CompiledFn* fn);

}  // namespace opt

#endif  // ICARUS_OPT_COMBINE_BLOCKS_H
