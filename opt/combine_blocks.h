#ifndef ICARUS_OPT_COMBINE_BLOCKS_H
#define ICARUS_OPT_COMBINE_BLOCKS_H

namespace ir {
struct CompiledFn;
}  // namespace ir

namespace opt {

void CombineBlocks(ir::CompiledFn* fn);

}  // namespace opt

#endif  // ICARUS_OPT_COMBINE_BLOCKS_H
