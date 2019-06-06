#ifndef ICARUS_OPT_COMBINE_PRINTS_H
#define ICARUS_OPT_COMBINE_PRINTS_H

namespace ir {
struct CompiledFn;
}  // namespace ir

namespace opt {

void CombinePrints(ir::CompiledFn* fn);

}  // namespace opt

#endif  // ICARUS_OPT_COMBINE_PRINTS_H
