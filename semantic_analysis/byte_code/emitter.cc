#include "semantic_analysis/byte_code/emitter.h"

#include "vm/argument_slice.h"

namespace semantic_analysis {

std::span<std::byte const> EmitterBase::EvaluateConstant(
    ast::Expression const *expr, QualifiedType qt) {
  NTH_ASSERT(qt == context().qualified_type(expr));
  auto [result_ptr, inserted] = context().insert_constant(expr);
  if (inserted) {
    // TODO: Integers are an annoying special case at the moment.
    core::TypeContour contour = ContourOf(qt.type(), type_system());
    if (PassInRegister(contour)) {
      vm::Function f(0, contour.bytes().value() / jasmin::ValueSize);

      // This `variable_offsets` map is intentionally empty. There will never
      // be declarations from which data needs to be loaded. Because
      // `EvaluateConstant` is only to be called on constant expressions, any
      // identifier will refer to a declaration that is constant, and so
      // lookup will happen by loading the value directly rather than adding
      // instructions which load at runtime.
      nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;

      as<ByteCodeValueEmitter>().Emit(expr, FunctionData(f, variable_offsets));
      f.AppendReturn();

      vm::ArgumentSlice argument_slice(nullptr, 0);
      data_types::IntegerTable table;
      jasmin::ValueStack value_stack;
      vm::Execute(f, vm::ExecutionState{table, type_system(), argument_slice},
                  value_stack);
      result_ptr->resize(contour.bytes().value());
      std::byte *data = result_ptr->data();
      for (std::byte *ptr = data + result_ptr->size() - jasmin::ValueSize;
           ptr >= data; ptr -= jasmin::ValueSize) {
        jasmin::Value::Store(value_stack.pop_value(), ptr, jasmin::ValueSize);
      }
      return *result_ptr;
    } else {
      NTH_UNIMPLEMENTED("{}") <<= {DebugQualifiedType(qt, type_system())};
    }
  } else {
    return *result_ptr;
  }
}

}  // namespace semantic_analysis
