#include <utility>

#include "base/flyweight_set.h"
#include "base/meta.h"
#include "ir/instruction/arithmetic.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/core.h"
#include "ir/instruction/deserializer.h"
#include "ir/instruction/instructions.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/function_instructions.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/pointer_instructions.h"
#include "type/slice.h"
#include "type/struct.h"

namespace compiler {

namespace internal_instruction_set {

template <typename I>
ir::Inst DeserializeInstruction(ir::InstructionProto const &proto) {
  ir::InstructionDeserializer d(&proto.content());
  I result;
  if (not base::Deserialize(d, result)) { NOT_YET(); }
  return result;
}

template <typename... Is>
struct InstructionIndex {
  static void for_each(auto &&f) { (f(base::meta<Is>), ...); }

  static base::flyweight_set<base::MetaValue> const &index() { return kIndex; }

  static ir::Inst Deserialize(ir::InstructionProto const &proto) {
    auto const &[meta_value, deserializer] =
        kDeserializer.from_index(proto.identifier());
    return deserializer(proto);
  }

 private:
  static base::flyweight_map<
      base::MetaValue,
      base::any_invocable<ir::Inst(ir::InstructionProto const &)>>
      kDeserializer;
  static base::flyweight_set<base::MetaValue> kIndex;
};

template <typename... Is>
base::flyweight_set<base::MetaValue> InstructionIndex<Is...>::kIndex{
    base::meta<Is>...};

template <typename... Is>
base::flyweight_map<base::MetaValue,
                    base::any_invocable<ir::Inst(ir::InstructionProto const &)>>
    InstructionIndex<Is...>::kDeserializer = [] {
      base::flyweight_map<base::MetaValue, base::any_invocable<ir::Inst(
                                               ir::InstructionProto const &)>>
          result;
      (result.try_emplace(base::meta<Is>, DeserializeInstruction<Is>), ...);
      return result;
    }();

}  // namespace internal_instruction_set

struct InstructionSet
    : internal_instruction_set::InstructionIndex<
          // Core instructions
          ir::RegisterInstruction<bool>, ir::RegisterInstruction<ir::Char>,
          ir::RegisterInstruction<uint8_t>, ir::RegisterInstruction<int8_t>,
          ir::RegisterInstruction<uint16_t>, ir::RegisterInstruction<int16_t>,
          ir::RegisterInstruction<uint32_t>, ir::RegisterInstruction<int32_t>,
          ir::RegisterInstruction<uint64_t>, ir::RegisterInstruction<int64_t>,
          ir::RegisterInstruction<float>, ir::RegisterInstruction<double>,
          ir::RegisterInstruction<type::Type>,
          ir::RegisterInstruction<ir::addr_t>, ir::RegisterInstruction<ir::Fn>,
          ir::RegisterInstruction<ir::Scope>,
          ir::RegisterInstruction<ir::ModuleId>,
          ir::RegisterInstruction<ir::UnboundScope>,

          ir::StoreInstruction<bool>, ir::StoreInstruction<ir::Char>,
          ir::StoreInstruction<uint8_t>, ir::StoreInstruction<int8_t>,
          ir::StoreInstruction<uint16_t>, ir::StoreInstruction<int16_t>,
          ir::StoreInstruction<uint32_t>, ir::StoreInstruction<int32_t>,
          ir::StoreInstruction<uint64_t>, ir::StoreInstruction<int64_t>,
          ir::StoreInstruction<float>, ir::StoreInstruction<double>,
          ir::StoreInstruction<type::Type>, ir::StoreInstruction<ir::addr_t>,
          ir::StoreInstruction<ir::Fn>, ir::StoreInstruction<ir::Scope>,
          ir::StoreInstruction<ir::ModuleId>,
          ir::StoreInstruction<ir::UnboundScope>,

          ir::PhiInstruction<bool>, ir::PhiInstruction<ir::Char>,
          ir::PhiInstruction<uint8_t>, ir::PhiInstruction<int8_t>,
          ir::PhiInstruction<uint16_t>, ir::PhiInstruction<int16_t>,
          ir::PhiInstruction<uint32_t>, ir::PhiInstruction<int32_t>,
          ir::PhiInstruction<uint64_t>, ir::PhiInstruction<int64_t>,
          ir::PhiInstruction<float>, ir::PhiInstruction<double>,
          ir::PhiInstruction<type::Type>, ir::PhiInstruction<ir::addr_t>,
          ir::PhiInstruction<ir::Fn>, ir::PhiInstruction<ir::Scope>,
          ir::PhiInstruction<ir::ModuleId>,
          ir::PhiInstruction<ir::UnboundScope>,

          ir::LoadInstruction, ir::CallInstruction,

          // Arithmetic instructions
          ir::AddInstruction<ir::Integer>, ir::AddInstruction<uint8_t>,
          ir::AddInstruction<uint16_t>, ir::AddInstruction<uint32_t>,
          ir::AddInstruction<uint64_t>, ir::AddInstruction<int8_t>,
          ir::AddInstruction<int16_t>, ir::AddInstruction<int32_t>,
          ir::AddInstruction<int64_t>, ir::AddInstruction<float>,
          ir::AddInstruction<double>,

          ir::SubInstruction<ir::Integer>, ir::SubInstruction<uint8_t>,
          ir::SubInstruction<uint16_t>, ir::SubInstruction<uint32_t>,
          ir::SubInstruction<uint64_t>, ir::SubInstruction<int8_t>,
          ir::SubInstruction<int16_t>, ir::SubInstruction<int32_t>,
          ir::SubInstruction<int64_t>, ir::SubInstruction<float>,
          ir::SubInstruction<double>,

          ir::MulInstruction<ir::Integer>, ir::MulInstruction<uint8_t>,
          ir::MulInstruction<uint16_t>, ir::MulInstruction<uint32_t>,
          ir::MulInstruction<uint64_t>, ir::MulInstruction<int8_t>,
          ir::MulInstruction<int16_t>, ir::MulInstruction<int32_t>,
          ir::MulInstruction<int64_t>, ir::MulInstruction<float>,
          ir::MulInstruction<double>,

          ir::DivInstruction<ir::Integer>, ir::DivInstruction<uint8_t>,
          ir::DivInstruction<uint16_t>, ir::DivInstruction<uint32_t>,
          ir::DivInstruction<uint64_t>, ir::DivInstruction<int8_t>,
          ir::DivInstruction<int16_t>, ir::DivInstruction<int32_t>,
          ir::DivInstruction<int64_t>, ir::DivInstruction<float>,
          ir::DivInstruction<double>,

          ir::ModInstruction<ir::Integer>, ir::ModInstruction<uint8_t>,
          ir::ModInstruction<int8_t>, ir::ModInstruction<uint16_t>,
          ir::ModInstruction<int16_t>, ir::ModInstruction<uint32_t>,
          ir::ModInstruction<int32_t>, ir::ModInstruction<uint64_t>,
          ir::ModInstruction<int64_t>,

          ir::NegInstruction<ir::Integer>, ir::NegInstruction<int8_t>,
          ir::NegInstruction<int16_t>, ir::NegInstruction<int32_t>,
          ir::NegInstruction<int64_t>, ir::NegInstruction<float>,
          ir::NegInstruction<double>,

          // Equality comparison instructions
          ir::EqInstruction<ir::Integer>, ir::EqInstruction<uint8_t>,
          ir::EqInstruction<uint16_t>, ir::EqInstruction<uint32_t>,
          ir::EqInstruction<uint64_t>, ir::EqInstruction<int8_t>,
          ir::EqInstruction<int16_t>, ir::EqInstruction<int32_t>,
          ir::EqInstruction<int64_t>, ir::EqInstruction<float>,
          ir::EqInstruction<double>, ir::EqInstruction<type::Type>,
          ir::EqInstruction<ir::addr_t>, ir::EqInstruction<bool>,
          ir::EqInstruction<ir::Char>,

          ir::NeInstruction<ir::Integer>, ir::NeInstruction<uint8_t>,
          ir::NeInstruction<uint16_t>, ir::NeInstruction<uint32_t>,
          ir::NeInstruction<uint64_t>, ir::NeInstruction<int8_t>,
          ir::NeInstruction<int16_t>, ir::NeInstruction<int32_t>,
          ir::NeInstruction<int64_t>, ir::NeInstruction<float>,
          ir::NeInstruction<double>, ir::NeInstruction<type::Type>,
          ir::NeInstruction<ir::addr_t>, ir::NeInstruction<bool>,
          ir::NeInstruction<ir::Char>,

          // Order comparison instructions
          ir::LtInstruction<ir::Integer>, ir::LtInstruction<uint8_t>,
          ir::LtInstruction<uint16_t>, ir::LtInstruction<uint32_t>,
          ir::LtInstruction<uint64_t>, ir::LtInstruction<int8_t>,
          ir::LtInstruction<int16_t>, ir::LtInstruction<int32_t>,
          ir::LtInstruction<int64_t>, ir::LtInstruction<float>,
          ir::LtInstruction<double>, ir::LtInstruction<ir::addr_t>,
          ir::LtInstruction<ir::Char>,

          ir::LeInstruction<ir::Integer>, ir::LeInstruction<uint8_t>,
          ir::LeInstruction<uint16_t>, ir::LeInstruction<uint32_t>,
          ir::LeInstruction<uint64_t>, ir::LeInstruction<int8_t>,
          ir::LeInstruction<int16_t>, ir::LeInstruction<int32_t>,
          ir::LeInstruction<int64_t>, ir::LeInstruction<float>,
          ir::LeInstruction<double>, ir::LeInstruction<ir::addr_t>,
          ir::LeInstruction<ir::Char>,

          // Type construction instructions
          type::PtrInstruction, type::BufPtrInstruction,
          type::OpaqueTypeInstruction, type::FunctionTypeInstruction,
          type::SliceInstruction, type::StructDataInstruction,
          type::StructInstruction, type::AllocateStructInstruction,
          type::EnumInstruction, type::FlagsInstruction, type::ArrayInstruction,

          // Type access instructions
          type::SliceLengthInstruction, type::SliceDataInstruction,
          ir::StructIndexInstruction, ir::PtrIncrInstruction,

          // Builtin instructions
          AbortInstruction, AlignmentInstruction, AsciiEncodeInstruction,
          AsciiDecodeInstruction, BytesInstruction, HasBlockInstruction,

          // Cast instructions
          // u8 from any numeric primitive
          ir::CastInstruction<uint8_t(ir::Integer)>,
          ir::CastInstruction<uint8_t(int8_t)>,
          ir::CastInstruction<uint8_t(uint16_t)>,
          ir::CastInstruction<uint8_t(int16_t)>,
          ir::CastInstruction<uint8_t(uint32_t)>,
          ir::CastInstruction<uint8_t(int32_t)>,
          ir::CastInstruction<uint8_t(uint64_t)>,
          ir::CastInstruction<uint8_t(int64_t)>,
          ir::CastInstruction<uint8_t(float)>,
          ir::CastInstruction<uint8_t(double)>,
          ir::CastInstruction<uint8_t(bool)>,
          // i8 from any numeric primitive
          ir::CastInstruction<int8_t(ir::Integer)>,
          ir::CastInstruction<int8_t(uint8_t)>,
          ir::CastInstruction<int8_t(uint16_t)>,
          ir::CastInstruction<int8_t(int16_t)>,
          ir::CastInstruction<int8_t(uint32_t)>,
          ir::CastInstruction<int8_t(int32_t)>,
          ir::CastInstruction<int8_t(uint64_t)>,
          ir::CastInstruction<int8_t(int64_t)>,
          ir::CastInstruction<int8_t(float)>,
          ir::CastInstruction<int8_t(double)>,
          ir::CastInstruction<int8_t(bool)>,
          // u16 from any numeric primitive
          ir::CastInstruction<uint16_t(ir::Integer)>,
          ir::CastInstruction<uint16_t(uint8_t)>,
          ir::CastInstruction<uint16_t(int8_t)>,
          ir::CastInstruction<uint16_t(int16_t)>,
          ir::CastInstruction<uint16_t(uint32_t)>,
          ir::CastInstruction<uint16_t(int32_t)>,
          ir::CastInstruction<uint16_t(uint64_t)>,
          ir::CastInstruction<uint16_t(int64_t)>,
          ir::CastInstruction<uint16_t(float)>,
          ir::CastInstruction<uint16_t(double)>,
          ir::CastInstruction<uint16_t(bool)>,
          // i16 from any numeric primitive
          ir::CastInstruction<int16_t(ir::Integer)>,
          ir::CastInstruction<int16_t(uint8_t)>,
          ir::CastInstruction<int16_t(int8_t)>,
          ir::CastInstruction<int16_t(uint16_t)>,
          ir::CastInstruction<int16_t(uint32_t)>,
          ir::CastInstruction<int16_t(int32_t)>,
          ir::CastInstruction<int16_t(uint64_t)>,
          ir::CastInstruction<int16_t(int64_t)>,
          ir::CastInstruction<int16_t(float)>,
          ir::CastInstruction<int16_t(double)>,
          ir::CastInstruction<int16_t(bool)>,
          // u32 from any numeric primitive
          ir::CastInstruction<uint32_t(ir::Integer)>,
          ir::CastInstruction<uint32_t(uint8_t)>,
          ir::CastInstruction<uint32_t(int8_t)>,
          ir::CastInstruction<uint32_t(uint16_t)>,
          ir::CastInstruction<uint32_t(int16_t)>,
          ir::CastInstruction<uint32_t(int32_t)>,
          ir::CastInstruction<uint32_t(uint64_t)>,
          ir::CastInstruction<uint32_t(int64_t)>,
          ir::CastInstruction<uint32_t(float)>,
          ir::CastInstruction<uint32_t(double)>,
          ir::CastInstruction<uint32_t(bool)>,
          // i32 from any numeric primitive
          ir::CastInstruction<int32_t(ir::Integer)>,
          ir::CastInstruction<int32_t(uint8_t)>,
          ir::CastInstruction<int32_t(int8_t)>,
          ir::CastInstruction<int32_t(uint16_t)>,
          ir::CastInstruction<int32_t(int16_t)>,
          ir::CastInstruction<int32_t(uint32_t)>,
          ir::CastInstruction<int32_t(uint64_t)>,
          ir::CastInstruction<int32_t(int64_t)>,
          ir::CastInstruction<int32_t(float)>,
          ir::CastInstruction<int32_t(double)>,
          ir::CastInstruction<int32_t(bool)>,
          // u64 from any numeric primitive
          ir::CastInstruction<uint64_t(ir::Integer)>,
          ir::CastInstruction<uint64_t(uint8_t)>,
          ir::CastInstruction<uint64_t(int8_t)>,
          ir::CastInstruction<uint64_t(uint16_t)>,
          ir::CastInstruction<uint64_t(int16_t)>,
          ir::CastInstruction<uint64_t(uint32_t)>,
          ir::CastInstruction<uint64_t(int32_t)>,
          ir::CastInstruction<uint64_t(int64_t)>,
          ir::CastInstruction<uint64_t(float)>,
          ir::CastInstruction<uint64_t(double)>,
          ir::CastInstruction<uint64_t(bool)>,
          // i64 from any numeric primitive
          ir::CastInstruction<int64_t(ir::Integer)>,
          ir::CastInstruction<int64_t(uint8_t)>,
          ir::CastInstruction<int64_t(int8_t)>,
          ir::CastInstruction<int64_t(uint16_t)>,
          ir::CastInstruction<int64_t(int16_t)>,
          ir::CastInstruction<int64_t(uint32_t)>,
          ir::CastInstruction<int64_t(int32_t)>,
          ir::CastInstruction<int64_t(uint64_t)>,
          ir::CastInstruction<int64_t(float)>,
          ir::CastInstruction<int64_t(double)>,
          ir::CastInstruction<int64_t(bool)>,
          // f32 from regular numeric primitives
          ir::CastInstruction<float(uint8_t)>,
          ir::CastInstruction<float(int8_t)>,
          ir::CastInstruction<float(uint16_t)>,
          ir::CastInstruction<float(int16_t)>,
          ir::CastInstruction<float(uint32_t)>,
          ir::CastInstruction<float(int32_t)>,
          ir::CastInstruction<float(uint64_t)>,
          ir::CastInstruction<float(int64_t)>,
          ir::CastInstruction<float(ir::Integer)>,
          ir::CastInstruction<float(double)>,
          // f64 from regular numeric primitives
          ir::CastInstruction<double(uint8_t)>,
          ir::CastInstruction<double(int8_t)>,
          ir::CastInstruction<double(uint16_t)>,
          ir::CastInstruction<double(int16_t)>,
          ir::CastInstruction<double(uint32_t)>,
          ir::CastInstruction<double(int32_t)>,
          ir::CastInstruction<double(uint64_t)>,
          ir::CastInstruction<double(int64_t)>,
          ir::CastInstruction<double(ir::Integer)>,
          ir::CastInstruction<double(float)>,
          // integer from regular numeric primitives
          ir::CastInstruction<ir::Integer(uint8_t)>,
          ir::CastInstruction<ir::Integer(int8_t)>,
          ir::CastInstruction<ir::Integer(uint16_t)>,
          ir::CastInstruction<ir::Integer(int16_t)>,
          ir::CastInstruction<ir::Integer(uint32_t)>,
          ir::CastInstruction<ir::Integer(int32_t)>,
          ir::CastInstruction<ir::Integer(uint64_t)>,
          ir::CastInstruction<ir::Integer(int64_t)>,

          // Miscellaneous instructions
          ir::CompileTime<ir::Action::CopyInit, ir::Integer>,
          ir::CompileTime<ir::Action::MoveInit, ir::Integer>,
          ir::CompileTime<ir::Action::CopyAssign, ir::Integer>,
          ir::CompileTime<ir::Action::MoveAssign, ir::Integer>,

          ir::AndInstruction, ir::NotInstruction, type::XorFlagsInstruction,
          type::AndFlagsInstruction, type::OrFlagsInstruction,
          ir::LoadDataSymbolInstruction, ir::InitInstruction,
          ir::DestroyInstruction, ir::MoveInitInstruction,
          ir::CopyInitInstruction, ir::MoveInstruction, ir::CopyInstruction,
          ir::PtrDiffInstruction, ir::DebugIrInstruction,
          type::IsAFunctionInstruction> {};

}  // namespace compiler
