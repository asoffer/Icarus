#include "type/struct.h"

#include "core/arch.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/hashtag.h"
#include "module/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

Struct::Struct(ir::ModuleId mod, Struct::Options options)
    : LegacyType(IndexOf<Struct>(),
                 LegacyType::Flags{.is_default_initializable = 1,
                                   .is_copyable    = options.is_copyable,
                                   .is_movable     = options.is_movable,
                                   .has_destructor = 0}),
      mod_(mod) {}

void Struct::AppendConstants(std::vector<Struct::Field> constants) {
  constants_ = std::move(constants);
  size_t i   = 0;
  for (auto const &constant : constants_) {
    ASSERT(constant.type.valid() == true);
    field_indices_.emplace(constant.name, i++);
  }
}

void Struct::AppendFields(std::vector<Struct::Field> fields) {
  completeness_ = Completeness::DataComplete;
  fields_       = std::move(fields);
  size_t i      = 0;
  for (auto const &field : fields_) {
    ASSERT(field.type.valid() == true);
    field_indices_.emplace(field.name, i++);
    flags_.is_default_initializable &=
        field.type.get()->IsDefaultInitializable() or
        not field.initial_value.empty();
    flags_.is_copyable &= field.type.get()->IsCopyable();
    flags_.is_movable &= field.type.get()->IsMovable();
    flags_.has_destructor |= field.type.get()->HasDestructor();
  }
}

void Struct::SetInits(absl::Span<std::pair<ir::Fn, Type> const> move_inits,
                      absl::Span<std::pair<ir::Fn, Type> const> copy_inits) {
  for (auto [init, t] : move_inits) {
    core::Parameters<QualType> const &params = t.as<Function>().parameters();
    ASSERT(params.size() == 1u);
    move_inits_.emplace(params[0].value.type(), init);
  }
  for (auto [init, t] : copy_inits) {
    core::Parameters<QualType> const &params = t.as<Function>().parameters();
    ASSERT(params.size() == 1u);
    copy_inits_.emplace(params[0].value.type(), init);

    // If no move is explicitly specified for this assigment type, use the copy
    // instead.
    move_inits_.try_emplace(params[0].value.type(), init);
  }
}

void Struct::SetDestructor(ir::Fn dtor) {
  flags_.has_destructor = true;
  dtor_                 = dtor;
}

ir::Fn Struct::Destructor() const {
  ASSERT(dtor_.has_value() == true);
  return *dtor_;
}

void Struct::SetAssignments(
    absl::Span<std::pair<ir::Fn, Type> const> move_assignments,
    absl::Span<std::pair<ir::Fn, Type> const> copy_assignments) {
  for (auto [assignment, t] : move_assignments) {
    core::Parameters<QualType> const &params = t.as<Function>().parameters();
    ASSERT(params.size() == 2u);
    move_assignments_.emplace(params[1].value.type(), assignment);
  }
  for (auto [assignment, t] : copy_assignments) {
    core::Parameters<QualType> const &params = t.as<Function>().parameters();
    ASSERT(params.size() == 2u);
    copy_assignments_.emplace(params[1].value.type(), assignment);

    // If no move is explicitly specified for this assigment type, use the copy
    // instead.
    move_assignments_.try_emplace(params[1].value.type(), assignment);
  }
}

ir::Fn const *Struct::MoveInit(type::Type from_type) const {
  auto iter = move_inits_.find(type::Ptr(from_type));
  if (iter == move_inits_.end()) { return nullptr; }
  return &iter->second;
}

ir::Fn const *Struct::CopyInit(type::Type from_type) const {
  auto iter = copy_inits_.find(type::Ptr(from_type));
  if (iter == copy_inits_.end()) { return nullptr; }
  return &iter->second;
}

ir::Fn const *Struct::MoveAssignment(type::Type from_type) const {
  auto iter = move_assignments_.find(type::Ptr(from_type));
  if (iter == move_assignments_.end()) { return nullptr; }
  return &iter->second;
}

ir::Fn const *Struct::CopyAssignment(type::Type from_type) const {
  auto iter = copy_assignments_.find(type::Ptr(from_type));
  if (iter == copy_assignments_.end()) { return nullptr; }
  return &iter->second;
}

core::Bytes Struct::offset(size_t field_num, core::Arch const &a) const {
  ASSERT(completeness_ >= Completeness::DataComplete);
  auto offset = core::Bytes{0};
  for (size_t i = 0; i < field_num; ++i) {
    offset += fields_.at(i).type.bytes(a);
    offset = core::FwdAlign(offset, fields_.at(i + 1).type.alignment(a));
  }
  return offset;
}

size_t Struct::index(std::string_view name) const {
  return field_indices_.find(name)->second;
}

Struct::Field const *Struct::field(std::string_view name) const {
  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}

Struct::Field const *Struct::constant(std::string_view name) const {
  for (auto &constant : constants_) {
    if (constant.name == name) { return &constant; }
  }
  return nullptr;
}

void Struct::WriteTo(std::string *result) const {
  result->append("struct.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

core::Bytes Struct::bytes(core::Arch const &a) const {
  ASSERT(completeness_ >= Completeness::DataComplete);
  auto num_bytes = core::Bytes{0};
  for (auto const &field : fields_) {
    num_bytes += field.type.bytes(a);
    // TODO it'd be in the (common, I think) case where you want both, it would
    // be faster to compute bytes and alignment simultaneously.
    num_bytes = core::FwdAlign(num_bytes, field.type.alignment(a));
  }

  return num_bytes;
}

core::Alignment Struct::alignment(core::Arch const &a) const {
  ASSERT(completeness_ >= Completeness::DataComplete);
  auto align = core::Alignment{1};
  for (auto const &field : fields_) {
    align = std::max(align, field.type.alignment(a));
  }
  return align;
}

bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                          StructDataInstruction const &inst) {
  std::vector<Struct::Field> struct_fields;
  struct_fields.reserve(inst.fields.size());
  for (auto const &field : inst.fields) {
    absl::flat_hash_set<ir::Hashtag> tags;
    if (field.exported()) { tags.insert(ir::Hashtag::Export); }

    if (ir::CompleteResultRef value = field.initial_value();
        not value.empty()) {
      // TODO: field.type() can be null. If the field type is inferred from the
      // initial value.
      Type t  = interpreter.frame().resolve(field.type());
      auto &f = struct_fields.emplace_back(
          Struct::Field{.name     = std::string(field.name()),
                        .type     = t,
                        .hashtags = std::move(tags)});
      f.initial_value.append(value);
    } else {
      struct_fields.push_back(
          Struct::Field{.name     = std::string(field.name()),
                        .type     = interpreter.frame().resolve(field.type()),
                        .hashtags = std::move(tags)});
    }
  }

  Struct &s = const_cast<Struct &>(
      interpreter.frame().resolve(inst.struct_).as<Struct>());
  s.AppendFields(std::move(struct_fields));
  s.data_complete();
  return true;
}

bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                          StructInstruction const &inst) {
  std::vector<Struct::Field> constant_fields;
  constant_fields.reserve(inst.constants.size());
  for (auto const &constant : inst.constants) {
    absl::flat_hash_set<ir::Hashtag> tags;
    if (constant.exported()) { tags.insert(ir::Hashtag::Export); }

    if (ir::CompleteResultRef value = constant.initial_value();
        not value.empty()) {
      // TODO: constant.type() can be null. If the type is inferred from the
      // initial value.
      Type t  = interpreter.frame().resolve(constant.type());
      auto &f = constant_fields.emplace_back(
          Struct::Field{.name     = std::string(constant.name()),
                        .type     = t,
                        .hashtags = std::move(tags)});
      f.initial_value.append(value);
    } else {
      constant_fields.push_back(
          Struct::Field{.name = std::string(constant.name()),
                        .type = interpreter.frame().resolve(constant.type()),
                        .hashtags = std::move(tags)});
    }
  }

  Struct &s = const_cast<Struct &>(
      interpreter.frame().resolve(inst.struct_).as<Struct>());
  s.AppendConstants(std::move(constant_fields));
  s.SetInits(inst.move_inits, inst.copy_inits);
  s.SetAssignments(inst.move_assignments, inst.copy_assignments);
  if (inst.dtor) { s.SetDestructor(*inst.dtor); }
  s.complete();
  return true;
}

}  // namespace type
