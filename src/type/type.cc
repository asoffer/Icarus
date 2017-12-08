#include "type.h"
#include "../scope.h"

std::string Primitive::to_string() const {
  switch (type_) {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  case PrimType::EnumName:                                                     \
    return #name;
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO
  default: UNREACHABLE();
  }
}

// TODO better to hash pair of Array*
static std::unordered_map<Array *, std::unordered_map<Array *, IR::Func *>>
    eq_funcs;
static std::unordered_map<Array *, std::unordered_map<Array *, IR::Func *>>
    ne_funcs;
IR::Val Array::Compare(Array *lhs_type, IR::Val lhs_ir, Array *rhs_type,
                       IR::Val rhs_ir, bool equality) {
  auto &funcs = equality ? eq_funcs : ne_funcs;

  auto insertion = funcs[lhs_type].emplace(rhs_type, nullptr);
  if (insertion.second) {
    std::vector<std::pair<std::string, AST::Expression *>> args = {
        {"lhs", nullptr}, {"rhs", nullptr}};
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func({Ptr(lhs_type), Ptr(rhs_type)}, Bool), std::move(args)));
    auto *fn = insertion.first->second = IR::Func::All.back().get();
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();

      auto lhs_len = lhs_type->fixed_length
                         ? IR::Val::Uint(lhs_type->len)
                         : IR::Load(IR::ArrayLength(fn->Argument(0)));

      auto rhs_len = rhs_type->fixed_length
                         ? IR::Val::Uint(rhs_type->len)
                         : IR::Load(IR::ArrayLength(fn->Argument(1)));

      auto len_cmp = IR::Eq(lhs_len, rhs_len);

      auto equal_len_block = IR::Func::Current->AddBlock();
      auto true_block      = IR::Func::Current->AddBlock();
      auto false_block     = IR::Func::Current->AddBlock();
      auto phi_block       = IR::Func::Current->AddBlock();
      auto body_block      = IR::Func::Current->AddBlock();
      auto incr_block      = IR::Func::Current->AddBlock();

      IR::CondJump(len_cmp, equal_len_block, false_block);

      IR::Block::Current = true_block;
      IR::SetReturn(0, IR::Val::Bool(true));
      IR::ReturnJump();

      IR::Block::Current = false_block;
      IR::SetReturn(0, IR::Val::Bool(false));
      IR::ReturnJump();

      IR::Block::Current = equal_len_block;
      auto lhs_start     = IR::Index(fn->Argument(0), IR::Val::Uint(0));
      auto rhs_start     = IR::Index(fn->Argument(1), IR::Val::Uint(0));
      auto lhs_end       = IR::PtrIncr(lhs_start, lhs_len);
      IR::UncondJump(phi_block);

      IR::Block::Current = phi_block;
      auto lhs_phi       = IR::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi       = IR::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg   = IR::Func::Current->Command(lhs_phi).reg();
      auto rhs_phi_reg   = IR::Func::Current->Command(rhs_phi).reg();
      IR::CondJump(IR::Eq(lhs_phi_reg, lhs_end), true_block, body_block);

      IR::Block::Current = body_block;
      // TODO what if data type is an array?
      IR::CondJump(IR::Eq(IR::Load(lhs_phi_reg), IR::Load(rhs_phi_reg)),
                        incr_block, false_block);

      IR::Block::Current = incr_block;
      auto lhs_incr      = IR::PtrIncr(lhs_phi_reg, IR::Val::Uint(1));
      auto rhs_incr      = IR::PtrIncr(rhs_phi_reg, IR::Val::Uint(1));
      IR::UncondJump(phi_block);

      fn->SetArgs(lhs_phi, {IR::Val::Block(equal_len_block), lhs_start,
                            IR::Val::Block(incr_block), lhs_incr});
      fn->SetArgs(rhs_phi, {IR::Val::Block(equal_len_block), rhs_start,
                            IR::Val::Block(incr_block), rhs_incr});
    }
  }

  return IR::Call(IR::Val::Func(insertion.first->second), {lhs_ir, rhs_ir});
}

std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

// TODO mess around to see the performance characteristics. Maybe a flat map is
// better?
template <typename Key, typename Val>
using TypeContainer = std::unordered_map<Key, Val>;

static TypeContainer<Type *, std::unordered_map<size_t, Array>> fixed_arrays_;
Array *Arr(Type *t, size_t len) {
  return &fixed_arrays_[t].emplace(len, Array(t, len)).first->second;
}
static TypeContainer<Type *, Array> arrays_;
Array *Arr(Type *t) { return &arrays_.emplace(t, Array(t)).first->second; }

static std::map<std::vector<Type *>, Tuple> tuples_;
Type *Tup(std::vector<Type *> types) {
  if (types.empty()) { return Void; }
  if (types.size() == 1) { return types.front(); }
  return &tuples_.emplace(types, Tuple(types)).first->second;
}

static TypeContainer<Type *, Pointer> pointers_;
Pointer *Ptr(Type *t) {
  return &pointers_.emplace(t, Pointer(t)).first->second;
}

static TypeContainer<Type *, TypeContainer<Type *, Function>> funcs_;
Function *Func(Type *in, Type *out) {
  return &funcs_[in].emplace(out, Function(in, out)).first->second;
}

Function *Func(std::vector<Type *> in, Type *out) {
  return Func(Tup(std::move(in)), out);
}
Function *Func(Type *in, std::vector<Type *> out) {
  return Func(in, Tup(std::move(out)));
}
Function *Func(std::vector<Type *> in, std::vector<Type *> out) {
  return Func(Tup(std::move(in)), Tup(std::move(out)));
}

static TypeContainer<Type *, RangeType> ranges_;
RangeType *Range(Type *t) {
  return &ranges_.emplace(t, RangeType(t)).first->second;
}

static TypeContainer<Array *, SliceType> slices_;
SliceType *Slice(Array *a) {
  return &slices_.emplace(a, SliceType(a)).first->second;
}

static TypeContainer<Type *, Scope_Type> scopes_;
Scope_Type *ScopeType(Type *t) {
  return &scopes_.emplace(t, Scope_Type(t)).first->second;
}

static std::map<std::vector<Type *>, Variant> variants_;
Type *Var(std::vector<Type *> variants) {
  ASSERT_NE(variants.size(), 0);
  if (variants.size() == 1) { return variants.front(); }
  // TODO This sort order should be deterministic to allow interoperability
  // between multiple runs of the compiler.
  std::sort(variants.begin(), variants.end());
  variants.erase(std::unique(variants.begin(), variants.end()), variants.end());
  Variant v(variants);
  return &variants_.emplace(std::move(variants), std::move(v)).first->second;
}
