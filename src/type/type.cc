#include "all.h"

#include <map>
#include <unordered_map>

#include "base/guarded.h"
#include "../architecture.h"
#include "../context.h"
#include "../ir/func.h"

namespace type {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  Type *GlobalName = new Primitive(PrimType::EnumName);
#include "../type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

// TODO better to hash pair of Array*
// TODO thread safety?
static std::unordered_map<const Array *,
                          std::unordered_map<const Array *, IR::Func *>>
    eq_funcs;
static std::unordered_map<const Array *,
                          std::unordered_map<const Array *, IR::Func *>>
    ne_funcs;
IR::Val Array::Compare(const Array *lhs_type, IR::Val lhs_ir,
                             const Array *rhs_type, IR::Val rhs_ir,
                             bool equality, Context* ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;

  auto[iter, success] = funcs[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    std::vector<std::pair<std::string, AST::Expression *>> args = {
        {"lhs", nullptr}, {"rhs", nullptr}};
    auto *fn = ctx->mod_->AddFunc(Func({Ptr(lhs_type), Ptr(rhs_type)}, {Bool}),
                                  std::move(args));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();

      auto lhs_len = lhs_type->fixed_length
                         ? IR::Val::Int(static_cast<i32>(lhs_type->len))
                         : IR::Load(IR::ArrayLength(fn->Argument(0)));

      auto rhs_len = rhs_type->fixed_length
                         ? IR::Val::Int(static_cast<i32>(rhs_type->len))
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
      IR::SetReturn(IR::ReturnValue{0}, IR::Val::Bool(true));
      IR::ReturnJump();

      IR::Block::Current = false_block;
      IR::SetReturn(IR::ReturnValue{0}, IR::Val::Bool(false));
      IR::ReturnJump();

      IR::Block::Current = equal_len_block;
      auto lhs_start     = IR::Index(fn->Argument(0), IR::Val::Int(0));
      auto rhs_start     = IR::Index(fn->Argument(1), IR::Val::Int(0));
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
      auto lhs_incr      = IR::PtrIncr(lhs_phi_reg, IR::Val::Int(1));
      auto rhs_incr      = IR::PtrIncr(rhs_phi_reg, IR::Val::Int(1));
      IR::UncondJump(phi_block);

      fn->SetArgs(lhs_phi, {IR::Val::Block(equal_len_block), lhs_start,
                            IR::Val::Block(incr_block), lhs_incr});
      fn->SetArgs(rhs_phi, {IR::Val::Block(equal_len_block), rhs_start,
                            IR::Val::Block(incr_block), rhs_incr});
    }
  }

  return IR::Call(IR::Val::Func(iter->second), {lhs_ir, rhs_ir}, {});
}

using InitFnType = void (*)(const Type *, const Type *, IR::Val,
                            IR::Val, Context* ctx);

template <InitFnType InitFn>
static IR::Val ArrayInitializationWith(const Array *from_type,
                                       const Array *to_type, Context* ctx) {
  static std::unordered_map<const Array *,
                            std::unordered_map<const Array *, IR::Func *>>
      init_fns;

  auto[iter, success] = init_fns[to_type].emplace(from_type, nullptr);
  if (success) {
    std::vector<std::pair<std::string, AST::Expression *>> args = {
        {"arg0", nullptr}, {"arg1", nullptr}};
    auto *fn = ctx->mod_->AddFunc(Func({from_type, Ptr(to_type)}, {}),
                                  std::move(args));
    iter->second = fn;

    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      auto from_arg      = fn->Argument(0);
      auto to_arg        = fn->Argument(1);
      auto phi_block     = IR::Func::Current->AddBlock();
      auto body_block    = IR::Func::Current->AddBlock();
      auto exit_block    = IR::Func::Current->AddBlock();

      auto from_len = from_type->fixed_length
                          ? IR::Val::Int(static_cast<i32>(from_type->len))
                          : IR::Load(IR::ArrayLength(from_arg));

      if (!to_type->fixed_length) {
        // TODO Architecture dependence?
        auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
            from_len, from_type->data_type);

        IR::Store(from_len, IR::ArrayLength(to_arg));
        IR::Store(IR::Malloc(from_type->data_type, to_bytes),
                  IR::ArrayData(to_arg));
      }

      auto from_start = IR::Index(from_arg, IR::Val::Int(0));
      auto to_start   = IR::Index(to_arg, IR::Val::Int(0));
      auto from_end   = IR::PtrIncr(from_start, from_len);
      IR::UncondJump(phi_block);

      IR::Block::Current = phi_block;
      auto from_phi      = IR::Phi(Ptr(from_type->data_type));
      auto from_phi_reg  = IR::Func::Current->Command(from_phi).reg();
      auto to_phi        = IR::Phi(Ptr(to_type->data_type));
      auto to_phi_reg    = IR::Func::Current->Command(to_phi).reg();
      IR::CondJump(IR::Ne(from_phi_reg, from_end), body_block, exit_block);

      IR::Block::Current = body_block;
      InitFn(from_type->data_type, to_type->data_type, PtrCallFix(from_phi_reg),
             to_phi_reg, ctx);
      auto from_incr = IR::PtrIncr(from_phi_reg, IR::Val::Int(1));
      auto to_incr   = IR::PtrIncr(to_phi_reg, IR::Val::Int(1));
      IR::UncondJump(phi_block);

      fn->SetArgs(from_phi, {IR::Val::Block(fn->entry()), from_start,
                             IR::Val::Block(body_block), from_incr});
      fn->SetArgs(to_phi, {IR::Val::Block(fn->entry()), to_start,
                           IR::Val::Block(body_block), to_incr});

      IR::Block::Current = exit_block;
      IR::ReturnJump();
    }
  }
  return IR::Val::Func(iter->second);
}

template <InitFnType InitFn>
static IR::Val StructInitializationWith(const Struct *struct_type,
                                        Context *ctx) {
  static std::unordered_map<const Struct *, IR::Func *> struct_init_fns;
  auto[iter, success] = struct_init_fns.emplace(struct_type, nullptr);

  if (success) {
    std::vector<std::pair<std::string, AST::Expression *>> args = {
        {"arg0", nullptr}, {"arg1", nullptr}};
    auto *fn = iter->second = ctx->mod_->AddFunc(
        Func({Ptr(struct_type), Ptr(struct_type)}, {}), std::move(args));

    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      for (size_t i = 0; i < struct_type->fields_.size(); ++i) {
        InitFn(struct_type->fields_[i].type, struct_type->fields_[i].type,
               PtrCallFix(IR::Field(fn->Argument(0), i)),
               IR::Field(fn->Argument(1), i), ctx);
      }
      IR::ReturnJump();
    }
  }
  return IR::Val::Func(iter->second);
}

void EmitCopyInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Val to_var, Context* ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() ||
      to_type->is<Pointer>() || to_type->is<Function>()) {
    ASSERT(to_type == from_type);
    IR::Store(from_val, to_var);
  } else if (to_type->is<Array>()) {
    IR::Call(ArrayInitializationWith<EmitCopyInit>(&from_type->as<Array>(),
                                                   &to_type->as<Array>(), ctx),
             {from_val, to_var}, {});

  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);
    IR::Call(
        StructInitializationWith<EmitCopyInit>(&to_type->as<Struct>(), ctx),
        {from_val, to_var}, {});

  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Scope>()) {
    NOT_YET();
  } else {
    UNREACHABLE(to_type, from_type);
  }
}

void EmitMoveInit(const Type *from_type, const Type *to_type, IR::Val from_val,
                  IR::Val to_var, Context *ctx) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() ||
      to_type->is<Pointer>()) {
    ASSERT(to_type == from_type);
    IR::Store(from_val, to_var);

  } else if (to_type->is<Array>()) {
    auto *to_array_type   = &to_type->as<Array>();
    auto *from_array_type = &from_type->as<Array>();

    if (to_array_type->fixed_length || from_array_type->fixed_length) {
      IR::Call(ArrayInitializationWith<EmitMoveInit>(from_array_type,
                                                     to_array_type, ctx),
               {from_val, to_var}, {});

    } else {
      IR::Store(IR::Load(IR::ArrayLength(from_val)), IR::ArrayLength(to_var));
      IR::Store(IR::Load(IR::ArrayData(from_val)), IR::ArrayData(to_var));
      // TODO if this move is to be destructive, this assignment to array
      // length is not necessary.
      IR::Store(IR::Val::Int(0), IR::ArrayLength(from_val));
      IR::Store(IR::Malloc(from_array_type->data_type, IR::Val::Int(0)),
                IR::ArrayData(from_val));
    }
  } else if (to_type->is<Struct>()) {
    ASSERT(to_type == from_type);
    IR::Call(
        StructInitializationWith<EmitMoveInit>(&to_type->as<Struct>(), ctx),
        {from_val, to_var}, {});

  } else if (to_type->is<Function>()) {
    NOT_YET();
  } else if (to_type->is<Variant>()) {
    // TODO destruction in assignment may cause problems.
    to_type->EmitAssign(from_type, from_val, to_var, ctx);
  } else if (to_type->is<Scope>()) {
    NOT_YET();
  }
}

// TODO optimize (early exists. don't check lhs->is<> && rhs->is<>. If they
// don't match you can early exit.
const Type *Meet(const Type *lhs, const Type *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Err) { return rhs; } // Ignore errors
  if (rhs == Err) { return lhs; } // Ignore errors
  if (lhs == NullPtr || rhs == NullPtr) {
    // TODO It's not obvious to me that this is what I want to do.
    return nullptr;
  }
  if (lhs->is<Pointer>()) {
    return rhs->is<Pointer>() ? Ptr(Meet(lhs->as<Pointer>().pointee,
                                         rhs->as<Pointer>().pointee))
                              : nullptr;
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    const Type *result = nullptr;
    if (lhs->as<Array>().fixed_length && rhs->as<Array>().fixed_length) {
      if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
      result = Meet(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result, lhs->as<Array>().len) : result;
    } else {
      result = Meet(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result,
                          std::max(lhs->as<Array>().len, rhs->as<Array>().len))
                    : result;
    }
  } else if (lhs->is<Array>() && rhs == EmptyArray &&
             !lhs->as<Array>().fixed_length) {
    return Arr(lhs->as<Array>().data_type, 0);
  } else if (rhs->is<Array>() && lhs == EmptyArray &&
             !rhs->as<Array>().fixed_length) {
    return Arr(rhs->as<Array>().data_type, 0);
  } else if (lhs->is<Variant>()) {
    // TODO this feels very fishy, cf. ([3; int] | [4; int]) with [--; int]
    std::vector<const Type *> results;
    if (rhs->is<Variant>()) {
      for (const Type *l_type : lhs->as<Variant>().variants_) {
        for (const Type *r_type : rhs->as<Variant>().variants_) {
          const Type *result = Meet(l_type, r_type);
          if (result != nullptr) { results.push_back(result); }
        }
      }
    } else {
      for (const Type *t : lhs->as<Variant>().variants_) {
        if (const Type *result = Meet(t, rhs)) { results.push_back(result); }
      }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  } else if (rhs->is<Variant>()) { // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give a
    // better interface.
    std::vector<const Type *> results;
    for (const Type *t : rhs->as<Variant>().variants_) {
      if (const Type *result = Meet(t, lhs)) { results.push_back(result); }
    }
    return results.empty() ? nullptr : Var(std::move(results));
  }

  return nullptr;
}

const Type *Join(const Type *lhs, const Type *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Err) { return rhs; } // Ignore errors
  if (rhs == Err) { return lhs; } // Ignore errors
  if (lhs->is<Primitive>() && rhs->is<Primitive>()) {
    return lhs == rhs ? lhs : nullptr;
  }
  if (lhs == NullPtr && rhs->is<Pointer>()) { return rhs; }
  if (rhs == NullPtr && lhs->is<Pointer>()) { return lhs; }
  if (lhs->is<Pointer>() && rhs->is<Pointer>()) {
    return Join(lhs->as<Pointer>().pointee, rhs->as<Pointer>().pointee);
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    const Type *result = nullptr;
    if (lhs->as<Array>().fixed_length && rhs->as<Array>().fixed_length) {
      if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
      result = Join(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result, lhs->as<Array>().len) : result;
    } else {
      result = Join(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result) : result;
    }
  } else if (lhs->is<Array>() && rhs == EmptyArray &&
             !lhs->as<Array>().fixed_length) {
    return lhs;
  } else if (rhs->is<Array>() && lhs == EmptyArray &&
             !rhs->as<Array>().fixed_length) {
    return rhs;
  } else if (lhs->is<Variant>()) {
    std::vector<const Type *> rhs_types;
    if (rhs->is<Variant>()) {
      rhs_types = rhs->as<Variant>().variants_;
    } else {
      rhs_types = {rhs};
    }

    auto vars = lhs->as<Variant>().variants_;
    vars.insert(vars.end(), rhs_types.begin(), rhs_types.end());
    return Var(std::move(vars));
  } else if (rhs->is<Variant>()) { // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give
    // a better interface.
    for (const Type *v : rhs->as<Variant>().variants_) {
      if (lhs == v) { return rhs; }
    }
    return nullptr;
  }
  UNREACHABLE(lhs, rhs);
}

template <typename Key, typename Val>
using TypeContainer = std::unordered_map<Key, Val>;

static TypeContainer<const Type *,
                     std::unordered_map<size_t, Array>>
    fixed_arrays_;
const Array *Arr(const Type *t, size_t len) {
  return &fixed_arrays_[t].emplace(len, Array(t, len)).first->second;
}
static TypeContainer<const Type *, Array> arrays_;
const Array *Arr(const Type *t) {
  return &arrays_.emplace(t, Array(t)).first->second;
}

static std::map<std::vector<const Type *>, Variant> variants_;
const Type *Var(std::vector<const Type *> variants) {
  if (variants.empty()) { return type::Void; }
  if (variants.size() == 1) { return variants[0]; }

  size_t end = variants.size();
  size_t i   = 0;
  while (i < end) {
    if (variants[i]->is<Variant>()) {
      const Variant *var = &variants[i]->as<Variant>();
      variants[i]        = variants.back();
      variants.pop_back();
      variants.insert(variants.end(), var->variants_.begin(),
                      var->variants_.end());
    } else {
      ++i;
    }
  }

  // TODO This sort order should be deterministic to allow interoperability
  // between multiple runs of the compiler.

  std::sort(variants.begin(), variants.end());
  variants.erase(std::unique(variants.begin(), variants.end()), variants.end());

  if (variants.size() == 1) { return variants.front(); }

  Variant v(variants);
  return &variants_.emplace(std::move(variants), std::move(v)).first->second;
}

const Function *Function::ToIR() const {
  std::vector<const Type *> ins;
  ins.reserve(input.size());
  for (const Type *t : input) { ins.push_back(t->is_big() ? Ptr(t) : t); }

  std::vector<const Type *> outs;
  outs.reserve(output.size());
  for (const Type *t : output) { outs.push_back(t->is_big() ? Ptr(t) : t); }
  return Func(std::move(ins), std::move(outs));
}

static base::guarded<TypeContainer<const Type *, const Pointer>> pointers_;
const Pointer *Ptr(const Type *t) {
  return &pointers_.lock()->emplace(t, Pointer(t)).first->second;
}

static base::guarded<std::map<std::vector<const Type *>,
                              std::map<std::vector<const Type *>, Function>>>
    funcs_;
const Function *Func(std::vector<const Type *> in,
                     std::vector<const Type *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

static base::guarded<std::map<std::vector<const Type *>, const Scope>> scopes_;
const Scope *Scp(const std::vector<const Type *> &types) {
  return &scopes_.lock()->emplace(types, Scope(types)).first->second;
}

static base::guarded<std::map<std::vector<const Type *>, const Tuple>> tups_;
const Type *Tup(std::vector<const Type *> entries) {
  switch (entries.size()) {
    case 0: return type::Void;
    case 1: return entries[0];
    default: {
      Tuple tup(entries);
      auto[iter, success] =
          tups_.lock()->emplace(std::move(entries), std::move(tup));
      return &iter->second;
    } break;
  }
}

bool Type::is_big() const {
  return is<Array>() || is<Struct>() || is<Variant>() || is<Tuple>();
}
} // namespace type
