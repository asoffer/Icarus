#include "absl/algorithm/container.h"

#include "ast/ast.h"
#include "ast/methods/dump.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/table.h"
#include "compiler/extract_jumps.h"
#include "error/inference_failure_reason.h"
#include "frontend/operators.h"
#include "ir/compiled_fn.h"
#include "type/cast.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/parameter_pack.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace ir {

// TODO Duplicated in emit_value.h
static type::Type const *BuiltinType(core::Builtin b) {
  switch (b) {
#define ICARUS_CORE_BUILTIN_X(enumerator, str, t)                              \
  case core::Builtin::enumerator:                                              \
    return t;
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
  }
  UNREACHABLE();
}
}  // namespace ir

namespace compiler {
std::ostream &operator<<(std::ostream &os, VerifyResult r) {
  if (not r.ok()) { return os << "error"; }
  return os << (r.constant() ? "const[" : "non-const[") << r.type()->to_string()
            << "]";
}

namespace {

void AddAdl(ast::OverloadSet *overload_set, std::string_view id,
            type::Type const *t) {
  absl::flat_hash_set<module::BasicModule const *> modules;
  // TODO t->ExtractDefiningModules(&modules);

  for (auto const *mod : modules) {
    auto decls = mod->declarations(id);
    for (auto *d : decls) {
      ASSIGN_OR(continue, auto &t,
                Compiler(const_cast<module::BasicModule *>(mod)).type_of(d));
      // TODO handle this case. I think it's safe to just discard it.
      for (auto const *expr : overload_set->members()) {
        if (d == expr) { return; }
      }

      // TODO const
      overload_set->insert(d);
    }
  }
}

// NOTE: the order of these enumerators is meaningful and relied upon! They are
// ordered from strongest relation to weakest.
enum class Cmp { Order, Equality, None };

Cmp Comparator(type::Type const *t);

template <typename TypeContainer>
Cmp MinCmp(TypeContainer const &c) {
  using cmp_t = std::underlying_type_t<Cmp>;
  return static_cast<Cmp>(absl::c_accumulate(
      c, static_cast<cmp_t>(Cmp::Equality), [](cmp_t val, type::Type const *t) {
        return std::min(val, static_cast<cmp_t>(Comparator(t)));
      }));
}

Cmp Comparator(type::Type const *t) {
  using cmp_t = std::underlying_type_t<Cmp>;
  if (auto *v = t->if_as<type::Variant>()) { return MinCmp(v->variants_); }
  if (auto *tup = t->if_as<type::Tuple>()) { return MinCmp(tup->entries_); }
  if (auto *a = t->if_as<type::Array>()) {
    return static_cast<Cmp>(
        std::min(static_cast<cmp_t>(Comparator(a->data_type)),
                 static_cast<cmp_t>(Cmp::Equality)));
  } else if (auto *p = t->if_as<type::Primitive>()) {
    return type::IsNumeric(p) ? Cmp::Order : Cmp::Equality;
  } else if (t->is<type::Flags>() or t->is<type::BufferPointer>()) {
    return Cmp::Order;
  } else if (t->is<type::Enum>() or t->is<type::Pointer>()) {
    return Cmp::Equality;
  } else {
    return Cmp::None;
  }
}

bool VerifyAssignment(Compiler *visitor, frontend::SourceRange const &span,
                      type::Type const *to, type::Type const *from) {
  if (to == from and to->is<type::GenericStruct>()) { return true; }

  // TODO this feels like the semantics are iffy. It works fine if we assign
  // to/from the same type, but we really care if you can assign to a type
  // rather than copy from another, I think.
  if (not from->IsMovable()) {
    visitor->error_log()->NotMovable(span, from->to_string());
    return false;
  }

  if (to == from) { return true; }
  auto *to_tup   = to->if_as<type::Tuple>();
  auto *from_tup = from->if_as<type::Tuple>();
  if (to_tup and from_tup) {
    if (to_tup->size() != from_tup->size()) {
      visitor->error_log()->MismatchedAssignmentSize(span, to_tup->size(),
                                                     from_tup->size());
      return false;
    }

    bool result = true;
    for (size_t i = 0; i < to_tup->size(); ++i) {
      result &= VerifyAssignment(visitor, span, to_tup->entries_.at(i),
                                 from_tup->entries_.at(i));
    }
    return result;
  }

  if (auto *to_var = to->if_as<type::Variant>()) {
    if (auto *from_var = from->if_as<type::Variant>()) {
      for (auto fvar : from_var->variants_) {
        if (not to_var->contains(fvar)) {
          NOT_YET("log an error", from, to);
          return false;
        }
      }
      return true;
    } else {
      if (not to_var->contains(from)) {
        NOT_YET("log an error", from, to);
        return false;
      }

      return true;
    }
  }

  if (auto *to_ptr = to->if_as<type::Pointer>()) {
    if (from == type::NullPtr) { return true; }
    NOT_YET("log an error", from, to);
    return false;
  }

  NOT_YET("log an error: no cast from ", from->to_string(), " to ",
          to->to_string());
}

type::Type const *DereferenceAll(type::Type const *t) {
  while (auto *p = t->if_as<type::Pointer>()) { t = p->pointee; }
  return t;
}

}  // namespace

static std::optional<std::vector<VerifyResult>> VerifyWithoutSetting(
    Compiler *visitor, base::PtrSpan<ast::Expression const> exprs) {
  std::vector<VerifyResult> results;
  results.reserve(exprs.size());
  for (auto const &expr : exprs) {
    auto r = visitor->Visit(expr, VerifyTypeTag{});
    if (expr->needs_expansion()) {
      auto &entries = r.type()->as<type::Tuple>().entries_;
      for (auto *t : entries) { results.emplace_back(t, r.constant()); }
    } else {
      results.push_back(r);
    }
  }
  if (absl::c_any_of(results,
                     [](VerifyResult const &r) { return not r.ok(); })) {
    return std::nullopt;
  }
  return results;
}

static VerifyResult VerifySpecialFunctions(Compiler *visitor,
                                           ast::Declaration const *decl,
                                           type::Type const *decl_type) {
  bool error = false;
  if (decl->id() == "copy") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (not f->output.empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->input.size() != 2 or f->input.at(0) != f->input.at(1) or
          not f->input.at(0)->is<type::Pointer>() or
          not f->input.at(0)->as<type::Pointer>().pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect input type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s =
            f->input.at(0)->as<type::Pointer>().pointee->as<type::Struct>();

        if (decl->scope_ != s.scope_) {
          error = true;
          NOT_YET(
              "(copy) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(ast::Hashtag::Builtin::Uncopyable)) {
          NOT_YET("defined (copy) on a non-copyable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (copy) must be a function.");
    }
  } else if (decl->id() == "move") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (not f->output.empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->input.size() != 2 or f->input.at(0) != f->input.at(1) or
          not f->input.at(0)->is<type::Pointer>() or
          not f->input.at(0)->as<type::Pointer>().pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect input type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s =
            f->input.at(0)->as<type::Pointer>().pointee->as<type::Struct>();

        if (decl->scope_ != s.scope_) {
          error = true;
          NOT_YET(
              "(move) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(ast::Hashtag::Builtin::Immovable)) {
          error = true;
          NOT_YET("defined (move) for an immovable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (move) must be a function.");
    }
  }
  if (error) { visitor->set_result(decl, VerifyResult::Error()); }

  return visitor->set_result(
      decl,
      VerifyResult(decl_type, decl->flags() & ast::Declaration::f_IsConst));
}

// TODO what about shadowing of symbols across module boundaries imported with
// -- ::= ?
// Or when you import two modules verifying that symbols don't conflict.
bool Shadow(Compiler *visitor, type::Typed<ast::Declaration const *> decl1,
            type::Typed<ast::Declaration const *> decl2) {
  // TODO Don't worry about generic shadowing? It'll be checked later?
  if (decl1.type() == type::Generic or decl2.type() == type::Generic) {
    return false;
  }

  auto ExtractParams =
      [visitor](
          ast::Declaration const *decl) -> core::FnParams<type::Type const *> {
    bool is_const               = (decl->flags() & ast::Declaration::f_IsConst);
    ast::Expression const *expr = decl->init_val();
    if (not is_const) {
      return visitor->type_of(expr)
          ->as<type::Function>()
          .AnonymousFnParams()
          .Transform([](type::Typed<ast::Declaration const *> expr) {
            return expr.type();
          });
    } else if (auto *fn_lit = expr->if_as<ast::FunctionLiteral>()) {
      return fn_lit->params().Transform(
          [visitor](std::unique_ptr<ast::Declaration> const &decl) {
            return visitor->type_of(decl.get());
          });
    }
    NOT_YET();
  };
  return core::AmbiguouslyCallable(
      ExtractParams(*decl1), ExtractParams(*decl2),
      [](type::Type const *lhs, type::Type const *rhs) {
        return type::Meet(lhs, rhs) != nullptr;
      });
}

enum DeclKind { INFER = 1, CUSTOM_INIT = 2, UNINITIALIZED = 4 };

static InferenceFailureReason Inferrable(type::Type const *t) {
  if (t == type::NullPtr) { return InferenceFailureReason::NullPtr; }
  if (t == type::EmptyArray) { return InferenceFailureReason::EmptyArray; }
  if (auto *a = t->if_as<type::Array>()) { return Inferrable(a->data_type); }
  if (auto *p = t->if_as<type::Pointer>()) { return Inferrable(p->pointee); }
  if (auto *v = t->if_as<type::Variant>()) {
    // TODO only returning the first failure here and not even givving a good
    // explanation of precisely what the problem is. Fix here and below.
    for (auto const *var : v->variants_) {
      auto reason = Inferrable(var);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  } else if (auto *tup = t->if_as<type::Tuple>()) {
    for (auto const *entry : tup->entries_) {
      auto reason = Inferrable(entry);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  } else if (auto *f = t->if_as<type::Function>()) {
    for (auto const *t : f->input) {
      auto reason = Inferrable(t);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
    for (auto const *t : f->output) {
      auto reason = Inferrable(t);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  }
  // TODO higher order types?
  return InferenceFailureReason::Inferrable;
}

// TODO there's not that much shared between the inferred and uninferred cases,
// so probably break them out.
VerifyResult VerifyBody(Compiler *visitor, ast::FunctionLiteral const *node) {
  for (auto const *stmt : node->stmts()) {
    visitor->Visit(stmt, VerifyTypeTag{});
  }
  // TODO propogate cyclic dependencies.

  ExtractJumps extractor;
  for (auto const *stmt : node->stmts()) { extractor.Visit(stmt); }

  // TODO we can have yields and returns, or yields and jumps, but not jumps and
  // returns. Check this.
  absl::flat_hash_set<type::Type const *> types;
  absl::flat_hash_map<ast::ReturnStmt const *, type::Type const *>
      saved_ret_types;
  for (auto const *n : extractor.jumps(ExtractJumps::Kind::Return)) {
    if (auto const *ret_node = n->if_as<ast::ReturnStmt>()) {
      std::vector<type::Type const *> ret_types;
      for (auto const *expr : ret_node->exprs()) {
        ret_types.push_back(visitor->type_of(expr));
      }
      auto *t = Tup(std::move(ret_types));
      types.emplace(t);
      saved_ret_types.emplace(ret_node, t);
    } else {
      UNREACHABLE();  // TODO
    }
  }

  std::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(node->params().size());
  for (auto &param : node->params()) {
    input_type_vec.push_back(
        ASSERT_NOT_NULL(visitor->type_of(param.value.get())));
  }

  if (not node->outputs()) {
    std::vector<type::Type const *> output_type_vec(
        std::make_move_iterator(types.begin()),
        std::make_move_iterator(types.end()));

    if (types.size() > 1) { NOT_YET("log an error"); }
    auto f = type::Func(std::move(input_type_vec), std::move(output_type_vec));
    return visitor->set_result(node, VerifyResult::Constant(f));

  } else {
    auto *node_type  = visitor->type_of(node);
    auto const &outs = ASSERT_NOT_NULL(node_type)->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *n : extractor.jumps(ExtractJumps::Kind::Return)) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            if (not ret_node->exprs().empty()) {
              visitor->error_log()->NoReturnTypes(ret_node);
              err = true;
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? VerifyResult::Error() : VerifyResult::Constant(node_type);
      } break;
      case 1: {
        bool err = false;
        for (auto *n : extractor.jumps(ExtractJumps::Kind::Return)) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *t = ASSERT_NOT_NULL(saved_ret_types.at(ret_node));
            if (t == outs[0]) { continue; }
            visitor->error_log()->ReturnTypeMismatch(
                outs[0]->to_string(), t->to_string(), ret_node->span);
            err = true;
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? VerifyResult::Error() : VerifyResult::Constant(node_type);
      } break;
      default: {
        for (auto *n : extractor.jumps(ExtractJumps::Kind::Return)) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *expr_type = ASSERT_NOT_NULL(saved_ret_types.at(ret_node));
            if (expr_type->is<type::Tuple>()) {
              auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
              if (tup_entries.size() != outs.size()) {
                visitor->error_log()->ReturningWrongNumber(
                    ret_node->span,
                    (expr_type->is<type::Tuple>()
                         ? expr_type->as<type::Tuple>().size()
                         : 1),
                    outs.size());
                return VerifyResult::Error();
              } else {
                bool err = false;
                for (size_t i = 0; i < tup_entries.size(); ++i) {
                  if (tup_entries.at(i) != outs.at(i)) {
                    // TODO if this is a commalist we can point to it more
                    // carefully but if we're just passing on multiple return
                    // values it's harder.
                    //
                    // TODO point the span to the correct entry which may be
                    // hard if it's splatted.
                    visitor->error_log()->IndexedReturnTypeMismatch(
                        outs.at(i)->to_string(), tup_entries.at(i)->to_string(),
                        ret_node->span, i);
                    err = true;
                  }
                }
                if (err) { return VerifyResult::Error(); }
              }
            } else {
              visitor->error_log()->ReturningWrongNumber(
                  ret_node->span,
                  (expr_type->is<type::Tuple>()
                       ? expr_type->as<type::Tuple>().size()
                       : 1),
                  outs.size());
              return VerifyResult::Error();
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return VerifyResult::Constant(node_type);
      } break;
    }
  }
}

void VerifyBody(Compiler *visitor, ast::JumpHandler const *node) {
  DEBUG_LOG("JumpHandler")(ast::Dump::ToString(node));
  ExtractJumps extractor;
  for (auto const *stmt : node->stmts()) {
    visitor->Visit(stmt, VerifyTypeTag{});
    extractor.Visit(stmt);
  }

  auto jumps = extractor.jumps(ExtractJumps::Kind::Jump);
  for (auto const *jump : jumps) {
    DEBUG_LOG("JumpHandler")
    (ast::Dump::ToString(&jump->as<ast::Jump>()));
    for (auto const &jump_opt : jump->as<ast::Jump>().options_) {
      DEBUG_LOG("JumpHandler")
      (jump_opt.block, " args=(", jump_opt.args, ")");
    }
  }
}

VerifyResult Compiler::VerifyConcreteFnLit(ast::FunctionLiteral const *node) {
  std::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(node->params().size());
  for (auto &d : node->params()) {
    ASSIGN_OR(return _, auto result, Visit(d.value.get(), VerifyTypeTag{}));
    input_type_vec.push_back(result.type());
  }

  std::vector<type::Type const *> output_type_vec;
  bool error   = false;
  auto outputs = node->outputs();
  if (outputs) {
    output_type_vec.reserve(outputs->size());
    for (auto *output : *outputs) {
      auto result = Visit(output, VerifyTypeTag{});
      output_type_vec.push_back(result.type());
      if (result.type() != nullptr and not result.constant()) {
        // TODO this feels wrong because output could be a decl. And that decl
        // being a const decl isn't what I care about.
        NOT_YET("log an error");
        error = true;
      }
    }
  }

  if (error or
      absl::c_any_of(input_type_vec,
                     [](type::Type const *t) { return t == nullptr; }) or
      absl::c_any_of(output_type_vec,
                     [](type::Type const *t) { return t == nullptr; })) {
    return VerifyResult::Error();
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (num_errors() > 0) {
    error_log()->Dump();
    return VerifyResult::Error();
  }

  if (outputs) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = (*outputs)[i]->if_as<ast::Declaration>()) {
        output_type_vec.at(i) = type_of(decl);
      } else {
        ASSERT(output_type_vec.at(i) == type::Type_);
        output_type_vec.at(i) = backend::EvaluateAs<type::Type const *>(
            MakeThunk((*outputs)[i], type::Type_));
      }
    }

    return set_result(
        node, VerifyResult::Constant(type::Func(std::move(input_type_vec),
                                                std::move(output_type_vec))));
  } else {
    return VerifyBody(this, node);
  }
}

static std::optional<
    std::vector<std::pair<ast::Expression const *, VerifyResult>>>
VerifySpan(Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, VerifyResult>> results;
  bool err = false;
  for (auto *expr : exprs) {
    results.emplace_back(expr, v->Visit(expr, VerifyTypeTag{}));
    err |= not results.back().second.ok();
  }
  if (err) { return std::nullopt; }
  return results;
}

enum class Constness { Error, Const, NonConst };
static Constness VerifyAndGetConstness(
    Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  bool err      = false;
  bool is_const = true;
  for (auto *expr : exprs) {
    auto r = v->Visit(expr, VerifyTypeTag{});
    err |= not r.ok();
    if (not err) { is_const &= r.constant(); }
  }
  if (err) { return Constness::Error; }
  return is_const ? Constness::Const : Constness::NonConst;
}

static std::vector<
    core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>>
VerifyBlockNode(Compiler *visitor, ast::BlockNode const *node) {
  visitor->Visit(node, VerifyTypeTag{});

  ExtractJumps extractor;
  for (auto const *stmt : node->stmts()) { extractor.Visit(stmt); }

  auto yields = extractor.jumps(ExtractJumps::Kind::Yield);
  // TODO this setup is definitely wrong because it doesn't account for
  // multiple yields correctly. For example,
  //
  // ```
  //  result: int32 | bool = if (cond) then {
  //    yield 3
  //  } else if (other_cond) then {
  //    yield 4
  //  } else {
  //    yield true
  //  }
  //  ```
  std::vector<core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>>
      result;
  for (auto *yield : yields) {
    auto &back = result.emplace_back();
    // TODO actually fill a fnargs
    std::vector<std::pair<ast::Expression const *, VerifyResult>>
        local_pos_yields;
    for (auto *yield_expr : yields[0]->as<ast::YieldStmt>().exprs()) {
      back.pos_emplace(
          yield_expr,
          *ASSERT_NOT_NULL(visitor->prior_verification_attempt(yield_expr)));
    }
  }
  return result;
}

VerifyResult Compiler::Visit(ast::Access const *node, VerifyTypeTag) {
  ASSIGN_OR(return VerifyResult::Error(), auto operand_result,
                   Visit(node->operand(), VerifyTypeTag{}));

  auto base_type = DereferenceAll(operand_result.type());
  if (base_type == type::Type_) {
    if (not operand_result.constant()) {
      error_log()->NonConstantTypeMemberAccess(node->span);
      return VerifyResult::Error();
    }
    // TODO We may not be allowed to evaluate node:
    //    f ::= (T: type) => T.key
    // We need to know that T is const
    auto *t           = type_of(node->operand());
    auto *evaled_type =
        backend::EvaluateAs<type::Type const *>(MakeThunk(node->operand(), t));

    // For enums and flags, regardless of whether we can get the value, it's
    // clear that node is supposed to be a member so we should emit an error but
    // carry on assuming that node is an element of that enum type.
    if (auto *e = evaled_type->if_as<type::Enum>()) {
      if (not e->Get(node->member_name()).has_value()) {
        error_log()->MissingMember(node->span, node->member_name(),
                                   evaled_type->to_string());
      }
      return set_result(node, VerifyResult::Constant(evaled_type));
    } else if (auto *f = evaled_type->if_as<type::Flags>()) {
      if (not f->Get(node->member_name()).has_value()) {
        error_log()->MissingMember(node->span, node->member_name(),
                                   evaled_type->to_string());
      }
      return set_result(node, VerifyResult::Constant(evaled_type));
    } else {
      // TODO what about structs? Can structs have constant members we're
      // allowed to access?
      error_log()->TypeHasNoMembers(node->span);
      return VerifyResult::Error();
    }

  } else if (auto *s = base_type->if_as<type::Struct>()) {
    auto const *member = s->field(node->member_name());
    if (member == nullptr) {
      error_log()->MissingMember(node->span, node->member_name(),
                                 s->to_string());
      return VerifyResult::Error();
    }

    if (module() != s->defining_module() and
        absl::c_none_of(member->hashtags_, [](ast::Hashtag h) {
          return h.kind_ == ast::Hashtag::Builtin::Export;
        })) {
      error_log()->NonExportedMember(node->span, node->member_name(),
                                     s->to_string());
    }

    return set_result(node,
                      VerifyResult(member->type, operand_result.constant()));

  } else if (base_type == type::Module) {
    if (not operand_result.constant()) {
      error_log()->NonConstantModuleMemberAccess(node->span);
      return VerifyResult::Error();
    }

    auto *mod = backend::EvaluateAs<module::BasicModule const *>(
        MakeThunk(node->operand(), operand_result.type()));
    auto decls = mod->declarations(node->member_name());
    type::Type const *t;
    switch (decls.size()) {
      case 0: NOT_YET();
      case 1:
        // TODO remove this const_cast.
        t = Compiler(const_cast<module::BasicModule *>(mod)).type_of(decls[0]);
        break;
      default: NOT_YET();
    }

    if (t == nullptr) {
      error_log()->NoExportedSymbol(node->span);
      return VerifyResult::Error();
    }

    // TODO is node right?
    return set_result(node, VerifyResult::Constant(t));
  } else {
    error_log()->MissingMember(node->span, node->member_name(),
                               base_type->to_string());
    return VerifyResult::Error();
  }
}

VerifyResult Compiler::Visit(ast::ArrayLiteral const *node, VerifyTypeTag) {
  if (node->empty()) {
    return set_result(node, VerifyResult::Constant(type::EmptyArray));
  }

  ASSIGN_OR(return VerifyResult::Error(), auto expr_results,
                   VerifyWithoutSetting(this, node->elems()));
  auto *t                       = expr_results.front().type();
  type::Type const *result_type = type::Arr(expr_results.size(), t);
  bool constant                 = true;
  for (auto expr_result : expr_results) {
    constant &= expr_result.constant();
    if (expr_result.type() != t) {
      error_log()->InconsistentArrayType(node->span);
      return VerifyResult::Error();
    }
  }
  return set_result(node, VerifyResult(result_type, constant));
}

VerifyResult Compiler::Visit(ast::ArrayType const *node, VerifyTypeTag) {
  std::vector<VerifyResult> length_results;
  length_results.reserve(node->lengths().size());
  bool is_const = true;
  for (auto const &len : node->lengths()) {
    auto result = Visit(len, VerifyTypeTag{});
    is_const &= result.constant();
    length_results.push_back(result);
    if (result.type() != type::Int64) {
      error_log()->ArrayIndexType(node->span);
    }
  }

  auto data_type_result = Visit(node->data_type(), VerifyTypeTag{});
  if (data_type_result.type() != type::Type_) {
    error_log()->ArrayDataTypeNotAType(node->data_type()->span);
  }

  return set_result(
      node,
      VerifyResult(type::Type_, data_type_result.constant() and is_const));
}

static bool IsTypeOrTupleOfTypes(type::Type const *t) {
  return t == type::Type_ or t->is<type::Tuple>();
}

VerifyResult Compiler::Visit(ast::Binop const *node, VerifyTypeTag) {
  auto lhs_result = Visit(node->lhs(), VerifyTypeTag{});
  auto rhs_result = Visit(node->rhs(), VerifyTypeTag{});
  if (not lhs_result.ok() or not rhs_result.ok()) {
    return VerifyResult::Error();
  }

  using frontend::Operator;
  switch (node->op()) {
    case Operator::Assign: {
      // TODO if lhs is reserved?
      if (not VerifyAssignment(this, node->span, lhs_result.type(),
                               rhs_result.type())) {
        return VerifyResult::Error();
      }
      return VerifyResult::NonConstant(type::Void());
    } break;
    case Operator::XorEq:
      if (lhs_result.type() == rhs_result.type() and
          (lhs_result.type() == type::Bool or
           lhs_result.type()->is<type::Flags>())) {
        return set_result(node, lhs_result);
      } else {
        error_log()->XorEqNeedsBoolOrFlags(node->span);
        return VerifyResult::Error();
      }
    case Operator::AndEq:
      if (lhs_result.type() == rhs_result.type() and
          (lhs_result.type() == type::Bool or
           lhs_result.type()->is<type::Flags>())) {
        return set_result(node, lhs_result);
      } else {
        error_log()->AndEqNeedsBoolOrFlags(node->span);
        return VerifyResult::Error();
      }
    case Operator::OrEq:
      if (lhs_result.type() == rhs_result.type() and
          (lhs_result.type() == type::Bool or
           lhs_result.type()->is<type::Flags>())) {
        return set_result(node, lhs_result);
      } else {
        error_log()->OrEqNeedsBoolOrFlags(node->span);
        return VerifyResult::Error();
      }

#define CASE(OpName, symbol, return_type)                                      \
  case Operator::OpName: {                                                     \
    bool is_const = lhs_result.constant() and rhs_result.constant();           \
    if (type::IsNumeric(lhs_result.type()) and                                 \
        type::IsNumeric(rhs_result.type())) {                                  \
      if (lhs_result.type() == rhs_result.type()) {                            \
        return set_result(node, VerifyResult((return_type), is_const));        \
      } else {                                                                 \
        error_log()->MismatchedBinopArithmeticType(                            \
            lhs_result.type()->to_string(), rhs_result.type()->to_string(),    \
            node->span);                                                       \
        return VerifyResult::Error();                                          \
      }                                                                        \
    } else {                                                                   \
      ast::OverloadSet os(node->scope_, symbol);                               \
      AddAdl(&os, symbol, lhs_result.type());                                  \
      AddAdl(&os, symbol, rhs_result.type());                                  \
      return ast::VerifyDispatch(                                              \
          this, node, os,                                                      \
          core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(      \
              {std::pair{node->lhs(), lhs_result},                             \
               std::pair{node->rhs(), rhs_result}},                            \
              {}));                                                            \
    }                                                                          \
  } break;
      CASE(Sub, "-", lhs_result.type());
      CASE(Mul, "*", lhs_result.type());
      CASE(Div, "/", lhs_result.type());
      CASE(Mod, "%", lhs_result.type());
      CASE(SubEq, "-=", type::Void());
      CASE(MulEq, "*=", type::Void());
      CASE(DivEq, "/=", type::Void());
      CASE(ModEq, "%=", type::Void());
#undef CASE
    case Operator::Add: {
      bool is_const = lhs_result.constant() and rhs_result.constant();
      if (type::IsNumeric(lhs_result.type()) and
          type::IsNumeric(rhs_result.type())) {
        if (lhs_result.type() == rhs_result.type()) {
          return set_result(node, VerifyResult(lhs_result.type(), is_const));
        } else {
          error_log()->MismatchedBinopArithmeticType(
              lhs_result.type()->to_string(), rhs_result.type()->to_string(),
              node->span);
          return VerifyResult::Error();
        }
      } else {
        ast::OverloadSet os(node->scope_, "+");
        AddAdl(&os, "+", lhs_result.type());
        AddAdl(&os, "+", rhs_result.type());
        return ast::VerifyDispatch(
            this, node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair{node->lhs(), lhs_result},
                 std::pair{node->rhs(), rhs_result}},
                {}));
      }
    } break;
    case Operator::AddEq: {
      bool is_const = lhs_result.constant() and rhs_result.constant();
      if (type::IsNumeric(lhs_result.type()) and
          type::IsNumeric(rhs_result.type())) {
        if (lhs_result.type() == rhs_result.type()) {
          return set_result(node, VerifyResult(type::Void(), is_const));
        } else {
          error_log()->MismatchedBinopArithmeticType(
              lhs_result.type()->to_string(), rhs_result.type()->to_string(),
              node->span);
          return VerifyResult::Error();
        }
      } else {
        ast::OverloadSet os(node->scope_, "+=");
        AddAdl(&os, "+=", lhs_result.type());
        AddAdl(&os, "+=", rhs_result.type());
        return ast::VerifyDispatch(
            this, node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair{node->lhs(), lhs_result},
                 std::pair{node->rhs(), rhs_result}},
                {}));
      }
    } break;
    case Operator::Arrow: {
      type::Type const *t = type::Type_;
      if (not IsTypeOrTupleOfTypes(lhs_result.type())) {
        t = nullptr;
        error_log()->NonTypeFunctionInput(node->span);
      }

      if (not IsTypeOrTupleOfTypes(rhs_result.type())) {
        t = nullptr;
        error_log()->NonTypeFunctionOutput(node->span);
      }

      if (t == nullptr) { return VerifyResult::Error(); }

      return set_result(
          node, VerifyResult(type::Type_,
                             lhs_result.constant() and rhs_result.constant()));
    }
    default: UNREACHABLE();
  }
  UNREACHABLE(static_cast<int>(node->op()));
}

VerifyResult Compiler::Visit(ast::BlockLiteral const *node, VerifyTypeTag) {
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { Visit(b, VerifyTypeTag{}); }
  for (auto *a : node->after()) { Visit(a, VerifyTypeTag{}); }

  return set_result(node, VerifyResult::Constant(type::Block));
}

VerifyResult Compiler::Visit(ast::BlockNode const *node, VerifyTypeTag) {
  for (auto *arg : node->args()) { Visit(arg, VerifyTypeTag{}); }
  for (auto *stmt : node->stmts()) { Visit(stmt, VerifyTypeTag{}); }
  return set_result(node, VerifyResult::Constant(type::Block));
}

VerifyResult Compiler::Visit(ast::BuiltinFn const *node, VerifyTypeTag) {
  return set_result(node,
                    VerifyResult::Constant(ir::BuiltinType(node->value())));
}

static ast::OverloadSet FindOverloads(
    compiler::Compiler *visitor, ast::Scope *scope, std::string_view token,
    core::FnArgs<type::Type const *> arg_types) {
  ast::OverloadSet os(scope, token);
  for (type::Type const *t : arg_types) { AddAdl(&os, token, t); };
  return os;
}

template <typename EPtr, typename StrType>
static VerifyResult VerifyCall(Compiler *visitor, ast::BuiltinFn const *b,
                               core::FnArgs<EPtr, StrType> const &args,
                               core::FnArgs<VerifyResult> const &arg_results) {
  switch (b->value()) {
    case core::Builtin::Foreign: {
      bool err = false;
      if (not arg_results.named().empty()) {
        visitor->error_log()->BuiltinError(b->span,
                                           "Built-in function `foreign` cannot "
                                           "be called with named arguments.");
        err = true;
      }

      size_t size = arg_results.size();
      if (size != 2u) {
        visitor->error_log()->BuiltinError(
            b->span, absl::StrCat("Built-in function `foreign` takes exactly "
                                  "two arguments (You provided ",
                                  size, ")."));
        err = true;
      }

      if (not err) {
        if (arg_results.at(0).type() != type::ByteView) {
          visitor->error_log()->BuiltinError(
              b->span,
              absl::StrCat("First argument to `foreign` must be a byte-view "
                           "(You provided a(n) ",
                           arg_results.at(0).type()->to_string(), ")."));
        }
        if (not arg_results.at(0).constant()) {
          visitor->error_log()->BuiltinError(
              b->span, "First argument to `foreign` must be a constant.");
        }
        if (arg_results.at(1).type() != type::Type_) {
          visitor->error_log()->BuiltinError(
              b->span,
              "Second argument to `foreign` must be a type (You provided "
              "a(n) " +
                  arg_results.at(0).type()->to_string() + ").");
        }
        if (not arg_results.at(1).constant()) {
          visitor->error_log()->BuiltinError(
              b->span, "Second argument to `foreign` must be a constant.");
        }
      }
      return VerifyResult::Constant(backend::EvaluateAs<type::Type const *>(
          visitor->MakeThunk(args.at(1), type::Type_)));
    } break;
    case core::Builtin::Opaque:
      if (not arg_results.empty()) {
        visitor->error_log()->BuiltinError(
            b->span, "Built-in function `opaque` takes no arguments.");
      }
      return VerifyResult::Constant(ir::BuiltinType(core::Builtin::Opaque)
                                        ->as<type::Function>()
                                        .output[0]);

    case core::Builtin::Bytes: {
      size_t size = arg_results.size();
      if (not arg_results.named().empty()) {
        visitor->error_log()->BuiltinError(
            b->span,
            "Built-in function `bytes` cannot be "
            "called with named arguments.");
      } else if (size != 1u) {
        visitor->error_log()->BuiltinError(
            b->span,
            "Built-in function `bytes` takes "
            "exactly one argument (You provided " +
                std::to_string(size) + ").");
      } else if (arg_results.at(0).type() != type::Type_) {
        visitor->error_log()->BuiltinError(
            b->span,
            "Built-in function `bytes` must take a single argument of type "
            "`type` (You provided a(n) " +
                arg_results.at(0).type()->to_string() + ").");
      }
      return VerifyResult::Constant(ir::BuiltinType(core::Builtin::Bytes)
                                        ->as<type::Function>()
                                        .output[0]);
    }
    case core::Builtin::Alignment: {
      size_t size = arg_results.size();
      if (not arg_results.named().empty()) {
        visitor->error_log()->BuiltinError(
            b->span,
            "Built-in function `alignment` cannot "
            "be called with named arguments.");
      }
      if (size != 1u) {
        visitor->error_log()->BuiltinError(
            b->span,
            "Built-in function `alignment` takes "
            "exactly one argument (You provided " +
                std::to_string(size) + ").");

      } else if (arg_results.at(0).type() != type::Type_) {
        visitor->error_log()->BuiltinError(
            b->span,
            "Built-in function `alignment` must take a single argument of "
            "type `type` (you provided a(n) " +
                arg_results.at(0).type()->to_string() + ")");
      }
      return VerifyResult::Constant(ir::BuiltinType(core::Builtin::Alignment)
                                        ->as<type::Function>()
                                        .output[0]);
    }
#if defined(ICARUS_DEBUG)
    case core::Builtin::DebugIr:
      // This is for debugging the compiler only, so there's no need to write
      // decent errors here.
      ASSERT(arg_results, matcher::IsEmpty());
      return VerifyResult::Constant(type::Void());
#endif  // defined(ICARUS_DEBUG)
  }
  UNREACHABLE();
}

template <typename EPtr, typename StrType>
static std::pair<core::FnArgs<VerifyResult, StrType>, bool> VerifyFnArgs(
    Compiler *visitor, core::FnArgs<EPtr, StrType> const &args) {
  bool err         = false;
  auto arg_results = args.Transform([&](EPtr const &expr) {
    auto expr_result = visitor->Visit(expr, VerifyTypeTag{});
    err |= not expr_result.ok();
    return expr_result;
  });

  return std::pair{std::move(arg_results), err};
}

VerifyResult Compiler::Visit(ast::Call const *node, VerifyTypeTag) {
  auto [arg_results, err] = VerifyFnArgs(this, node->args());
  // TODO handle cyclic dependencies in call arguments.
  if (err) { return VerifyResult::Error(); }

  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    // TODO: Should we allow these to be overloaded?
    ASSIGN_OR(return VerifyResult::Error(), auto result,
                     VerifyCall(this, b, node->args(), arg_results));
    return set_result(node, VerifyResult(result.type(), result.constant()));
  }

  ast::OverloadSet overload_set = [&]() {
    if (auto *id = node->callee()->if_as<ast::Identifier>()) {
      return FindOverloads(this, node->scope_, id->token(),
                           arg_results.Transform(
                               [](VerifyResult const &p) { return p.type(); }));
    } else {
      ast::OverloadSet os;
      os.insert(node->callee());
      // TODO ADL for node?
      return os;
    }
  }();

  core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>
      arg_expr_result;
  for (size_t i = 0; i < arg_results.pos().size(); ++i) {
    arg_expr_result.pos_emplace(node->args().at(i), arg_results.at(i));
  }
  for (auto const &[name, res] : arg_results.named()) {
    arg_expr_result.named_emplace(
        std::piecewise_construct, std::forward_as_tuple(name),
        std::forward_as_tuple(node->args().at(name), res));
  }

  ASSIGN_OR(
      return VerifyResult::Error(),  //
             auto table,
             FnCallDispatchTable::Verify(
                 this, overload_set,
                 arg_expr_result.Transform([](auto x) { return x.second; })));
  auto result = VerifyResult::NonConstant(table.result_type());
  data_.set_dispatch_table(node, std::move(table));
  DEBUG_LOG("dispatch-verify")("Resulting type of dispatch is ", result);
  return set_result(node, result);
}

VerifyResult Compiler::Visit(ast::Cast const *node, VerifyTypeTag) {
  auto expr_result = Visit(node->expr(), VerifyTypeTag{});
  auto type_result = Visit(node->type(), VerifyTypeTag{});
  if (not expr_result.ok() or not type_result.ok()) {
    return VerifyResult::Error();
  }

  if (type_result.type() != type::Type_) {
    error_log()->CastToNonType(node->span);
    return VerifyResult::Error();
  }
  if (not type_result.constant()) {
    error_log()->CastToNonConstantType(node->span);
    return VerifyResult::Error();
  }
  auto *t = ASSERT_NOT_NULL(backend::EvaluateAs<type::Type const *>(
      MakeThunk(node->type(), type::Type_)));
  if (t->is<type::Struct>()) {
    ast::OverloadSet os(node->scope_, "as");
    AddAdl(&os, "as", t);
    AddAdl(&os, "as", expr_result.type());
    NOT_YET();
    // os.keep([t](ast::Overload const &o) { return o.result.type() == t; });

    return ast::VerifyDispatch(
        this, node, os,
        core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
            {std::pair(node->expr(), expr_result)}, {}));
  } else {
    if (not type::CanCast(expr_result.type(), t)) {
      error_log()->InvalidCast(expr_result.type()->to_string(), t->to_string(),
                               node->span);
      NOT_YET("log an error", expr_result.type(), t);
    }

    return set_result(node, VerifyResult(t, expr_result.constant()));
  }
}

VerifyResult Compiler::Visit(ast::ChainOp const *node, VerifyTypeTag) {
  std::vector<VerifyResult> results;
  results.reserve(node->exprs().size());
  for (auto *expr : node->exprs()) {
    results.push_back(Visit(expr, VerifyTypeTag{}));
  }
  if (absl::c_any_of(results,
                     [](VerifyResult const &v) { return not v.ok(); })) {
    return VerifyResult::Error();
  }

  if (node->ops()[0] == frontend::Operator::Or) {
    bool found_err = false;
    for (size_t i = 0; i < results.size() - 1; ++i) {
      if (results[i].type() == type::Block) {
        if (not results[i].constant()) {
          NOT_YET("log an error: non const block");
        }

        error_log()->EarlyRequiredBlock(node->exprs()[i]->span);
        found_err = true;
      } else {
        goto not_blocks;
      }
    }
    if (found_err) { return VerifyResult::Error(); }
    auto &last = results.back();
    if (last.type() != type::Block) {
      goto not_blocks;
    } else if (not results.back().constant()) {
      NOT_YET("log an error: non const block");
    } else {
      return set_result(node, VerifyResult::Constant(last.type()));
    }
  }
not_blocks:

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  switch (node->ops()[0]) {
    case frontend::Operator::Or:
    case frontend::Operator::And:
    case frontend::Operator::Xor: {
      bool failed                       = false;
      bool is_const                     = true;
      type::Type const *first_expr_type = results[0].type();

      for (auto &result : results) {
        // TODO node collection of error messages could be greatly improved.
        if (result.type() != first_expr_type) {
          auto op_str = [node] {
            switch (node->ops()[0]) {
              case frontend::Operator::Or: return "|";
              case frontend::Operator::And: return "&";
              case frontend::Operator::Xor: return "^";
              default: UNREACHABLE();
            }
          }();

          NOT_YET("Log an error");
          is_const &= result.constant();
          failed = true;
        }
      }

      if (failed) { return VerifyResult::Error(); }
      return set_result(node, VerifyResult(first_expr_type, is_const));
    } break;
    default: {
      bool is_const = results[0].constant();
      ASSERT(node->exprs().size() >= 2u);
      for (size_t i = 0; i + 1 < node->exprs().size(); ++i) {
        VerifyResult const &lhs_result = results[i];
        VerifyResult const &rhs_result = results[i + 1];
        is_const &= rhs_result.constant();

        // TODO struct is wrong. generally user-defined (could be array of
        // struct too, or perhaps a variant containing a struct?) need to
        // figure out the details here.
        const char *token = nullptr;
        switch (node->ops()[i]) {
          case frontend::Operator::Lt: token = "<"; break;
          case frontend::Operator::Le: token = "<="; break;
          case frontend::Operator::Eq: token = "=="; break;
          case frontend::Operator::Ne: token = "!="; break;
          case frontend::Operator::Ge: token = ">="; break;
          case frontend::Operator::Gt: token = ">"; break;
          default: UNREACHABLE();
        }

        if (lhs_result.type()->is<type::Struct>() or
            lhs_result.type()->is<type::Struct>()) {
          // TODO overwriting type a bunch of times?
          ast::OverloadSet os(node->scope_, token);
          AddAdl(&os, token, lhs_result.type());
          AddAdl(&os, token, rhs_result.type());
          return ast::VerifyDispatch(
              this, ast::ExprPtr{node->exprs()[i], 0x01}, os,
              core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                  {std::pair(node->exprs()[i], lhs_result),
                   std::pair(node->exprs()[i + 1], rhs_result)},
                  {}));
        }

        if (lhs_result.type() != rhs_result.type() and
            not(lhs_result.type()->is<type::Pointer>() and
                rhs_result.type() == type::NullPtr) and
            not(rhs_result.type()->is<type::Pointer>() and
                lhs_result.type() == type::NullPtr)) {
          NOT_YET("Log an error", lhs_result.type()->to_string(),
                  rhs_result.type()->to_string(), node);

        } else {
          auto cmp = Comparator(lhs_result.type());

          switch (node->ops()[i]) {
            case frontend::Operator::Eq:
            case frontend::Operator::Ne: {
              switch (cmp) {
                case Cmp::Order:
                case Cmp::Equality: continue;
                case Cmp::None:
                  error_log()->ComparingIncomparables(
                      lhs_result.type()->to_string(),
                      rhs_result.type()->to_string(),
                      frontend::SourceRange(node->exprs()[i]->span,
                                            node->exprs()[i + 1]->span));
                  return VerifyResult::Error();
              }
            } break;
            case frontend::Operator::Lt:
            case frontend::Operator::Le:
            case frontend::Operator::Ge:
            case frontend::Operator::Gt: {
              switch (cmp) {
                case Cmp::Order: continue;
                case Cmp::Equality:
                case Cmp::None:
                  error_log()->ComparingIncomparables(
                      lhs_result.type()->to_string(),
                      rhs_result.type()->to_string(),
                      frontend::SourceRange(node->exprs()[i]->span,
                                            node->exprs()[i + 1]->span));
                  return VerifyResult::Error();
              }
            } break;
            default: UNREACHABLE("Expecting a ChainOp operator type.");
          }
        }
      }

      return set_result(node, VerifyResult(type::Bool, is_const));
    }
  }
}

VerifyResult Compiler::Visit(ast::CommaList const *node, VerifyTypeTag) {
  ASSIGN_OR(
      return VerifyResult::Error(), auto results,
             VerifyWithoutSetting(
                 this, base::PtrSpan<ast::Expression const>(node->exprs_)));
  std::vector<type::Type const *> ts;
  ts.reserve(results.size());
  bool is_const = true;
  for (auto const &r : results) {
    ts.push_back(r.type());
    is_const &= r.constant();
  }
  return set_result(node, VerifyResult(type::Tup(std::move(ts)), is_const));
}

VerifyResult Compiler::Visit(ast::Declaration const *node, VerifyTypeTag) {
  // TODO swap module?

  // Declarations may have already been computed. Essentially the first time we
  // see an identifier (either a real identifier node, or a declaration, we need
  // to verify the type, but we only want to do node once.
  if (auto *attempt = prior_verification_attempt(node)) { return *attempt; }

  int dk = 0;
  if (node->IsInferred()) { dk = INFER; }
  if (node->IsUninitialized()) {
    dk |= UNINITIALIZED;
  } else if (node->IsCustomInitialized()) {
    dk |= CUSTOM_INIT;
  }
  type::Type const *node_type = nullptr;
  switch (dk) {
    case 0 /* Default initailization */: {
      ASSIGN_OR(return set_result(node, VerifyResult::Error()),
                       auto type_expr_result,
                       Visit(node->type_expr(), VerifyTypeTag{}));
      if (not type_expr_result.constant()) {
        // Hmm, not necessarily an error. Example (not necessarily minimal):
        //
        //   S ::= (x: any`T) => struct {
        //     _val: T = x
        //   }
        //
        NOT_YET("log an error", ast::Dump::ToString(node));
        return set_result(node, VerifyResult::Error());
      }
      auto *type_expr_type = type_expr_result.type();
      if (type_expr_type == type::Type_) {
        node_type = ASSERT_NOT_NULL(
            set_result(
                node,
                VerifyResult::Constant(backend::EvaluateAs<type::Type const *>(
                    MakeThunk(node->type_expr(), type_expr_type))))
                .type());

        if (not(node->flags() & ast::Declaration::f_IsFnParam) and
            not node_type->IsDefaultInitializable()) {
          error_log()->TypeMustBeInitialized(node->span,
                                             node_type->to_string());
        }

      } else {
        error_log()->NotAType(node->type_expr()->span,
                              type_expr_type->to_string());
        return set_result(node, VerifyResult::Error());
      }
    } break;
    case INFER: UNREACHABLE(); break;
    case INFER | CUSTOM_INIT: {
      ASSIGN_OR(return set_result(node, VerifyResult::Error()),
                       auto init_val_result,
                       Visit(node->init_val(), VerifyTypeTag{}));

      auto reason = Inferrable(init_val_result.type());
      if (reason != InferenceFailureReason::Inferrable) {
        error_log()->UninferrableType(reason, node->init_val()->span);
        return set_result(node, VerifyResult::Error());
      }

      // TODO initialization, not assignment.
      if (not VerifyAssignment(this, node->span, init_val_result.type(),
                               init_val_result.type())) {
        return set_result(node, VerifyResult::Error());
      }

      node_type =
          set_result(
              node, VerifyResult(init_val_result.type(),
                                 (node->flags() & ast::Declaration::f_IsConst)))
              .type();
    } break;
    case INFER | UNINITIALIZED: {
      error_log()->UninferrableType(InferenceFailureReason::Hole,
                                    node->init_val()->span);
      if (node->flags() & ast::Declaration::f_IsConst) {
        error_log()->UninitializedConstant(node->span);
      }
      return set_result(node, VerifyResult::Error());
    } break;
    case CUSTOM_INIT: {
      auto init_val_result = Visit(node->init_val(), VerifyTypeTag{});
      bool error           = not init_val_result.ok();

      auto *init_val_type   = Visit(node->init_val(), VerifyTypeTag{}).type();
      auto type_expr_result = Visit(node->type_expr(), VerifyTypeTag{});
      auto *type_expr_type  = type_expr_result.type();

      if (type_expr_type == nullptr) {
        error = true;
      } else if (type_expr_type == type::Type_) {
        if (not type_expr_result.constant()) {
          NOT_YET("log an error");
          error = true;
        } else {
          node_type =
              set_result(node,
                         VerifyResult::Constant(
                             backend::EvaluateAs<type::Type const *>(
                                 MakeThunk(node->type_expr(), type::Type_))))
                  .type();
        }

        // TODO initialization, not assignment. Error messages will be
        // wrong.
        if (node_type != nullptr and init_val_type != nullptr) {
          error |=
              not VerifyAssignment(this, node->span, node_type, init_val_type);
        }
      } else {
        error_log()->NotAType(node->type_expr()->span,
                              type_expr_type->to_string());
        error = true;
      }

      if (error) { return set_result(node, VerifyResult::Error()); }
    } break;
    case UNINITIALIZED: {
      ASSIGN_OR(return set_result(node, VerifyResult::Error()),
                       auto type_expr_result,
                       Visit(node->type_expr(), VerifyTypeTag{}));
      auto *type_expr_type = type_expr_result.type();
      if (type_expr_type == type::Type_) {
        if (not type_expr_result.constant()) {
          NOT_YET("log an error");
          return set_result(node, VerifyResult::Error());
        }
        node_type =
            set_result(
                node,
                VerifyResult::Constant(backend::EvaluateAs<type::Type const *>(
                    MakeThunk(node->type_expr(), type::Type_))))
                .type();
      } else {
        error_log()->NotAType(node->type_expr()->span,
                              type_expr_type->to_string());
        return set_result(node, VerifyResult::Error());
      }

      if (node->flags() & ast::Declaration::f_IsConst) {
        error_log()->UninitializedConstant(node->span);
        return set_result(node, VerifyResult::Error());
      }

    } break;
    default: UNREACHABLE(dk);
  }

  if (node->id().empty()) {
    if (node_type == type::Module) {
      // TODO check shadowing against other modules?
      // TODO what if no init val is provded? what if not constant?
      node->scope_->embedded_modules_.insert(
          backend::EvaluateAs<module::BasicModule const *>(
              MakeThunk(node->init_val(), type::Module)));
      return set_result(node, VerifyResult::Constant(type::Module));
    } else if (node_type->is<type::Tuple>()) {
      NOT_YET(node_type, ast::Dump::ToString(node));
    } else {
      NOT_YET(node_type, ast::Dump::ToString(node));
    }
  }

  // TODO simplify now that you don't have error decls.
  ASSERT(node_type != nullptr) << ast::Dump::ToString(node);
  std::vector<type::Typed<ast::Declaration const *>> decls_to_check;
  {
    auto good_decls_to_check = node->scope_->AllDeclsWithId(node->id());
    size_t num_total         = good_decls_to_check.size();
    auto iter                = node->scope_->child_decls_.find(node->id());

    bool has_children = (iter != node->scope_->child_decls_.end());
    if (has_children) { num_total += iter->second.size(); }

    decls_to_check.reserve(num_total);
    for (auto *decl : good_decls_to_check) {
      decls_to_check.emplace_back(decl, type_of(decl));
    }

    if (has_children) {
      for (auto *decl : iter->second) {
        decls_to_check.emplace_back(decl, type_of(decl));
      }
    }
  }

  auto iter = std::partition(decls_to_check.begin(), decls_to_check.end(),
                             [node](type::Typed<ast::Declaration const *> td) {
                               return node >= td.get();
                             });
  bool failed_shadowing = false;
  while (iter != decls_to_check.end()) {
    auto typed_decl = *iter;
    if (Shadow(this, type::Typed(node, node_type), typed_decl)) {
      failed_shadowing = true;
      error_log()->ShadowingDeclaration(node->span, (*typed_decl)->span);
    }
    ++iter;
  }

  if (failed_shadowing) {
    // TODO node may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else on
    // a different branch could find it unambiguously. It's also just a hack
    // from the get-go so maybe we should just do it the right way.
    return set_result(node, VerifyResult::Error());
  }

  return VerifySpecialFunctions(this, node, node_type);
}

VerifyResult Compiler::Visit(ast::EnumLiteral const *node, VerifyTypeTag) {
  for (auto const &elem : node->elems()) {
    if (auto *decl = elem->if_as<ast::Declaration>()) {
      auto *t = Visit(decl->init_val(), VerifyTypeTag{}).type();
      ASSERT(type::IsIntegral(t) == true);
      // TODO determine what is allowed here and how to generate errors.
    }
  }

  return set_result(node, VerifyResult::Constant(type::Type_));
}

VerifyResult Compiler::Visit(ast::FunctionLiteral const *node, VerifyTypeTag) {
  for (auto const &p : node->params()) {
    if ((p.value->flags() & ast::Declaration::f_IsConst) or
        not node->param_dep_graph_.at(p.value.get()).empty()) {
      return set_result(node, VerifyResult::Constant(type::Generic));
    }
  }

  return VerifyConcreteFnLit(node);
}

VerifyResult Compiler::Visit(ast::Identifier const *node, VerifyTypeTag) {
  for (auto iter = data_.cyc_deps_.begin(); iter != data_.cyc_deps_.end();
       ++iter) {
    if (*iter == node) {
      error_log()->CyclicDependency(
          std::vector<ast::Identifier const *>(iter, data_.cyc_deps_.end()));
      return VerifyResult::Error();
    }
  }
  data_.cyc_deps_.push_back(node);
  base::defer d([&] { data_.cyc_deps_.pop_back(); });

  // `node->decl()` is not necessarily null. Because we may call VerifyType many
  // times in multiple contexts, it is null the first time, but not on future
  // iterations.
  //
  // TODO that means we should probably resolve identifiers ahead of
  // type verification, but I think we rely on type information to figure it out
  // for now so you'll have to undo that first.
  if (node->decl() == nullptr) {
    auto potential_decls = node->scope_->AllDeclsWithId(node->token());
    switch (potential_decls.size()) {
      case 1: {
        // TODO could it be that evn though there is only one declaration,
        // there's a bound constant of the same name? If so, we need to deal
        // with node case.
        const_cast<ast::Identifier *>(node)->set_decl(potential_decls[0]);
        if (node->decl() == nullptr) { return VerifyResult::Error(); }
      } break;
      case 0:
        error_log()->UndeclaredIdentifier(node);
        return VerifyResult::Error();
      default:
        // TODO Should we allow the overload?
        error_log()->UnspecifiedOverload(node->span);
        return VerifyResult::Error();
    }

    if (not(node->decl()->flags() & ast::Declaration::f_IsConst) and
        node->span.begin() < node->decl()->span.begin()) {
      error_log()->DeclOutOfOrder(node->decl(), node);
    }
  }

  // TODO node is because we may have determined the declartaion previously with
  // a different generic setup but not bound the type for node context. But node
  // is wrong in the sense that the declaration bound is possibly dependent on
  // the context.
  type::Type const *t = type_of(node->decl());

  if (t == nullptr) { return VerifyResult::Error(); }
  return set_result(node, VerifyResult(t, node->decl()->flags() &
                                              ast::Declaration::f_IsConst));
}

VerifyResult Compiler::Visit(ast::Import const *node, VerifyTypeTag) {
  ASSIGN_OR(return _, auto result, Visit(node->operand(), VerifyTypeTag{}));
  bool err = false;
  if (result.type() != type::ByteView) {
    // TODO allow (import) overload
    error_log()->InvalidImport(node->operand()->span);
    err = true;
  }

  if (not result.constant()) {
    error_log()->NonConstantImport(node->operand()->span);
    err = true;
  }

  if (err) { return VerifyResult::Error(); }
  // TODO storing node might not be safe.
  auto src = backend::EvaluateAs<std::string_view>(
      MakeThunk(node->operand(), type::ByteView));
  // TODO source name?
  ASSIGN_OR(
      error_log()->MissingModule(src, std::filesystem::path{"TODO source"});
      return VerifyResult::Error(),  //
             auto pending_mod,
             module::ImportModule(std::filesystem::path{src}, module(),
                                  CompileModule));

  if (not pending_mod.valid()) { return VerifyResult::Error(); }
  set_pending_module(node, pending_mod);
  return set_result(node, VerifyResult::Constant(type::Module));
}

VerifyResult Compiler::Visit(ast::Index const *node, VerifyTypeTag) {
  auto lhs_result = Visit(node->lhs(), VerifyTypeTag{});
  auto rhs_result = Visit(node->rhs(), VerifyTypeTag{});
  if (not lhs_result.ok() or not rhs_result.ok()) {
    return VerifyResult::Error();
  }

  auto *index_type = rhs_result.type()->if_as<type::Primitive>();
  if (not index_type or not index_type->is_integral()) {
    error_log()->InvalidIndexType(node->span, lhs_result.type()->to_string(),
                                  lhs_result.type()->to_string());
  }

  if (lhs_result.type() == type::ByteView) {
    return set_result(node, VerifyResult(type::Nat8, rhs_result.constant()));
  } else if (auto *lhs_array_type = lhs_result.type()->if_as<type::Array>()) {
    return set_result(
        node, VerifyResult(lhs_array_type->data_type, rhs_result.constant()));
  } else if (auto *lhs_buf_type =
                 lhs_result.type()->if_as<type::BufferPointer>()) {
    return set_result(
        node, VerifyResult(lhs_buf_type->pointee, rhs_result.constant()));
  } else if (auto *tup = lhs_result.type()->if_as<type::Tuple>()) {
    if (not rhs_result.constant()) {
      error_log()->NonConstantTupleIndex(node->span);
      return VerifyResult::Error();
    }

    int64_t index = [&]() -> int64_t {
      auto results = backend::Evaluate(MakeThunk(node->rhs(), index_type));
      if (index_type == type::Int8) { return results.get<int8_t>(0).value(); }
      if (index_type == type::Int16) { return results.get<int16_t>(0).value(); }
      if (index_type == type::Int32) { return results.get<int32_t>(0).value(); }
      if (index_type == type::Int64) { return results.get<int64_t>(0).value(); }
      if (index_type == type::Nat8) { return results.get<uint8_t>(0).value(); }
      if (index_type == type::Nat16) {
        return results.get<uint16_t>(0).value();
      }
      if (index_type == type::Nat32) {
        return results.get<uint32_t>(0).value();
      }
      if (index_type == type::Nat64) {
        return results.get<uint64_t>(0).value();
      }
      UNREACHABLE();
    }();

    if (index < 0 or index >= static_cast<int64_t>(tup->size())) {
      error_log()->IndexingTupleOutOfBounds(node->span, tup->to_string(),
                                            tup->size(), index);
      return VerifyResult::Error();
    }

    return set_result(
        node, VerifyResult(tup->entries_.at(index), lhs_result.constant()));

  } else {
    error_log()->InvalidIndexing(node->span, lhs_result.type()->to_string());
    return VerifyResult::Error();
  }
}

VerifyResult Compiler::Visit(ast::Jump const *node, VerifyTypeTag) {
  for (auto const &option : node->options_) {
    for (std::unique_ptr<ast::Expression> const &expr : option.args) {
      Visit(expr.get(), VerifyTypeTag{});
    };
  }
  return VerifyResult::Constant(type::Void());
}

VerifyResult Compiler::Visit(ast::JumpHandler const *node, VerifyTypeTag) {
  DEBUG_LOG("JumpHandler")(ast::Dump::ToString(node));
  bool err = false;
  std::vector<type::Type const *> arg_types;
  arg_types.reserve(node->input().size());
  for (auto const &input : node->input()) {
    auto v = Visit(input, VerifyTypeTag{});
    if (not v.ok()) {
      err = true;
    } else {
      arg_types.push_back(v.type());
    }
  }

  return set_result(node, err ? VerifyResult::Error()
                              : VerifyResult::Constant(type::Jmp(arg_types)));
}

VerifyResult Compiler::Visit(ast::PrintStmt const *node, VerifyTypeTag) {
  auto verify_results = VerifySpan(this, node->exprs());
  if (not verify_results) { return VerifyResult::Error(); }
  for (auto &verify_result : *verify_results) {
    auto *expr_type = verify_result.second.type();
    // TODO print arrays?
    if (expr_type->is<type::Primitive>() or expr_type->is<type::Pointer>() or
        expr_type == type::ByteView or expr_type->is<type::Enum>() or
        expr_type->is<type::Flags>()) {
      continue;
    } else {
      ast::OverloadSet os(node->scope_, "print");
      AddAdl(&os, "print", expr_type);
      // TODO using expr.get() for the dispatch table is super janky. node is
      // used so we don't collide with the table for the actual expression as
      // `print f(x)` needs a table both for the printing and for the call to
      // `f`. Test node thoroughly.
      auto dispatch_result = ast::VerifyDispatch(
          this, ast::ExprPtr{verify_result.first, 0x01}, os,
          core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
              {verify_result}, {}));
      if (dispatch_result.type() and dispatch_result.type() != type::Void()) {
        error_log()->PrintMustReturnVoid(dispatch_result.type()->to_string(),
                                         node->span);
        return VerifyResult::Error();
      }
    }
  }
  return VerifyResult::NonConstant(type::Void());
}

VerifyResult Compiler::Visit(ast::ReturnStmt const *node, VerifyTypeTag) {
  auto c = VerifyAndGetConstness(this, node->exprs());
  if (c == Constness::Error) { return VerifyResult::Error(); }
  return c == Constness::Const ? VerifyResult::Constant(type::Void())
                               : VerifyResult::NonConstant(type::Void());
}

VerifyResult Compiler::Visit(ast::YieldStmt const *node, VerifyTypeTag) {
  auto c = VerifyAndGetConstness(this, node->exprs());
  if (c == Constness::Error) { return VerifyResult::Error(); }
  return c == Constness::Const ? VerifyResult::Constant(type::Void())
                               : VerifyResult::NonConstant(type::Void());
}

VerifyResult Compiler::Visit(ast::ScopeLiteral const *node, VerifyTypeTag) {
  auto verify_result = set_result(node, VerifyResult::Constant(type::Scope));
  bool error         = false;
  for (auto const *decl : node->decls()) {
    auto result = Visit(decl, VerifyTypeTag{});
    if (not result.constant()) {
      error = true;
      NOT_YET("log an error");
    }
  }
  // TODO verify that it has at least one entry and exit point each.
  if (error) { return VerifyResult::Error(); }
  return verify_result;
}

VerifyResult Compiler::Visit(ast::ScopeNode const *node, VerifyTypeTag) {
  // TODO how do you determine the type of this?
  ASSIGN_OR(return _, auto name_result, Visit(node->name(), VerifyTypeTag{}));
  static_cast<void>(name_result);

  auto arg_results = node->args().Transform([this](ast::Expression const *arg) {
    return std::pair{arg, Visit(arg, VerifyTypeTag{})};
  });

  // TODO later on you'll want to allow dynamic dispatch. Calling a scope on an
  // argument of type `A | B` where there are two scope objects (one of type `A`
  // and one of type `B`) should work. But this necessitates evaluating not as a
  // ScopeDef but as an overload set.
  //
  // Then on each possibility, you should first check that all the block names
  // are available on all possible scopes.
  //
  // TODO you may not be able to compute this ahead of time. It relies on
  // computing all the block handlers which may be generics.
  //
  // TODO is the scope type correct here?
  auto *scope_def =
      backend::EvaluateAs<ir::ScopeDef *>(MakeThunk(node->name(), type::Scope));
  if (scope_def->work_item and *scope_def->work_item) {
    (std::move(*scope_def->work_item))();
  }

  bool err = false;
  std::vector<ir::BlockDef const *> block_defs;
  for (auto const &block : node->blocks()) {
    DEBUG_LOG("ScopeNode")("Verifying dispatch for block `", block.name(), "`");
    auto block_results = VerifyBlockNode(this, &block);
    auto *block_def    = scope_def->blocks_.at(block.name());
    DEBUG_LOG("ScopeNode")("    ", block_results);
    if (block_results.empty()) {
      DEBUG_LOG("ScopeNode")("    ... empty block results");
      auto result = ast::VerifyJumpDispatch(this, node, block_def->after_, {},
                                            &block_defs);
      static_cast<void>(result);
      DEBUG_LOG("ScopeNode")("    ... dispatch result = ", result);
    } else {
      for (auto const &fn_args : block_results) {
        auto result = ast::VerifyJumpDispatch(this, node, block_def->after_,
                                              fn_args, &block_defs);
        static_cast<void>(result);
        DEBUG_LOG("ScopeNode")("    ... dispatch result = ", result);
      }
    }
    DEBUG_LOG("ScopeNode")("    ... done.");
  }
  auto init_result = ast::VerifyJumpDispatch(this, node, scope_def->inits_,
                                             arg_results, &block_defs);
  static_cast<void>(init_result);
  DEBUG_LOG("ScopeNode")("    ... init_result = ", init_result);
  DEBUG_LOG("ScopeNode")("    ... block_defs = ", block_defs);
  return VerifyResult::Constant(type::Void());
}

VerifyResult Compiler::Visit(ast::StructLiteral const *node, VerifyTypeTag) {
  std::vector<type::Type const *> ts;
  ts.reserve(node->args_.size());
  for (auto &a : node->args_) {
    ts.push_back(Visit(&a, VerifyTypeTag{}).type());
  }
  if (absl::c_any_of(ts, [](type::Type const *t) { return t == nullptr; })) {
    return VerifyResult::Error();
  }

  if (node->args_.empty()) {
    bool is_const = true;
    bool err      = false;
    for (auto const &field : node->fields_) {
      if (not field.type_expr()) { continue; }
      auto result = Visit(field.type_expr(), VerifyTypeTag{});
      if (not result) {
        err = true;
        continue;
      }
      is_const &= result.constant();
      if (field.init_val()) {
        if (result.constant()) {
          auto init_val_result = Visit(field.init_val(), VerifyTypeTag{});
          if (not init_val_result.constant()) {
            error_log()->NonConstantStructFieldDefaultValue(
                field.init_val()->span);
            err = true;
          }

          if (init_val_result.type() != result.type()) {
            NOT_YET("type mismatch");
          }

        } else {
          NOT_YET(
              "can't have an initial value set if the type is also present and "
              "non-constant");
        }
      }
    }
    if (err) { return set_result(node, VerifyResult::Error()); }
    return set_result(node, VerifyResult::Constant(type::Type_));
    // TODO, we need to verify the body of this struct at some point, but it may
    // be dependent on things we can't evaluate yet. For example,
    //
    // wrapper ::= (T: type) => struct { val: T }
    //
    // I'm yet unsure exactly how we should handle this.
    /*
    bool ok = absl::c_all_of(node->fields_, [this](ast::Declaration const
    &field) {
      // TODO you should verify each field no matter what.
      Visit(&field, VerifyTypeTag{});
      if (not field.init_val() or
          ASSERT_NOT_NULL(prior_verification_attempt(field.init_val()))
              ->constant()) {
        return true;
      }
      error_log()->NonConstantStructFieldDefaultValue(
          field.init_val()->span);
      return false;
    });
    // TODO so in fact we could recover here and just not emit ir but we're no
    // longer set up to do that.
    if (ok) {
      return set_result(node, VerifyResult::Constant(type::Type_));
    } else {
      return VerifyResult::Error();
    }
    */
  } else {
    return set_result(node, VerifyResult::Constant(
                                type::GenStruct(node->scope_, std::move(ts))));
  }
}

VerifyResult Compiler::Visit(ast::StructType const *node, VerifyTypeTag) {
  for (auto &arg : node->args_) { Visit(arg.get(), VerifyTypeTag{}); }
  return set_result(node, VerifyResult::Constant(type::Type_));
}

VerifyResult Compiler::Visit(ast::Switch const *node, VerifyTypeTag) {
  bool is_const               = true;
  type::Type const *expr_type = nullptr;
  if (node->expr_) {
    ASSIGN_OR(return _, auto result, Visit(node->expr_.get(), VerifyTypeTag{}));
    is_const &= result.constant();
    expr_type = result.type();
  }

  absl::flat_hash_set<type::Type const *> types;
  bool err = false;
  for (auto &[body, cond] : node->cases_) {
    auto cond_result = Visit(cond.get(), VerifyTypeTag{});
    auto body_result = Visit(body.get(), VerifyTypeTag{});
    err |= not cond_result or not body_result;
    if (err) {
      NOT_YET();
      continue;
    }

    is_const &= cond_result.constant() and body_result.constant();
    if (node->expr_) {
      static_cast<void>(expr_type);
      // TODO dispatch table
    } else {
      if (cond_result.type() != type::Bool) {
        error_log()->SwitchConditionNeedsBool(cond_result.type()->to_string(),
                                              node->span);
      }
    }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    if (body->is<ast::Expression>()) {
      // TODO check that it's actually a jump
      types.insert(body_result.type());
    }
  }
  if (err) { return VerifyResult::Error(); }

  // TODO check to ensure that the type is either exhaustable or has a default.

  if (types.empty()) {
    return set_result(node, VerifyResult(type::Void(), is_const));
  }
  auto some_type = *types.begin();
  if (absl::c_all_of(types,
                     [&](type::Type const *t) { return t == some_type; })) {
    // TODO node might be a constant.
    return set_result(node, VerifyResult(some_type, is_const));
  } else {
    NOT_YET("handle type error");
    return VerifyResult::Error();
  }
}

VerifyResult Compiler::Visit(ast::Terminal const *node, VerifyTypeTag) {
  return set_result(node,
                    VerifyResult::Constant(type::Prim(node->basic_type())));
}

VerifyResult Compiler::Visit(ast::Unop const *node, VerifyTypeTag) {
  ASSIGN_OR(return VerifyResult::Error(), auto result,
                   Visit(node->operand(), VerifyTypeTag{}));
  auto *operand_type = result.type();

  switch (node->op()) {
    case frontend::Operator::Copy:
      if (not operand_type->IsCopyable()) {
        NOT_YET("log an error. not copyable");
      }
      // TODO Are copies always consts?
      return set_result(node, VerifyResult(operand_type, result.constant()));
    case frontend::Operator::Move:
      if (not operand_type->IsMovable()) {
        NOT_YET("log an error. not movable");
      }
      // TODO Are copies always consts?
      return set_result(node, VerifyResult(operand_type, result.constant()));
    case frontend::Operator::BufPtr:
      return set_result(node, VerifyResult(type::Type_, result.constant()));
    case frontend::Operator::TypeOf:
      return set_result(node, VerifyResult(type::Type_, result.constant()));
    case frontend::Operator::Eval:
      if (not result.constant()) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do node probably worth a
        // full pass over all verification code.
        error_log()->NonConstantEvaluation(node->operand()->span);
        return VerifyResult::Error();
      } else {
        return set_result(node, VerifyResult(operand_type, result.constant()));
      }
    case frontend::Operator::Which:
      if (not operand_type->is<type::Variant>()) {
        error_log()->WhichNonVariant(operand_type->to_string(), node->span);
      }
      return set_result(node, VerifyResult(type::Type_, result.constant()));
    case frontend::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        return set_result(
            node, VerifyResult(operand_type->as<type::Pointer>().pointee,
                               result.constant()));
      } else {
        error_log()->DereferencingNonPointer(operand_type->to_string(),
                                             node->span);
        return VerifyResult::Error();
      }
    case frontend::Operator::And:
      // TODO  does it make sense to take the address of a constant? I think it
      // has to but it also has to have some special meaning. Things we take the
      // address of in run-time code need to be made available at run-time.
      return set_result(
          node, VerifyResult(type::Ptr(operand_type), result.constant()));
    case frontend::Operator::Mul:
      if (operand_type != type::Type_) {
        NOT_YET("log an error, ", operand_type, node);
        return VerifyResult::Error();
      } else {
        return set_result(node, VerifyResult(type::Type_, result.constant()));
      }
    case frontend::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        return set_result(node, VerifyResult(operand_type, result.constant()));
      } else if (operand_type->is<type::Struct>()) {
        ast::OverloadSet os(node->scope_, "-");
        AddAdl(&os, "-", operand_type);
        return ast::VerifyDispatch(
            this, node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair(node->operand(), result)}, {}));
      }
      NOT_YET();
      return VerifyResult::Error();
    case frontend::Operator::Expand:
      // NOTE: It doesn't really make sense to ask for the type of an expanded
      // argument, but since we consider the type of the result of a function
      // call returning multiple arguments to be a tuple, we do the same here.
      //
      if (operand_type->is<type::Tuple>()) {
        // TODO there should be a way to avoid copying over any of entire type
        return set_result(node, VerifyResult(operand_type, result.constant()));
      } else {
        NOT_YET();  // Log an error. can't expand a non-tuple.
      }
    case frontend::Operator::Not:
      if (operand_type == type::Bool or operand_type->is<type::Enum>() or
          operand_type->is<type::Flags>()) {
        return set_result(node, VerifyResult(operand_type, result.constant()));
      }
      if (operand_type->is<type::Struct>()) {
        ast::OverloadSet os(node->scope_, "!");
        AddAdl(&os, "!", operand_type);
        return ast::VerifyDispatch(
            this, node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair(node->operand(), result)}, {}));
      } else {
        NOT_YET("log an error");
        return VerifyResult::Error();
      }
    case frontend::Operator::Needs:
      if (operand_type != type::Bool) {
        error_log()->PreconditionNeedsBool(node->operand()->span,
                                           operand_type->to_string());
      }
      if (not result.constant()) { NOT_YET(); }
      return set_result(node, VerifyResult::Constant(type::Void()));
    case frontend::Operator::Ensure:
      if (operand_type != type::Bool) {
        error_log()->PostconditionNeedsBool(node->operand()->span,
                                            operand_type->to_string());
      }
      if (not result.constant()) { NOT_YET(); }
      return set_result(node, VerifyResult::Constant(type::Void()));
    case frontend::Operator::VariadicPack: {
      if (not result.constant()) { NOT_YET("Log an error"); }
      // TODO could be a type, or a function returning a ty
      if (result.type() == type::Type_) {
        return set_result(node, VerifyResult::Constant(type::Type_));
      } else if (result.type()->is<type::Function>()) {
        NOT_YET();
      }
      NOT_YET(*node);
    }
    default: UNREACHABLE(*node);
  }
}

}  // namespace compiler
