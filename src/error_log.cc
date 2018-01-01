#include "error_log.h"

#include <iostream>

#include "ast/ast.h"
#include "base/source.h"
#include "type/type.h"

extern std::unordered_map<Source::Name, File *> source_map;

using LineNum    = size_t;
using LineOffset = size_t;
using Token      = std::string;

using TokenToErrorMap = std::unordered_map<
    Token,
    std::unordered_map<Source::Name,
                       std::unordered_map<LineNum, std::vector<LineOffset>>>>;
using FileToLineNumMap = std::unordered_map<Source::Name, std::vector<LineNum>>;
using DeclToErrorMap   = std::unordered_map<
    const AST::Declaration *,
    std::unordered_map<Source::Name,
                       std::unordered_map<LineNum, std::vector<LineOffset>>>>;
using TextSpanToErrorMap =
    std::map<TextSpan, std::unordered_map<LineNum, std::vector<LineOffset>>>;

static TokenToErrorMap undeclared_identifiers, ambiguous_identifiers;
static DeclToErrorMap implicit_capture;
static TextSpanToErrorMap case_errors;
static FileToLineNumMap global_non_decl;

size_t ErrorLog::num_errs_ = 0;

static inline size_t NumDigits(size_t n) {
  if (n == 0) { return 1; }
  size_t counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

static inline std::string NumTimes(size_t n) {
  switch (n) {
  case 1: return "once";
  case 2: return "twice";
  default: return std::to_string(n) + " times";
  }
}

static size_t
CountUses(const std::unordered_map<
          Source::Name, std::unordered_map<LineNum, std::vector<LineOffset>>>
              &map) {
  size_t num_uses = 0;
  for (const auto & [ file, locs ] : map) {
    for (const auto & [ line, offsets ] : locs) { num_uses += offsets.size(); }
  }
  return num_uses;
}

static void WriteMessage(const std::string &msg, const std::string &token,
                         size_t num_uses) {
  std::cerr << msg << " '" << token << '\'';
  if (num_uses != 1) { std::cerr << " used " << NumTimes(num_uses); }
  std::cerr << ".\n";
}

static void GatherAndDisplayIdentifierError(
    const std::unordered_map<
        Source::Name, std::unordered_map<LineNum, std::vector<LineOffset>>>
        &map) {
  for (const auto &[file, locs] : map) {
    size_t max_line_num = 0;
    for (const auto & [ line, offsets ] : locs) {
      if (max_line_num < line) { max_line_num = line; }
    }
    // size_t line_num_width = NumDigits(max_line_num);

    size_t num_uses_in_file = 0;
    for (const auto & [ line, offsets ] : locs) {
      num_uses_in_file += offsets.size();
    }

    std::cerr << "  Used " << NumTimes(num_uses_in_file) << " in '" << file
              << "':\n";

    for (const auto &[line, offsets] : locs) {
      auto line_text = source_map AT(file)->lines AT(line);
      // TODO alignment
      std::cerr << line << "| " << line_text << '\n'
                << std::string(line_text.size() + 1, '^');
    }
  }
}

static void GatherAndDisplayGlobalDeclErrors() {
  if (global_non_decl.empty()) { return; }

  size_t num_instances = 0;
  for (const auto &[key, val] : global_non_decl) { num_instances += val.size(); }

  std::cerr << "Found " << num_instances << " non-declaration statement"
            << (num_instances == 1 ? "" : "s")
            << " at the top level. All top-level statements must either be "
               "declarations, imports, or void compile-time evaluations.";

  for (const auto &[key, val] : global_non_decl) {
    std::cerr << "  Found " << val.size()
              << (val.size() == 1 ? " instance in '" : " instances in '")
              << key << "':\n";

    int line_num_width   = (int)NumDigits(val.back());
    size_t last_line_num = val.front();
    for (auto line_num : val) {
      // TODO alignment
      if (line_num - last_line_num == 2) {
        auto line = source_map AT(key)->lines AT(line_num - 1);
        std::cerr << (line_num - 1) << "| " << line << '\n';
      } else if (line_num - last_line_num == 3) {
        auto line = source_map AT(key)->lines AT(line_num - 1);
        std::cerr << (line_num - 1) << "| " << line << '\n';
        line = source_map AT(key)->lines AT(line_num - 2);
        std::cerr << (line_num - 2) << "| " << line << '\n';
      } else if (line_num - last_line_num > 3) {
        std::cerr << std::string(static_cast<size_t>(line_num_width) + 1, ' ')
                  << "...|\n";
      }

      // TODO alignment
      auto line = source_map AT(key)->lines AT(line_num);
      std::cerr << ">" << line_num << "| " << line << '\n';
      last_line_num = line_num;
    }
  }
}

void ErrorLog::Dump() {
  for (const auto &[key, val] : undeclared_identifiers) {
    WriteMessage("Undeclared identifier", key, CountUses(val));
    GatherAndDisplayIdentifierError(val);
  }
  for (const auto &[key, val] : ambiguous_identifiers) {
    WriteMessage("Ambiguous identifier", key, CountUses(val));
    GatherAndDisplayIdentifierError(val);
  }

  for (const auto &[key, val] : implicit_capture) {
    WriteMessage("Invalid capture of identifier", key->identifier->token,
                 CountUses(val));
    GatherAndDisplayIdentifierError(val);
  }

  // TODO fix this repsonse regarding imports.
  GatherAndDisplayGlobalDeclErrors();

  std::cerr << num_errs_ << " error";
  if (num_errs_ != 1) { std::cerr << "s"; }

  std::cerr << " found." << std::endl;
}

static std::string LineToDisplay(size_t line_num, const Source::Line &line,
                                 size_t border_alignment = 0) {
  auto num_digits = NumDigits(line_num);
  if (border_alignment == 0) { border_alignment = num_digits; }
  ASSERT_GE(border_alignment, num_digits);
  return std::string(border_alignment - num_digits, ' ') +
         std::to_string(line_num) + "| " + line.to_string() + "\n";
}

static void DisplayErrorMessage(const char *msg_head,
                                const std::string &msg_foot,
                                const TextSpan &span, size_t underline_length) {
  auto line = source_map AT(span.source->name)->lines AT(span.start.line_num);

  size_t left_border_width = NumDigits(span.start.line_num) + 6;

  // Extra + 1 at the end because we might point after the end of the line.
  std::string underline(line.size() + left_border_width + 1, ' ');
  for (size_t i = left_border_width + span.start.offset;
       i < left_border_width + span.start.offset + underline_length; ++i) {
    underline[i] = '^';
  }

  std::cerr << msg_head << "\n\n    "
            << LineToDisplay(span.start.line_num, line) << underline << '\n';
  if (msg_foot != "") {
    std::cerr << msg_foot << "\n\n";
  } else {
    std::cerr << '\n';
  }
}

namespace ErrorLog {
void NullCharInSrc(const TextSpan &span) {
  std::cerr << "I found a null-character in your source file on line "
            << span.start.line_num
            << ". I am ignoring it and moving on. Are you sure \""
            << span.source->name << "\" is a source file?\n\n";
  ++num_errs_;
}

void NonGraphicCharInSrc(const TextSpan &span) {
  std::cerr << "I found a non-graphic character in your source file on line "
            << span.start.line_num
            << ". I am ignoring it and moving on. Are you sure \""
            << span.source->name << "\" is a source file?\n\n";
  ++num_errs_;
}

void LogGeneric(const TextSpan &, const std::string &msg) {
  ++num_errs_;
  std::cerr << msg;
}

void InvalidRangeType(const TextSpan &span, Type *t) {
  ++num_errs_;
  std::string msg_foot = "Expected type: int, uint, or char\n"
                         "Given type: " +
                         t->to_string() + ".";
  DisplayErrorMessage("Attempting to create a range with an invalid type.",
                      msg_foot, span, t->to_string().size());
}

void TooManyDots(const TextSpan &span, size_t num_dots) {
  const char *msg_head = "There are too many consecutive '.' characters. I am "
                         "assuming you meant \"..\".";
  ++num_errs_;
  DisplayErrorMessage(msg_head, "", span, num_dots);
}

void ShadowingDeclaration(const AST::Declaration &decl1,
                          const AST::Declaration &decl2) {
  auto line1 = source_map AT(decl1.span.source->name)
                   ->lines AT(decl1.span.start.line_num);
  auto line2 = source_map AT(decl2.span.source->name)
                   ->lines AT(decl2.span.start.line_num);
  auto line_num1 = decl1.span.start.line_num;
  auto line_num2 = decl2.span.start.line_num;
  auto align =
      std::max(size_t{4}, NumDigits(std::max(line_num1, line_num2)) + 2);
  ++num_errs_;
  std::cerr << "Ambiguous declarations:\n\n"
            << LineToDisplay(line_num1, line1, align) << '\n'
            << LineToDisplay(line_num2, line2, align) << '\n';
}

void NonWhitespaceAfterNewlineEscape(const TextSpan &span, size_t dist) {
  const char *msg_head =
      "I found a non-whitespace character following a '\\' on the same line.";
  const char *msg_foot = "Backslashes are used to ignore line-breaks and "
                         "therefore must be followed by a newline (whitespace "
                         "between the backslash and the newline is okay).";

  ++num_errs_;
  DisplayErrorMessage(msg_head, msg_foot, span, dist);
}

void RunawayMultilineComment() {
  std::cerr << "Finished reading file during multi-line comment.\n\n";
  ++num_errs_;
}

void InvalidStringIndex(const TextSpan &span, Type *index_type) {
  std::string msg_head = "String indexed by an invalid type. Expected an int "
                         "or uint, but encountered a " +
                         index_type->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}
void NotAType(AST::Expression *expr, Type *t) {
  ++num_errs_;
  std::string msg_head =
      "Expression was expected to be a type, but instead it was a(n) " +
      t->to_string() + ".";
  DisplayErrorMessage(msg_head.c_str(), "", expr->span, 1);
}

void IndeterminantType(AST::Expression *expr) {
  ++num_errs_;
  DisplayErrorMessage("Cannot determine the type of the expression:", "",
                      expr->span, 1);
}

void CyclicDependency(AST::Node *node) {
  ++num_errs_;
  DisplayErrorMessage("This expression's type is declared self-referentially.",
                      "", node->span, 1);
}

void GlobalNonDecl(const TextSpan &span) {
  ++num_errs_;
  global_non_decl[span.source->name].push_back(span.start.line_num);
}

void NonIntegralArrayIndex(const TextSpan &span, const Type *index_type) {
  std::string msg_head = "Array is being indexed by an expression of type " +
                         index_type->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(
      msg_head.c_str(),
      "Arrays must be indexed by integral types (either int or uint)", span, 1);
}

void DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id) {
  std::string msg_head =
      "Identifier '" + id->token + "' used before it was declared.";
  std::string msg_foot = "Declaration can be found on line " +
                         std::to_string(decl->span.start.line_num) + ".";
  // TODO Provide file name as well.
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), msg_foot, id->span, id->token.size());
}

void InvalidAddress(const TextSpan &span, Assign mode) {
  ++num_errs_;
  if (mode == Assign::Const) {
    DisplayErrorMessage(
        "Attempting to take the address of an identifier marked as #const",
        "Marking an identifier as #const means that it is a compile-time "
        "constant. The value may not actually be associated with a storage "
        "address. For this reason, it is illegal to take the address.",
        span, 1);
  } else if (mode == Assign::RVal) {
    DisplayErrorMessage(
        "Attempting to take the address of a temporary expression.", "", span,
        1);
  } else {
    UNREACHABLE();
  }
}

void InvalidAssignment(const TextSpan &span, Assign mode) {
  ++num_errs_;
  if (mode == Assign::Const) {
    DisplayErrorMessage(
        "Attempting to assign to an identifier marked as #const",
        "Marking an identifier as #const means that it is a compile-time "
        "constant and cannot be modified at run-time.",
        span, 1);
  } else if (mode == Assign::RVal) {
    DisplayErrorMessage("Attempting to assign to a temporary expression.",
                        "", span, 1);
  } else {
    UNREACHABLE();
  }
}

void CaseLHSBool(const TextSpan &, const TextSpan &span, const Type *t) {
  std::string msg_head = "In a case statement, the lefthand-side of a case "
                         "must have type bool. However, the expression has "
                         "type " +
                         t->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void MissingMember(const TextSpan &span, const std::string &member_name,
                   const Type *t) {
  ++num_errs_;
  std::string msg_head = "Expressions of type `" + t->to_string() +
                         "` have no member named '" + member_name + "'.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void IndexingNonArray(const TextSpan &span, const Type *t) {
  ++num_errs_;
  DisplayErrorMessage("Cannot index into a non-array type.",
                      "Indexed type is a `" + t->to_string() + "`.", span, 1);
}

void UnopTypeFail(const std::string &msg, const AST::Unop *unop) {
  ++num_errs_;
  DisplayErrorMessage(msg.c_str(), "", unop->span, 1);
}

void SlicingNonArray(const TextSpan &span, const Type *t) {
  ++num_errs_;
  DisplayErrorMessage("Cannot slice a non-array type.",
                      "Sliced type is a `" + t->to_string() + "`.", span, 1);
}

void NonBinaryAssignment(const TextSpan &span, size_t len) {
  ++num_errs_;
  std::string msg_head = "Assignment must be a binary operator, but " +
                         std::to_string(len) + " argument" +
                         (len == 1 ? " was" : "s were") + " given.";
  // TODO is underline length correct?
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void AssignmentArrayLength(const TextSpan &span, size_t len) {
  ++num_errs_;
  std::string msg_head = "Invalid assignment. Array on right-hand side has "
                         "unknown length, but lhs is known to be of length " +
                         std::to_string(len) + ".";
  // TODO is underline length correct?
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void AlreadyFoundMatch(const TextSpan &span, const std::string &op_symbol,
                       const Type *lhs, const Type *rhs) {
  ++num_errs_;
  std::string msg_head = "Already found a match for operator `" + op_symbol +
                         "` with types " + lhs->to_string() + " and " +
                         rhs->to_string() + ".";

  // TODO underline length is incorrect?
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void NoKnownOverload(const TextSpan &span, const std::string &op_symbol,
                     const Type *lhs, const Type *rhs) {
  ++num_errs_;
  std::string msg_head = "No known operator overload for operator `" +
                         op_symbol + "` with types " + lhs->to_string() +
                         " and " + rhs->to_string() + ".";
  // TODO underline length is incorrect?
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void InvalidRangeTypes(const TextSpan &span, const Type *lhs, const Type *rhs) {
  ++num_errs_;
  std::string msg_head = "No range construction for types " + lhs->to_string() +
                         " .. " + rhs->to_string() + ".";
  // TODO underline length is incorrect?
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void AssignmentTypeMismatch(const TextSpan &span, const Type *lhs,
                            const Type *rhs) {
  ++num_errs_;
  std::string msg_head = "Invalid assignment. Left-hand side has type " +
                         lhs->to_string() + ", but right-hand side has type " +
                         rhs->to_string() + ".";
  // TODO underline isn't what it ought to be.
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void InvalidCast(const TextSpan &span, const Type *from, const Type *to) {
  ++num_errs_;
  std::string msg_head = "No valid cast from `" + from->to_string() + "` to `" +
                         to->to_string() + "`.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 2);
}

void NotAType(const TextSpan &span, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head = "In declaration of `" + id_tok +
                         "`, the declared type is not a actually a type.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void DeclaredVoidType(const TextSpan &span, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head =
      "Identifier `" + id_tok + "`is declared to have type `void`.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void DeclaredParametricType(const TextSpan &span, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head =
      "In declaration of `" + id_tok +
      "`, the declared type is parametric and has no parameters provided.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void DoubleDeclAssignment(const TextSpan &decl_span, const TextSpan &val_span) {
  ++num_errs_;
  if (decl_span.start.line_num == val_span.start.line_num) {
    DisplayErrorMessage("Attempting to initialize an identifier that already "
                        "has an initial value.",
                        "", decl_span, 1);
  } else {
    NOT_YET();
  }
}
void InvalidPrintDefinition(const TextSpan &span, const Type *t) {
  ++num_errs_;
  std::string msg_head =
      "Cannot define print function for type " + t->to_string() + ".";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void InitWithNull(const TextSpan &span, const Type *t, const Type *intended) {
  ++num_errs_;
  std::string msg_head = "Cannot initialize an identifier of type " +
                         t->to_string() +
                         " with null. Did you mean to declare it as " +
                         intended->to_string() + "?";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void InvalidAssignDefinition(const TextSpan &span, const Type *t) {
  ++num_errs_;
  std::string msg_head =
      "Cannot define assignment function for type " + t->to_string() + ".";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void InvalidScope(const TextSpan &span, const Type *t) {
  ++num_errs_;
  std::string msg_head =
      "Object of type '" + t->to_string() + "' used as if it were a scope.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void NotBinary(const TextSpan &span, const std::string &token) {
  ++num_errs_;
  std::string msg_head = "Operator '" + token + "' is not a binary operator.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void Reserved(const TextSpan &span, const std::string &token) {
  ++num_errs_;
  std::string msg_head = "Identifier '" + token + "' is a reserved keyword.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

// TODO better error message for repeated enum name
#define ERROR_MACRO(fn_name, msg_head, msg_foot, underline_length)             \
  void fn_name(const TextSpan &span) {                                         \
    ++num_errs_;                                                               \
    DisplayErrorMessage(msg_head, msg_foot, span, underline_length);           \
  }
#include "config/error.conf"
#undef ERROR_MACRO

void InvalidReturnType(const TextSpan &span, Type *given, Type *correct) {
  ++num_errs_;
  std::string msg_head = "Invalid return type on line " +
                         std::to_string(span.start.line_num) + " in \"" +
                         span.source->name.c_str() + "\".";
  std::string msg_foot = "Given return type:    " + given->to_string() +
                         "\n"
                         "Expected return type: " +
                         correct->to_string();
  DisplayErrorMessage(msg_head.c_str(), msg_foot, span,
                      span.source->lines[span.start.line_num].size() -
                          span.start.offset);
}

void ChainTypeMismatch(const TextSpan &span, std::set<Type *> types) {
  ++num_errs_;
  std::stringstream ss;
  ss << "Found the following types in the expression:\n";
  for (auto t : types) { ss << "  * " << t->to_string() << "\n"; }
  DisplayErrorMessage("Type do not all match in expression:", ss.str(), span,
                      1);
}

void UserDefinedError(const TextSpan &span, const std::string &msg) {
  ++num_errs_;
  DisplayErrorMessage(msg.c_str(), "", span, 1);
}

static void DisplayLines(const std::vector<TextSpan> &lines) {
  size_t left_space     = NumDigits(lines.back().start.line_num) + 2;
  std::string space_fmt = std::string(left_space - 3, ' ') + "...|\n";

  size_t last_line_num = lines[0].start.line_num - 1;
  for (auto span : lines) {
    if (span.start.line_num != last_line_num + 1) {
      std::cerr << space_fmt << '\n';
    }
    // TODO alignment
    std::cerr << LineToDisplay(span.start.line_num,
                               span.source->lines[span.start.line_num],
                               left_space);
    last_line_num = span.start.line_num;
  }
  std::cerr << '\n';
}

void CaseTypeMismatch(AST::Case *case_ptr, Type *correct) {
  if (correct) {
    std::cerr << "Type mismatch in case-expression on line "
              << case_ptr->span.start.line_num << " in \""
              << case_ptr->span.source->name.to_string() << "\".\n";

    std::vector<TextSpan> locs;
    for (auto &[key, val] : case_ptr->key_vals) {
      ++num_errs_;
      if (val->type == Err || val->type == correct) { continue; }
      locs.push_back(val->span);
    }

    num_errs_ += locs.size();
    DisplayLines(locs);
    std::cerr << "Expected an expression of type " << correct->to_string()
              << ".\n\n";

  } else {
    ++num_errs_;
    std::cerr << "Type mismatch in case-expression on line "
              << case_ptr->span.start.line_num << " in \""
              << case_ptr->span.source->name.to_string() << "\".\n";
  }
}

void UnknownParserError(const Source::Name &source_name,
                        const std::vector<TextSpan> &lines) {
  if (lines.empty()) {
    std::cerr << "Parse errors found in \"" << source_name.to_string()
              << "\". Sorry I can't be more specific.";
    ++num_errs_;
    return;
  }

  num_errs_ += lines.size();
  std::cerr << "Parse errors found in \"" << source_name.to_string()
            << "\" on the following lines:\n\n";
  DisplayLines(lines);
}

void UninferrableType(const TextSpan &span) {
  ++num_errs_;
  DisplayErrorMessage("Expression cannot have it's type inferred", "", span, 1);
}


} // namespace ErrorLog

namespace LogError {
void UndeclaredIdentifier(AST::Identifier *id) {
  undeclared_identifiers[id->token][id->span.source->name]
                        [id->span.start.line_num]
                            .push_back(id->span.start.offset);
}

void AmbiguousIdentifier(AST::Identifier *id) {
  ambiguous_identifiers[id->token][id->span.source->name]
                       [id->span.start.line_num]
                           .push_back(id->span.start.offset);
}

void ImplicitCapture(AST::Identifier *id) {
  implicit_capture[id->decl][id->span.source->name][id->span.start.line_num]
      .push_back(id->span.start.offset);
}

void FailedPrecondition(const IR::property::Property &) {
  std::cerr << "Precondition failed.\n";
}

void EnsureNeedsBool(AST::Expression *expr) {
  std::cerr << "Function postcondition must be of type bool, but you provided "
               "an expression of type "
            << expr->type->to_string() << ".\n";
}

void PreconditionNeedsBool(AST::Expression *expr) {
  std::cerr << "Function precondition must be of type bool, but you provided "
               "an expression of type "
            << expr->type->to_string() << ".\n";
}
} // namespace LogError
