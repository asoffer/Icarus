#include "error_log.h"

#include <cstring>

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
  if (n == 0) return 1;
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

static void GatherAndDisplayIdentifierError(const std::string &msg,
                                            const TokenToErrorMap &log) {
  for (const auto &kv : log) {
    size_t num_uses = 0;
    for (const auto &file_and_locs : kv.second) {
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses += line_and_offsets.second.size();
      }
    }

    if (num_uses == 1) {
      std::cerr << msg << " '" << kv.first << "'.\n";
    } else {
      std::cerr << msg << " '" << kv.first << "' used " << NumTimes(num_uses)
                << ".\n";
    }

    for (const auto &file_and_locs : kv.second) {
      size_t max_line_num = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        if (max_line_num < line_and_offsets.first) {
          max_line_num = line_and_offsets.first;
        }
      }
      // size_t line_num_width = NumDigits(max_line_num);

      size_t num_uses_in_file = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses_in_file += line_and_offsets.second.size();
      }

      std::cerr << "  Used " << NumTimes(num_uses_in_file) << " in '"
                << file_and_locs.first << "':\n";

      for (const auto &line_and_offsets : file_and_locs.second) {
        auto line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);
        // TODO alignment
        std::cerr << line_and_offsets.first << "| " << line << '\n'
          << std::string(line.size() + 1,'^');

        // std::cerr << base::fmt(
        //     "{0:>{1}}| {2}\n"
        //     "{3:>{4}}\n",
        //     /* 0 -  line number = */ line_and_offsets.first,
        //     /* 1 -    alignment = */ line_num_width + 4,
        //     /* 2 - line content = */ line,
        //     /* 3 -    underline = */ std::string(line.size() + 1, '^'),
        //     /* 4 -    alignment = */ line_num_width + 6 + line.size() + 1);
      }
    }
  }
}

static void GatherAndDisplay(const char *fmt_head, const DeclToErrorMap &log) {
  for (const auto &kv : log) {
    const char *token   = kv.first->identifier->token.c_str();
    size_t token_length = kv.first->identifier->token.size();

    size_t num_uses = 0;
    for (const auto &file_and_locs : kv.second) {
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses += line_and_offsets.second.size();
      }
    }

    fprintf(stderr, fmt_head, token);
    if (num_uses != 1) { std::cerr << " used " << NumTimes(num_uses); }
    std::cerr << ".\n";

    for (const auto &file_and_locs : kv.second) {
      size_t num_uses_in_file = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses_in_file += line_and_offsets.second.size();
      }

      std::cerr << "  Used " << NumTimes(num_uses_in_file) << " in '"
                << file_and_locs.first << "':\n";

      size_t max_line_num = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        if (max_line_num < line_and_offsets.first) {
          max_line_num = line_and_offsets.first;
        }
      }
      size_t line_num_width = NumDigits(max_line_num);

      for (const auto &line_and_offsets : file_and_locs.second) {
        auto line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);

        size_t left_border_width = line_num_width + 6;
        size_t line_length       = line.size() + 1;
        char *underline = new char[left_border_width + line_length + 1];
        underline[line_length + left_border_width] = '\0';
        memset(underline, ' ', left_border_width + line_length);

        for (const auto offset : line_and_offsets.second) {
          memset(underline + left_border_width + offset, '^', token_length);
        }

        fprintf(stderr, "    %*lu| %s\n"
                        "%s\n",
                (int)line_num_width, line_and_offsets.first, line.c_str(),
                underline);
        delete[] underline;
      }
    }
  }
}

static void GatherAndDisplay(const char *fmt, const FileToLineNumMap &log) {
  if (global_non_decl.empty()) { return; }

  size_t num_instances = 0;
  for (const auto &kv : log) { num_instances += kv.second.size(); }

  fprintf(stderr, fmt, num_instances, num_instances ? "s" : "");

  for (const auto &kv : log) {
    std::cerr << "  Found " << kv.second.size()
              << (kv.second.size() == 1 ? " instance in '" : " instances in '")
              << kv.first << "':\n";

    int line_num_width   = (int)NumDigits(kv.second.back());
    size_t last_line_num = kv.second.front();
    for (auto line_num : kv.second) {
      if (line_num - last_line_num == 2) {
        auto line = source_map AT(kv.first)->lines AT(line_num - 1);
        fprintf(stderr, "    %*lu| %s\n", line_num_width, line_num - 1,
                line.c_str());
      } else if (line_num - last_line_num == 3) {
        auto line = source_map AT(kv.first)->lines AT(line_num - 1);
        fprintf(stderr, "    %*lu| %s\n", line_num_width, line_num - 1,
                line.c_str());

        line = source_map AT(kv.first)->lines AT(line_num - 2);
        fprintf(stderr, "    %*lu| %s\n", line_num_width, line_num - 2,
                line.c_str());
      } else if (line_num - last_line_num > 3) {
        std::cerr << std::string(static_cast<size_t>(line_num_width) + 1, ' ')
                  << "...|\n";
      }

      auto line = source_map AT(kv.first)->lines AT(line_num);
      fprintf(stderr, ">   %*lu| %s\n", line_num_width, line_num, line.c_str());
      last_line_num = line_num;
    }
  }
}

void ErrorLog::Dump() {
  GatherAndDisplayIdentifierError("Undeclared identifier",
                                  undeclared_identifiers);
  GatherAndDisplayIdentifierError("Ambiguous identifier",
                                  ambiguous_identifiers);
  GatherAndDisplay("Invalid capture of identifier '%s'", implicit_capture);

  // TODO fix this repsonse regarding imports.
  GatherAndDisplay("Found %lu non-declaration statement%s at the top level. "
                   "All top-level statements must either be declarations, "
                   "imports, or void compile-time evaluations.",
                   global_non_decl);

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
         std::to_string(line_num) + "| " + line.get() + "\n";
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
  fprintf(stderr, "Finished reading file during multi-line comment.\n\n");
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
  auto t_str = t->to_string();
  const char *head_fmt =
      "Expression was expected to be a type, but instead it was a(n) %s.";
  char *msg_head = (char *)malloc(t_str.size() + strlen(head_fmt) - 1);
  sprintf(msg_head, head_fmt, t_str.c_str());

  DisplayErrorMessage(msg_head, "", expr->span, 1);
  free(msg_head);
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
  const char *head_fmt =
      "Assignment must be a binary operator, but %llu argument%s given.";

  size_t size_to_malloc = strlen(head_fmt) - 7 + NumDigits(len) +
                          (len == 1 ? strlen(" was") : strlen("s were"));
  auto msg_head = (char *)malloc(size_to_malloc);
  sprintf(msg_head, head_fmt, len, (len == 1 ? " was" : "s were"));
  // TODO is undeline length correct?
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void AssignmentArrayLength(const TextSpan &span, size_t len) {
  ++num_errs_;
  const char *head_fmt = "Invalid assignment. Array on right-hand side has "
                         "unknown length, but lhs is known to be of "
                         "length %llu.";
  auto msg_head = (char *)malloc(strlen(head_fmt) - 3 + NumDigits(len));
  sprintf(msg_head, head_fmt, len);
  // TODO is undeline length correct?
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void AlreadyFoundMatch(const TextSpan &span, const std::string &op_symbol,
                       const Type *lhs, const Type *rhs) {
  ++num_errs_;
  const char *head_fmt =
      "Already found a match for operator %s` with types %s and %s.";
  std::string lhs_str = lhs->to_string();
  std::string rhs_str = rhs->to_string();

  auto msg_head = (char *)malloc(strlen(head_fmt) - 5 + op_symbol.size() +
                                 lhs_str.size() + rhs_str.size());
  sprintf(msg_head, head_fmt, op_symbol.c_str(), lhs_str.c_str(),
          rhs_str.c_str());
  // TODO undeline length is incorrect?
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void NoKnownOverload(const TextSpan &span, const std::string &op_symbol,
                     const Type *lhs, const Type *rhs) {
  ++num_errs_;
  const char *head_fmt =
      "No known operator overload for `%s` with types %s and %s.";
  std::string lhs_str = lhs->to_string();
  std::string rhs_str = rhs->to_string();

  auto msg_head = (char *)malloc(strlen(head_fmt) - 5 + op_symbol.size() +
                                 lhs_str.size() + rhs_str.size());
  sprintf(msg_head, head_fmt, op_symbol.c_str(), lhs_str.c_str(),
          rhs_str.c_str());
  // TODO undeline length is incorrect?
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void InvalidRangeTypes(const TextSpan &span, const Type *lhs, const Type *rhs) {
  ++num_errs_;
  const char *head_fmt = "No range construction for types %s .. %s.";
  std::string lhs_str  = lhs->to_string();
  std::string rhs_str  = rhs->to_string();

  auto msg_head =
      (char *)malloc(strlen(head_fmt) - 3 + lhs_str.size() + rhs_str.size());
  sprintf(msg_head, head_fmt, lhs_str.c_str(), rhs_str.c_str());
  // TODO undeline length is incorrect?
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
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
  const char *msg_fmt = "Cannot define print function for type %s.";
  std::string t_str   = t->to_string();
  auto msg_head       = (char *)malloc(t_str.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, t_str.c_str());
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
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
  const char *msg_fmt = "Cannot define assignment function for type %s.";
  std::string t_str   = t->to_string();
  auto msg_head       = (char *)malloc(t_str.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, t_str.c_str());
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void InvalidScope(const TextSpan &span, const Type *t) {
  ++num_errs_;
  const char *msg_fmt = "Object of type '%s' used as if it were as scope.";
  std::string t_str   = t->to_string();
  auto msg_head       = (char *)malloc(t_str.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, t_str.c_str());
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void NotBinary(const TextSpan &span, const std::string &token) {
  ++num_errs_;
  const char *msg_fmt = "Operator '%s' is not a binary operator";
  auto msg_head       = (char *)malloc(token.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, token.c_str());
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
}

void Reserved(const TextSpan &span, const std::string &token) {
  ++num_errs_;
  const char *msg_fmt = "Identifier '%s' is a reserved keyword.";
  auto msg_head       = (char *)malloc(token.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, token.c_str());
  DisplayErrorMessage(msg_head, "", span, 1);
  free(msg_head);
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
      fputs(space_fmt.c_str(), stderr);
    }
    fprintf(stderr, "%*u| %s\n", (int)left_space, span.start.line_num,
            span.source->lines[span.start.line_num].c_str());
    last_line_num = span.start.line_num;
  }

  fputc('\n', stderr);
}

void CaseTypeMismatch(AST::Case *case_ptr, Type *correct) {
  if (correct) {
    fprintf(stderr, "Type mismatch in case-expression on line %u in \"%s\".\n",
            case_ptr->span.start.line_num, case_ptr->span.source->name.c_str());

    std::vector<TextSpan> locs;
    for (auto &kv : case_ptr->key_vals) {
      ++num_errs_;
      if (kv.second->type == Err || kv.second->type == correct) { continue; }
      locs.push_back(kv.second->span);
    }

    num_errs_ += locs.size();
    DisplayLines(locs);
    fprintf(stderr, "Expected an expression of type %s.\n\n",
            correct->to_string().c_str());

  } else {
    ++num_errs_;
    fprintf(stderr,
            "Type mismatch in case-expression on line %u in \"%s\".\n\n",
            case_ptr->span.start.line_num, case_ptr->span.source->name.c_str());
  }
}

void UnknownParserError(const Source::Name &source_name,
                        const std::vector<TextSpan> &lines) {
  if (lines.empty()) {
    fprintf(stderr,
            "Parse errors found in \"%s\". Sorry I can't be more specific.",
            source_name.c_str());
    ++num_errs_;
    return;
  }

  num_errs_ += lines.size();
  fprintf(stderr, "Parse errors found in \"%s\" on the following lines:\n\n",
          source_name.c_str());

  DisplayLines(lines);
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
  fprintf(stderr, "Precondition failed.\n");
}

void EnsureNeedsBool(AST::Expression *expr) {
  fprintf(stderr, "Function precondition must be of type bool, but you "
                  "provided an expression of type %s.\n",
          expr->type->to_string().c_str());
}

void PreconditionNeedsBool(AST::Expression *expr) {
  fprintf(stderr, "Function precondition must be of type bool, but you "
                  "provided an expression of type %s.\n",
          expr->type->to_string().c_str());
}
} // namespace LogError

