#include "error_log.h"

#include <cstring>

#include "ast/ast.h"
#include "base/source.h"
#include "cursor.h"
#include "type/type.h"

extern std::map<std::string, Source*> source_map;

using LineNum    = size_t;
using LineOffset = size_t;
using FileName   = std::string;
using Token      = std::string;

typedef std::map<Token,
                 std::map<FileName, std::map<LineNum, std::vector<LineOffset>>>>
    TokenToErrorMap;
typedef std::map<FileName, std::vector<LineNum>> FileToLineNumMap;
typedef std::map<const AST::Declaration *,
                 std::map<FileName, std::map<LineNum, std::vector<LineOffset>>>>
    DeclToErrorMap;
typedef std::map<Cursor, std::map<LineNum, std::vector<LineOffset>>>
    LocToErrorMap;

static TokenToErrorMap undeclared_identifiers, ambiguous_identifiers;
static DeclToErrorMap invalid_capture;
static LocToErrorMap case_errors;
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

static inline std::string NumTimes(size_t n, const char *numeric_prefix,
                                   bool capitalize) {
  if (n == 1) { return capitalize ? "Once" : "once"; }
  if (n == 2) { return capitalize ? "Twice" : "twice"; }
  if (numeric_prefix) {
    return numeric_prefix + std::to_string(n) + " times";
  } else {
    return std::to_string(n) + " times";
  }
}

static void GatherAndDisplay(const char *fmt, const TokenToErrorMap &log) {
  for (const auto &kv : log) {
    const char *token   = kv.first.c_str();
    size_t token_length = kv.first.size();

    size_t num_uses = 0;
    for (const auto &file_and_locs : kv.second) {
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses += line_and_offsets.second.size();
      }
    }

    fprintf(stderr, fmt, token);
    if (num_uses == 1) {
      fprintf(stderr, ".\n");
    } else {
      fprintf(stderr, " used %s.\n", NumTimes(num_uses, nullptr, false).c_str());
    }

    for (const auto &file_and_locs : kv.second) {
      size_t max_line_num = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        if (max_line_num < line_and_offsets.first) {
          max_line_num = line_and_offsets.first;
        }
      }
      size_t line_num_width = NumDigits(max_line_num);

      size_t num_uses_in_file = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses_in_file += line_and_offsets.second.size();
      }

      fprintf(stderr, "  %s in '%s':\n",
              NumTimes(num_uses_in_file, "Used ", true).c_str(),
              file_and_locs.first.c_str());

      for (const auto &line_and_offsets : file_and_locs.second) {
        auto line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);

        size_t left_border_width = line_num_width + 6;
        size_t line_length       = line.size() + 1;
        char *underline          = new char[left_border_width + line_length + 1];
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
    if (num_uses == 1) {
      fprintf(stderr, ".\n");
    } else {
      fprintf(stderr, " used %s.\n", NumTimes(num_uses, nullptr, false).c_str());
    }

    for (const auto &file_and_locs : kv.second) {
      size_t num_uses_in_file = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses_in_file += line_and_offsets.second.size();
      }

      fprintf(stderr, "  %s in '%s':\n",
              NumTimes(num_uses_in_file, "Used ", true).c_str(),
              file_and_locs.first.c_str());

      size_t max_line_num = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        if (max_line_num < line_and_offsets.first) { max_line_num = line_and_offsets.first; }
      }
      size_t line_num_width = NumDigits(max_line_num);

      for (const auto &line_and_offsets : file_and_locs.second) {
        auto line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);

        size_t left_border_width                   = line_num_width + 6;
        size_t line_length                         = line.size() + 1;
        char *underline                            = new char[left_border_width + line_length + 1];
        underline[line_length + left_border_width] = '\0';
        memset(underline, ' ', left_border_width + line_length);

        for (const auto offset : line_and_offsets.second) {
          memset(underline + left_border_width + offset, '^', token_length);
        }

        fprintf(stderr, "    %*lu| %s\n"
                        "%s\n",
                (int)line_num_width, line_and_offsets.first, line.c_str(), underline);
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
    fprintf(stderr, "  Found %lu instance%s in '%s':\n", kv.second.size(),
            kv.second.size() == 1 ? "s" : "", kv.first.c_str());

    int line_num_width = (int)NumDigits(kv.second.back());
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
        fprintf(stderr, "%s...|\n",
                std::string((size_t)line_num_width + 1, ' ').c_str());
      }

      auto line = source_map AT(kv.first)->lines AT(line_num);
      fprintf(stderr, ">   %*lu| %s\n", line_num_width, line_num, line.c_str());
      last_line_num = line_num;
    }
  }
}

void ErrorLog::Dump() {
  GatherAndDisplay("Undeclared identifier '%s'", undeclared_identifiers);
  GatherAndDisplay("Ambiguous identifier '%s'", ambiguous_identifiers);
  GatherAndDisplay("Invalid capture of identifier '%s'", invalid_capture);

  // TODO fix this repsonse regarding imports.
  GatherAndDisplay("Found %lu non-declaration statement%s at the top level. "
                   "All top-level statements must either be declarations, "
                   "imports, or void compile-time evaluations.",
                   global_non_decl);

  std::cerr << num_errs_ << " error";
  if (num_errs_ != 1) { std::cerr << "s"; }

  std::cerr << " found." << std::endl;
}

static void DisplayErrorMessage(const char *msg_head,
                                const char *msg_foot, const Cursor &loc,
                                size_t underline_length) {
  auto line = source_map AT(loc.file_name())->lines AT(loc.line_num);

  size_t left_border_width = NumDigits(loc.line_num) + 6;

  // Extra + 1 at the end because we might point after the end of the line.
  std::string underline(line.size() + left_border_width + 1, ' ');
  for (size_t i = left_border_width + loc.offset;
       i < left_border_width + loc.offset + underline_length; ++i) {
    underline[i] = '^';
  }

  fprintf(stderr, "%s\n\n"
                  "    %lu| %s\n"
                  "%s\n",
          msg_head, loc.line_num, line.c_str(), underline.c_str());

  if (msg_foot) {
    fprintf(stderr, "%s\n\n", msg_foot);
  } else {
    fprintf(stderr, "\n");
  }
}

namespace ErrorLog {
void NullCharInSrc(const Cursor &loc) {
  fprintf(stderr, "I found a null-character in your source file on line %lu. "
                  "I am ignoring it and moving on. Are you sure \"%s\" is a "
                  "source file?\n\n",
          loc.line_num, loc.file_name().c_str());
  ++num_errs_;
}

void NonGraphicCharInSrc(const Cursor &loc) {
  fprintf(stderr, "I found a non-graphic-character in your source file on line "
                  "%lu. I am ignoring it and moving on. Are you sure \"%s\" is "
                  "a source file?\n\n",
          loc.line_num, loc.file_name().c_str());
  ++num_errs_;
}

void LogGeneric(const Cursor &, const std::string &msg) {
  ++num_errs_;
  fprintf(stderr, "%s", msg.c_str());
}


void InvalidRangeType(const Cursor &loc, Type *t) {
  ++num_errs_;
  const char *foot_fmt = "Expected type: int, uint, or char\n"
                         "Given type: %s.";
  std::string t_str = t->to_string();
  size_t type_string_size = t_str.size();
  auto msg_foot = (char *)malloc(strlen(foot_fmt) + type_string_size - 1);
  sprintf(msg_foot, foot_fmt, t_str.c_str());

  DisplayErrorMessage("Attempting to create a range with an invalid type.",
                      msg_foot, loc, type_string_size);
  free(msg_foot);
}

void TooManyDots(const Cursor &loc, size_t num_dots) {
  const char *msg_head = "There are too many consecutive '.' characters. I am "
                         "assuming you meant \"..\".";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, num_dots);
}

void NonWhitespaceAfterNewlineEscape(const Cursor &loc, size_t dist) {
  const char *msg_head =
      "I found a non-whitespace character following a '\\' on the same line.";
  const char *msg_foot = "Backslashes are used to ignore line-breaks and "
                         "therefore must be followed by a newline (whitespace "
                         "between the backslash and the newline is okay).";

  ++num_errs_;
  DisplayErrorMessage(msg_head, msg_foot, loc, dist);
}

void RunawayMultilineComment() {
  fprintf(stderr, "Finished reading file during multi-line comment.\n\n");
  ++num_errs_;
}

void InvalidStringIndex(const Cursor &loc, Type *index_type) {
  std::string msg_head = "String indexed by an invalid type. Expected an int "
                         "or uint, but encountered a " +
                         index_type->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);

}
void NotAType(AST::Expression *expr, Type *t) {
  ++num_errs_;
  auto t_str = t->to_string();
  const char *head_fmt =
      "Expression was expected to be a type, but instead it was a(n) %s.";
  char *msg_head = (char *)malloc(t_str.size() + strlen(head_fmt) - 1);
  sprintf(msg_head, head_fmt, t_str.c_str());

  DisplayErrorMessage(msg_head, nullptr, expr->loc, 1);
  free(msg_head);
}

void IndeterminantType(AST::Expression *expr) {
  ++num_errs_;
  DisplayErrorMessage("Cannot determine the type of the expression:", nullptr,
                      expr->loc, 1);
}

void CyclicDependency(AST::Node *node) {
  ++num_errs_;
  DisplayErrorMessage("This expression's type is declared self-referentially.",
                      nullptr, node->loc, 1);
}

void GlobalNonDecl(const Cursor &loc) {
  ++num_errs_;
  global_non_decl[loc.file_name().c_str()].push_back(loc.line_num);
}


void NonIntegralArrayIndex(const Cursor &loc, const Type *index_type) {
  std::string msg_head = "Array is being indexed by an expression of type " +
                         index_type->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(
      msg_head.c_str(),
      "Arrays must be indexed by integral types (either int or uint)", loc, 1);
}

void DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id) {
  std::string msg_head =
      "Identifier '" + id->token + "' used before it was declared.";
  std::string msg_foot = "Declaration can be found on line " +
                         std::to_string(decl->loc.line_num) + ".";
  // TODO Provide file name as well.
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), msg_foot.c_str(), id->loc,
                      id->token.size());
}

void InvalidAddress(const Cursor &loc, Assign mode) {
  ++num_errs_;
  if (mode == Assign::Const) {
    DisplayErrorMessage(
        "Attempting to take the address of an identifier marked as #const",
        "Marking an identifier as #const means that it is a compile-time "
        "constant. The value may not actually be associated with a storage "
        "address. For this reason, it is illegal to take the address.",
        loc, 1);
  } else if (mode == Assign::RVal) {
    DisplayErrorMessage(
        "Attempting to take the address of a temporary expression.", nullptr, loc,
        1);
  } else {
    UNREACHABLE();
  }
}

void InvalidAssignment(const Cursor &loc, Assign mode) {
  ++num_errs_;
  if (mode == Assign::Const) {
    DisplayErrorMessage(
        "Attempting to assign to an identifier marked as #const",
        "Marking an identifier as #const means that it is a compile-time "
        "constant and cannot be modified at run-time.",
        loc, 1);
  } else if (mode == Assign::RVal) {
    DisplayErrorMessage(
        "Attempting to assign to a temporary expression.", nullptr, loc,
        1);
  } else {
    UNREACHABLE();
  }
}

void CaseLHSBool(const Cursor &, const Cursor &loc, const Type *t) {
  std::string msg_head = "In a case statement, the lefthand-side of a case "
                         "must have type bool. However, the expression has "
                         "type " +
                         t->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void MissingMember(const Cursor &loc, const std::string &member_name,
                   const Type *t) {
  ++num_errs_;
  std::string msg_head = "Expressions of type `" + t->to_string() + "` have no member named '" +
                         member_name + "'.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void IndexingNonArray(const Cursor &loc, const Type *t) {
  ++num_errs_;
  std::string msg_foot = "Indexed type is a `" + t->to_string() + "`.";
  DisplayErrorMessage("Cannot index into a non-array type.", msg_foot.c_str(),
                      loc, 1);
}

void UnopTypeFail(const std::string &msg, const AST::Unop *unop) {
  ++num_errs_;
  DisplayErrorMessage(msg.c_str(), nullptr, unop->loc, 1);
}

void SlicingNonArray(const Cursor &loc, const Type *t) {
  ++num_errs_;
  std::string msg_foot = "Sliced type is a `" + t->to_string() + "`.";
  DisplayErrorMessage("Cannot slice a non-array type.", msg_foot.c_str(), loc,
                      1);
}

void NonBinaryAssignment(const Cursor &loc, size_t len) {
  ++num_errs_;
  const char *head_fmt =
      "Assignment must be a binary operator, but %llu argument%s given.";

  size_t size_to_malloc = strlen(head_fmt) - 7 + NumDigits(len) +
                          (len == 1 ? strlen(" was") : strlen("s were"));
  auto msg_head = (char *)malloc(size_to_malloc);
  sprintf(msg_head, head_fmt, len, (len == 1 ? " was" : "s were"));
  // TODO is undeline length correct?
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void AssignmentArrayLength(const Cursor &loc, size_t len) {
  ++num_errs_;
  const char *head_fmt = "Invalid assignment. Array on right-hand side has "
                         "unknown length, but lhs is known to be of "
                         "length %llu.";
  auto msg_head = (char *)malloc(strlen(head_fmt) - 3 + NumDigits(len));
  sprintf(msg_head, head_fmt, len);
  // TODO is undeline length correct?
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void AlreadyFoundMatch(const Cursor &loc, const std::string &op_symbol,
                       const Type *lhs, const Type *rhs) {
  ++num_errs_;
  const char *head_fmt =
      "Already found a match for operator %s` with types %s and %s.";
  std::string lhs_str = lhs->to_string();
  std::string rhs_str = rhs->to_string();

  auto msg_head = (char *)malloc(strlen(head_fmt) - 5 + op_symbol.size() +
                                 lhs_str.size() + rhs_str.size());
  sprintf(msg_head, head_fmt, op_symbol.c_str(), lhs_str.c_str(), rhs_str.c_str());
  // TODO undeline length is incorrect?
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void NoKnownOverload(const Cursor &loc, const std::string&op_symbol, const Type *lhs,
                     const Type *rhs) {
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
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void InvalidRangeTypes(const Cursor &loc, const Type *lhs, const Type *rhs) {
  ++num_errs_;
  const char *head_fmt = "No range construction for types %s .. %s.";
  std::string lhs_str = lhs->to_string();
  std::string rhs_str = rhs->to_string();

  auto msg_head =
      (char *)malloc(strlen(head_fmt) - 3 + lhs_str.size() + rhs_str.size());
  sprintf(msg_head, head_fmt, lhs_str.c_str(), rhs_str.c_str());
  // TODO undeline length is incorrect?
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void AssignmentTypeMismatch(const Cursor &loc, const Type *lhs,
                            const Type *rhs) {
  ++num_errs_;
  std::string msg_head =
      "Invalid assignment. Left-hand side has type " + lhs->to_string() +
      ", but right-hand side has type " + rhs->to_string() + ".";
  // TODO underline isn't what it ought to be.
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}


void InvalidCast(const Cursor &loc, const Type *from, const Type *to) {
  ++num_errs_;
  std::string msg_head = "No valid cast from `" + from->to_string() + "` to `" +
                         to->to_string() + "`.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 2);
}

void NotAType(const Cursor &loc, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head = "In declaration of `" + id_tok +
                         "`, the declared type is not a actually a type.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void DeclaredVoidType(const Cursor &loc, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head =
      "Identifier `" + id_tok + "`is declared to have type `void`.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void DeclaredParametricType(const Cursor &loc, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head =
      "In declaration of `" + id_tok +
      "`, the declared type is parametric and has no parameters provided.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void DoubleDeclAssignment(const Cursor &decl_loc, const Cursor &val_loc) {
  ++num_errs_;
  if (decl_loc.line_num == val_loc.line_num){
    DisplayErrorMessage("Attempting to initialize an identifier that already "
                        "has an initial value.",
                        nullptr, decl_loc, 1);
  } else {
    NOT_YET();
  }
}
void InvalidPrintDefinition(const Cursor &loc, const Type *t) {
  ++num_errs_;
  const char *msg_fmt = "Cannot define print function for type %s.";
  std::string t_str   = t->to_string();
  auto msg_head       = (char *)malloc(t_str.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, t_str.c_str());
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void InitWithNull(const Cursor &loc, const Type *t, const Type *intended) {
  ++num_errs_;
  std::string msg_head = "Cannot initialize an identifier of type " +
                         t->to_string() +
                         " with null. Did you mean to declare it as " +
                         intended->to_string() + "?";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void InvalidAssignDefinition(const Cursor &loc, const Type *t) {
  ++num_errs_;
  const char *msg_fmt = "Cannot define assignment function for type %s.";
  std::string t_str   = t->to_string();
  auto msg_head       = (char *)malloc(t_str.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, t_str.c_str());
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void InvalidScope(const Cursor &loc, const Type *t) {
  ++num_errs_;
  const char*msg_fmt = "Object of type '%s' used as if it were as scope.";
  std::string t_str   = t->to_string();
  auto msg_head = (char *)malloc(t_str.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, t_str.c_str());
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void NotBinary(const Cursor &loc, const std::string &token) {
  ++num_errs_;
  const char *msg_fmt = "Operator '%s' is not a binary operator";
  auto msg_head = (char *)malloc(token.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, token.c_str());
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

void Reserved(const Cursor &loc, const std::string &token) {
  ++num_errs_;
  const char *msg_fmt = "Identifier '%s' is a reserved keyword.";
  auto msg_head = (char *)malloc(token.size() + strlen(msg_fmt) - 1);
  sprintf(msg_head, msg_fmt, token.c_str());
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
  free(msg_head);
}

// TODO better error message for repeated enum name
#define ERROR_MACRO(fn_name, msg_head, msg_foot, underline_length)             \
  void fn_name(const Cursor &loc) {                                            \
    ++num_errs_;                                                               \
    DisplayErrorMessage(msg_head, msg_foot, loc, underline_length);            \
  }
#include "config/error.conf"
#undef ERROR_MACRO

void InvalidReturnType(const Cursor &loc, Type *given, Type *correct) {
  ++num_errs_;
  std::string msg_head = "Invalid return type on line " +
                         std::to_string(loc.line_num) + " in \"" +
                         loc.file_name() + "\".";
  std::string msg_foot = "Given return type:    " + given->to_string() +
                         "\n"
                         "Expected return type: " +
                         correct->to_string();
  DisplayErrorMessage(msg_head.c_str(), msg_foot.c_str(), loc,
                      strlen(loc.line.c_str()) - loc.offset);
}

void ChainTypeMismatch(const Cursor &loc, std::set<Type *> types) {
  ++num_errs_;
  std::stringstream ss;
  ss << "Found the following types in the expression:\n";
  for (auto t : types) { ss << "  * " << t->to_string() << "\n"; }
  DisplayErrorMessage("Type do not all match in expression:", ss.str().c_str(),
                      loc, 1);
}

void UserDefinedError(const Cursor &loc, const std::string& msg) {
  ++num_errs_;
  DisplayErrorMessage(msg.c_str(), "", loc, 1);
}

static void DisplayLines(const std::vector<Cursor> &lines) {
  size_t left_space     = NumDigits(lines.back().line_num) + 2;
  std::string space_fmt = std::string(left_space - 3, ' ') + "...|\n";

  size_t last_line_num = lines[0].line_num - 1;
  for (auto loc : lines) {
    if (loc.line_num != last_line_num + 1) { fputs(space_fmt.c_str(), stderr); }
    fprintf(stderr, "%*lu| %s\n", (int)left_space, loc.line_num, loc.line.c_str());
    last_line_num = loc.line_num;
  }

  fputc('\n', stderr);
}

void CaseTypeMismatch(AST::Case *case_ptr, Type *correct) {
  if (correct) {
    fprintf(stderr, "Type mismatch in case-expression on line %lu in \"%s\".\n",
            case_ptr->loc.line_num, case_ptr->loc.file_name().c_str());

    std::vector<Cursor> locs;
    for (auto& kv : case_ptr->key_vals) {
      ++num_errs_;
      if (kv.second->type == Err || kv.second->type == correct) { continue; }
      locs.push_back(kv.second->loc);
    }

    num_errs_ += locs.size();
    DisplayLines(locs);
    fprintf(stderr, "Expected an expression of type %s.\n\n",
            correct->to_string().c_str());

  } else {
    ++num_errs_;
    fprintf(stderr, "Type mismatch in case-expression on line %lu in \"%s\".\n\n",
            case_ptr->loc.line_num, case_ptr->loc.file_name().c_str());
  }
}

void UnknownParserError(const std::string &file_name,
                        const std::vector<Cursor> &lines) {

  if (lines.empty()) {
    fprintf(stderr,
            "Parse errors found in \"%s\". Sorry I can't be more specific.",
            file_name.c_str());
    ++num_errs_;
    return;
  }

  num_errs_ += lines.size();
  fprintf(stderr, "Parse errors found in \"%s\" on the following lines:\n\n",
          file_name.c_str());

  DisplayLines(lines);
}

void UndeclaredIdentifier(const Cursor &loc, const std::string &token) {
  ++num_errs_;
  undeclared_identifiers[token][loc.file_name()][loc.line_num]
      .push_back(loc.offset);
}

void InvalidCapture(const Cursor &loc, const AST::Declaration *decl) {
  ++num_errs_;
  invalid_capture[decl][loc.file_name()][loc.line_num].push_back(
      loc.offset);
}

void AmbiguousIdentifier(const Cursor &loc, const std::string &token) {
  ++num_errs_;
  ambiguous_identifiers[token][loc.file_name()][loc.line_num].push_back(
      loc.offset);
}

} // namespace ErrorLog
