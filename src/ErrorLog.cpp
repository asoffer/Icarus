#include "ErrorLog.h"
#include "util/pstr.h"
#include "SourceFile.h"

// TODO null characters in string mess me up.

extern std::map<std::string, SourceFile *> source_map;

std::map<std::string, std::map<size_t, std::vector<std::string>>>
    Error::Log::log_;

size_t Error::Log::num_errs_   = 0;
bool Error::Log::ImmediateMode = false;

void Error::Log::Dump() {
  for (const auto &file_log : log_) {
    // NOTE: No sense in robustifying this. It probably won't last.
    size_t num_eqs = 38 - file_log.first.size() / 2;
    std::string eqs(num_eqs, '=');
    std::cerr << eqs << ' ' << file_log.first << ' ' << eqs << std::endl;

    for (const auto &line_log : file_log.second) {
      std::cerr << "  (line " << line_log.first << "):\n";

      for (const auto &msg : line_log.second) {
        std::cerr << "    " << msg << "\n\n";
      }
    }
  }

  std::cerr << num_errs_ << " error";
  if (num_errs_ != 1) { std::cerr << "s"; }

  std::cerr << " found." << std::endl;
}

void Error::Log::Log(TokenLocation loc, const std::string &msg) {
  ++num_errs_;
  log_[loc.file][loc.line_num].push_back(msg);
}

static inline size_t NumDigits(size_t n) {
  if (n == 0) return 1;
  size_t counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

void Error::Log::Log(MsgId mid, TokenLocation loc, size_t context_radius,
         size_t underline_length) {
  ++num_errs_;
  const char *msg_head = "";
  const char *msg_foot = nullptr;
  switch (mid) {
  case Error::MsgId::RunawayMultilineComment:
    // TODO give more context. How many layers deep are you? What lines were
    // they opened on?
    fprintf(stderr, "Finished reading file during multi-line comment.\n\n");
    return;
  case Error::MsgId::NullCharInSource:
    fprintf(stderr, "I found a null-character in your source file on line %lu. "
                    "I am ignoring it and moving on. Are you sure \"%s\" is a "
                    "source file?\n\n",
            loc.line_num, loc.file.c_str());
    return;
  case Error::MsgId::NonGraphicCharInSource:
    fprintf(stderr, "I found a non-graphic-character in your source file on "
                    "line %lu. I am ignoring it and moving on. Are you sure "
                    "\"%s\" is a source file?\n\n",
            loc.line_num, loc.file.c_str());
    return;
  case Error::MsgId::InvalidEscapeCharInStringLit:
    msg_head =
        "I encounterd an invalid escape sequence in your string-literal.";
    msg_foot = "The valid escape sequences in a string-literal are \\\\, "
               "\\\", \\a, \\b, \\f, \\n, \\r, \\t, and \\v.";
    break;
  case Error::MsgId::InvalidEscapeCharInCharLit:
    msg_head =
        "I encounterd an invalid escape sequence in your character-literal.";
    msg_foot = "The valid escape sequences in a character-literal are \\\\, "
               "\\\', \\a, \\b, \\f, \\n, \\r, \\t, and \\v.";
    break;
  case Error::MsgId::RunawayStringLit:
    msg_head = "You are missing a quotation mark at the end of your "
               "string-literal. I think it goes here:";
    break;
  case Error::MsgId::EscapedDoubleQuoteInCharLit:
    msg_head = "The double quotation mark character (\") does not need to be "
               "esacped in a character-literal.";
    break;
  case Error::MsgId::EscapedSingleQuoteInStringLit:
    msg_head = "The single quotation mark character (') does not need to be "
               "esacped in a string-literal.";
    break;
  case Error::MsgId::RunawayCharLit:
    msg_head = "You are missing a quotation mark at the end of your "
               "character-literal.";
    break;
  case Error::MsgId::InvalidCharQuestionMark:
    msg_head = "The character '?' is not used in Icarus. I am going to ignore "
               "it and continue processing.";
    break;
  case Error::MsgId::InvalidCharTilde:
    msg_head = "The character '~' is not used in Icarus. I am going to ignore "
               "it and continue processing.";
    break;
  case Error::MsgId::NonWhitespaceAfterNewlineEscape:
    msg_head =
        "I found a non-whitespace character following a '\\' on the same line.";
    msg_foot = "Backslashes are used to ignore line-breaks and therefore must "
               "be followed by a newline (whitespace between the backslash and "
               "the newline is okay).";
    break;
  case Error::MsgId::NotInMultilineComment:
    msg_head = "I found a token representing the end of a multi-line comment "
               "(*/), but it was not part of a comment block.";
    break;
  case Error::MsgId::TabInCharLit:
    msg_head = "I found a tab '\t' in your character-literal. You need to use "
               "\\t instead.";
    break;
  case Error::MsgId::TooManyDots:
    msg_head = "There are too many consecutive '.' characters. I am assuming "
               "you meant \"..\".";
    break;
  }

  pstr line = source_map AT(loc.file)->lines AT(loc.line_num);

  size_t left_border_width = NumDigits(loc.line_num) + 4;

  // Extra + 1 at the end because we might point after the end of the line.
  std::string underline(std::strlen(line.ptr) + left_border_width + 1, ' ');
  for (size_t i = left_border_width + loc.offset;
       i < left_border_width + loc.offset + underline_length; ++i) {
    underline[i] = '^';
  }

  fprintf(stderr, "%s\n\n"
                  "  %lu| %s\n"
                  "%s\n",
          msg_head, loc.line_num, line.ptr, underline.c_str());

  if (msg_foot) {
    fprintf(stderr, "%s\n\n", msg_foot);
  } else {
    fprintf(stderr, "\n");
  }
}
