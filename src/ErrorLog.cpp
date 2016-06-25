#include "ErrorLog.h"

#include "util/pstr.h"
#include "SourceFile.h"

// The global error log
ErrorLog error_log;

ErrorLog::ErrorLog() : err_num_(0) {}

bool ErrorLog::ImmediateMode = false;

std::ostream &operator<<(std::ostream &os, const ErrorLog &log) {
  for (const auto &file_log : log.log_) {
    //NOTE: No sense in robustifying this. It probably won't last.
    size_t num_eqs = 38 - file_log.first.size() / 2;
    std::string eqs(num_eqs, '=');
    os << eqs << ' ' << file_log.first << ' ' << eqs << std::endl;

    for (const auto &line_log : file_log.second) {
      os << "  (line " << line_log.first << "):\n";

      for (const auto &msg : line_log.second) { os << "    " << msg << "\n\n"; }
    }
  }

  os << log.err_num_ << " error";
  if (log.err_num_ != 1) { os << "s"; }

  return os << " found." << std::endl;
}

size_t ErrorLog::num_errors() const { return log_.size(); }

void ErrorLog::log(TokenLocation loc, const std::string &msg) {
  ++err_num_;
  log_[loc.file][loc.line_num].push_back(msg);
}

extern std::map<std::string, SourceFile *> source_map;

void ErrorLog::log(const Err::Message &m) {
  const char *msg_head = "";
  const char *msg_foot = "";
  switch (m.mid) {
  case Err::MessageID::RunawayMultilineComment: msg_head = ""; break;
  case Err::MessageID::InvalidEscapeCharInStringLit: msg_head = ""; break;
  case Err::MessageID::InvalidEscapeCharInCharLit:
    msg_head =
        "I encounterd an invalid escape sequence in your character-literal.";
    msg_foot = "The valid escape sequences for characters are '\\\\', "
               "'\'', '\\a', '\\b', '\\f', '\\n', '\\r, '\\t', and '\\v'.";
    break;
  case Err::MessageID::RunawayStringLit:
    msg_head = "You are missing a quotation mark at the end of your string. I "
               "think it goes here:";
    break;
  case Err::MessageID::NewlineInCharLit: msg_head = ""; break;
  case Err::MessageID::EscapedDoubleQuoteInCharLit:
    msg_head = "The double quotation mark character '\"' does not need to be "
               "esacped in a character literal.";
    break;
  case Err::MessageID::RunawayCharLit: msg_head = ""; break;
  }

  pstr line = source_map AT(m.loc.file)->lines AT(m.loc.line_num);

  int left_border_width;

  fprintf(stderr, "%s\n", msg_head);
  fprintf(stderr, "%lu| %n", m.loc.line_num, &left_border_width);

  std::string underline(std::strlen(line.ptr) + (size_t)left_border_width + 1,
                        ' ');

  for (int i = left_border_width + (int)m.loc.offset;
       i < left_border_width + (int)(m.loc.offset + m.underline_length); ++i) {
    underline[(size_t)i] = '^';
  }

  fprintf(stderr, "%s\n"
                  "%s\n"
                  "%s\n",
          line.ptr, underline.c_str(), msg_foot);
}
