#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

namespace Error {
enum class MsgId {
  NullCharInSource,
  NonGraphicCharInSource,
  InvalidEscapeCharInStringLit,
  InvalidEscapeCharInCharLit,
  TabInCharLit,
  EscapedDoubleQuoteInCharLit,
  EscapedSingleQuoteInStringLit,
  NonWhitespaceAfterNewlineEscape,
  NotInMultilineComment,
  RunawayMultilineComment,
  RunawayStringLit,
  RunawayCharLit,
  InvalidCharDollarSign,
  InvalidCharQuestionMark,
  InvalidCharTilde
};

struct Msg {
  Msg(MsgId msg_id, const TokenLocation &tok_loc, size_t rad, size_t under_len)
      : loc(tok_loc), context_radius(rad), underline_length(under_len),
        mid(msg_id) {}

  TokenLocation loc;
  size_t context_radius; // Number of lines surrounding this one to show.
  size_t underline_length;
  MsgId mid;
};

namespace Log {
extern size_t num_errs_;
extern std::map<std::string, std::map<size_t, std::vector<std::string>>> log_;

extern bool ImmediateMode;
void Dump();
inline size_t NumErrors() { return num_errs_; }
void Log(TokenLocation loc, const std::string &msg);
void Log(MsgId mid, TokenLocation loc, size_t context_radius,
         size_t underline_length);
} // namespace Log

} // namespace Error

#endif // ICARUS_ERROR_LOG_H
