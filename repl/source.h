#ifndef ICARUS_REPL_SOURCE_H
#define ICARUS_REPL_SOURCE_H

#include <iostream>
#include <string_view>

#include "frontend/source/buffer.h"
#include "frontend/source/source.h"

namespace repl {

struct Source : frontend::Source {
  Source(std::istream* input, std::ostream* output)
      : input_(*ASSERT_NOT_NULL(input)),
        output_(*ASSERT_NOT_NULL(output)),
        buffer_("\n") {  // TODO: Determine if this is going to screw up line
                         // numbers, and if we care for a REPL at all.
  }
  ~Source() override {}

  // TODO: This is incorrect but effectively unused and will be removed shortly.
  frontend::SourceChunk ReadUntil(char delim) override {
    ASSERT(delim == '\n');
    output_ << "\n>> ";
    std::string line;
    std::getline(input_, line);
    line.push_back('\n');
    buffer_.AppendChunk(std::move(line));

    return frontend::SourceChunk{
        .view         = buffer_.last_chunk(),
        .more_to_read = false,
    };
  }

  std::string_view line(size_t line_num) const override {
    return buffer_.line(line_num);
  }

  std::string FileName() const override { return "repl"; }

  frontend::SourceBuffer& buffer() override { return buffer_; }
  frontend::SourceBuffer const& buffer() const override { return buffer_; }

  std::istream& input_;
  std::ostream& output_;
  frontend::SourceBuffer buffer_;
};

}  // namespace repl

#endif  // ICARUS_REPL_SOURCE_H
