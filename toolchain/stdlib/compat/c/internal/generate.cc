#include <cstdio>
#include <string>
#include <type_traits>

struct string_view {
  char const * ptr;
  size_t length;
};

string_view FindReplacement(std::string const &s, std::string const & needle) {
  size_t index = s.find(needle);
  if (index == std::string::npos) { std::abort(); }
  if (index + needle.size() + 1 >= s.size()) { std::abort(); }
  if (s[index + needle.size()] != '\t') { std::abort(); }
  size_t start = index + needle.size() + 1;
  size_t end = s.find("\n", start);
  if (end == std::string::npos) { std::abort(); }
  string_view sv;
  sv.ptr    = s.data() + start;
  sv.length = end - start;
  return sv;
}

bool ReadFileToString(char const *file_name, std::string &result) {
  auto *f = std::fopen(file_name, "r");
  if (not f) { return false; }
  std::fseek(f, 0, SEEK_END);
  size_t file_size = std::ftell(f);
  std::rewind(f);
  result.clear();
  result.resize(file_size, '\0');
  size_t count = std::fread(&result[0], 1, result.size(), f);
  (void)count;
  std::fclose(f);
  return true;
}

int main(int argc, char const *argv[]) {
  std::string content, replacements;
  std::printf("// Generated from %s.\n", argv[1]);
  if (not ReadFileToString(argv[1], content)) { return 1; }
  if (not ReadFileToString(argv[2], replacements)) { return 1; }

  while (true) {
    auto index = content.find("{{{");
    if (index == std::string::npos) {
      std::printf("%*s", static_cast<int>(content.size()), content.data());
      break;
    }

    auto end_index = content.find("}}}", index);
    if (end_index == std::string::npos) { std::abort(); }

    string_view s = FindReplacement(
        replacements, content.substr(index + 3, end_index - index - 3));
    std::printf("%.*s%.*s", static_cast<int>(index), content.data(),
                static_cast<int>(s.length), s.ptr);

    content = content.substr(end_index + 3);
  }

  return 0;
}
