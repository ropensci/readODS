#pragma once
#include "zip.h"

#include "cpp4r/function.hpp"
#include "cpp4r/raws.hpp"

std::string zip_buffer(const std::string &zip_path,
                       const std::string &file_path) {
  cpp4r::function zip_buffer = cpp4r::package("readODS")["zip_buffer"];

  cpp4r::raws xml(zip_buffer(zip_path, file_path));
  std::string buffer(RAW(xml), RAW(xml) + xml.size());
  buffer.push_back('\0');

  return buffer;
}

bool zip_has_file(const std::string &zip_path, const std::string &file_path) {
  cpp4r::function zip_has_file = cpp4r::package("readODS")["zip_has_file"];
  return zip_has_file(zip_path, file_path);
}
