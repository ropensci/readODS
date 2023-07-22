#pragma once
#include "zip.h"

#include "cpp11/function.hpp"
#include "cpp11/raws.hpp"


std::string zip_buffer(const std::string& zip_path,
                       const std::string& file_path) {
  cpp11::function zip_buffer = cpp11::package("readODS")["zip_buffer"];

  cpp11::raws xml(zip_buffer(zip_path, file_path));
  std::string buffer(RAW(xml), RAW(xml) + xml.size());
  buffer.push_back('\0');

  return buffer;
}

bool zip_has_file(const std::string& zip_path,
                  const std::string& file_path) {
  cpp11::function zip_has_file = cpp11::package("readODS")["zip_has_file"];
  return zip_has_file(zip_path, file_path);
}
