#pragma once

#include "readxl/zip.h"
#include <string>

bool is_ods(const std::string &file);
bool is_flat_ods(const std::string &file);
