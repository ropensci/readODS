#pragma once

#include "cpp4r.hpp"
#include "cpp4r/list.hpp"
#include "cpp4r/r_string.hpp"
#include <R.h>
#include <Rinternals.h>
#include <fstream>
#include <ios> // For std::ios flags
#include <iostream>
#include <stdexcept> // For exception handling
#include <string>    // For std::string operations

cpp4r::r_string write_sheet_(const std::string &filename,
                             const cpp4r::data_frame &x,
                             const std::string &sheet, const bool row_names,
                             const bool col_names, const bool na_as_string,
                             const bool padding, const std::string &header,
                             const std::string &footer);
