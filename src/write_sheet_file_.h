#pragma once

#include "cpp11.hpp"
#include "cpp11/r_string.hpp"
#include "cpp11/list.hpp"
#include <R.h>
#include <Rinternals.h>
#include <fstream>
#include <iostream>

cpp11::r_string write_sheet_(const std::string& filename,
                             const cpp11::data_frame& x,
                             const std::string& sheet,
                             const bool row_names,
                             const bool col_names,
                             const bool na_as_string,
                             const bool padding,
                             const std::string& header,
                             const std::string& footer);
