#pragma once

#include "cpp11.hpp"
#include "cpp11/r_string.hpp"
#include "cpp11/list.hpp"
#include <R.h>
#include <Rinternals.h>
#include <fstream>
#include <iostream>

cpp11::r_string write_sheet_(const std::string& filename,
			     const cpp11::list_of<cpp11::strings>& x_list,
			     const cpp11::strings& column_types,
			     const std::string& sheet,
			     const bool row_names,
			     const bool col_names,
			     const cpp11::strings& rownames_x,
			     const cpp11::strings& colnames_x,
			     const bool na_as_string,
			     const bool padding,
			     const std::string& header,
			     const std::string& footer);
