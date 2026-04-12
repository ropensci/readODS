#pragma once

#include "cpp4r.hpp"

#include "rapidxml/rapidxml.hpp"

#include <algorithm> // For std::max
#include <cstring>   // For strcmp optimizations
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

// Function declarations with const correctness and optimization hints
std::string parse_p(rapidxml::xml_node<> *node);
std::string parse_textp(rapidxml::xml_node<> *cell);
std::string parse_single_cell(rapidxml::xml_node<> *cell,
                              const bool formula_as_formula,
                              const bool use_office_value);

std::vector<std::vector<rapidxml::xml_node<> *>>
find_rows(rapidxml::xml_node<> *sheet, const int start_row, const int stop_row,
          const int start_col, const int stop_col);

cpp4r::strings read_cell_values_(rapidxml::xml_node<> *rootNode,
                                 const int start_row, const int stop_row,
                                 const int start_col, const int stop_col,
                                 const int sheet_index,
                                 const bool formula_as_formula);
