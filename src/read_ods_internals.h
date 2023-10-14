#pragma once

#include "cpp11.hpp"
#include "cpp11/r_string.hpp"

#include "rapidxml/rapidxml.hpp"

#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>

std::string parse_p(rapidxml::xml_node<>* node);
std::string parse_textp(rapidxml::xml_node<>* cell);
std::string parse_single_cell(rapidxml::xml_node<>* cell, bool formula_as_formula, bool use_office_value);
std::vector<std::vector<rapidxml::xml_node<>*>> find_rows(rapidxml::xml_node<>* sheet, 
                int start_row,
                const int stop_row,
                int start_col,
                const int stop_col);

cpp11::strings read_cell_values_(rapidxml::xml_node<>* rootNode,
                                 int start_row,
                                 int stop_row,
                                 int start_col,
                                 int stop_col,
                                 const int sheet_index,
                                 const bool formula_as_formula);
