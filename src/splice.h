#pragma once

#include "cpp11.hpp"
#include "cpp11/r_string.hpp"
#include <fstream>
#include <string>

#include "rapidxml/rapidxml_ext.hpp"
#include "rapidxml/rapidxml_utils.hpp"

std::string splice_sheet_(const std::string original_xml, const std::string sheet_xml, const bool flat);
std::string update_sheet_(const std::string original_xml, const std::string sheet_xml, const bool flat, const int sheet);
