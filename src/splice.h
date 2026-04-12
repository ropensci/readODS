#pragma once

#include "cpp4r.hpp"
#include "cpp4r/r_string.hpp"
#include <fstream>
#include <ios>       // For std::ios flags
#include <memory>    // For std::unique_ptr
#include <stdexcept> // For exception handling
#include <string>

#include "rapidxml/rapidxml_ext.hpp"
#include "rapidxml/rapidxml_utils.hpp"

std::string splice_sheet_(const std::string &original_xml,
                          const std::string &sheet_xml, const bool flat);
std::string update_sheet_(const std::string &original_xml,
                          const std::string &sheet_xml, const bool flat,
                          const int sheet);
