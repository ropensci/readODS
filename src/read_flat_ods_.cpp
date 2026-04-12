#include "is_ods.h"
#include "read_ods_internals.h"

[[cpp4r::register]]
cpp4r::strings read_flat_ods_(const std::string &file, int start_row,
                              int stop_row, int start_col, int stop_col,
                              const int sheet_index,
                              const bool formula_as_formula) {
  // Validate inputs early
  if (sheet_index < 1) {
    throw std::invalid_argument("Cannot have sheet index less than 1");
  }

  if (!is_flat_ods(file)) {
    throw std::invalid_argument(file + " is not a correct FODS file");
  }

  // More efficient file reading with better memory management
  std::ifstream in(file, std::ios::in | std::ios::binary);
  if (!in) {
    throw std::invalid_argument("No such file: " + file);
  }

  // Get file size more efficiently
  in.seekg(0, std::ios::end);
  std::streamsize file_size = in.tellg();
  if (file_size <= 0) {
    throw std::invalid_argument("Empty or invalid file: " + file);
  }

  in.seekg(0, std::ios::beg);

  // Reserve memory with extra space for null terminator
  std::string xmlFile;
  xmlFile.reserve(static_cast<size_t>(file_size) + 1);
  xmlFile.resize(static_cast<size_t>(file_size));

  // Read file in one operation
  if (!in.read(&xmlFile[0], file_size)) {
    throw std::invalid_argument("Error reading file: " + file);
  }
  in.close();

  // Add null terminator for RapidXML
  xmlFile.push_back('\0');

  // Parse XML with error handling
  rapidxml::xml_document<> spreadsheet;
  try {
    spreadsheet.parse<0>(&xmlFile[0]);
  } catch (const rapidxml::parse_error &e) {
    throw std::invalid_argument("XML parsing error: " + std::string(e.what()));
  }

  // Navigate to root node with null checks
  rapidxml::xml_node<> *doc_node = spreadsheet.first_node("office:document");
  if (!doc_node) {
    throw std::invalid_argument(
        "Invalid ODS structure: missing office:document");
  }

  rapidxml::xml_node<> *body_node = doc_node->first_node("office:body");
  if (!body_node) {
    throw std::invalid_argument("Invalid ODS structure: missing office:body");
  }

  rapidxml::xml_node<> *spreadsheet_node =
      body_node->first_node("office:spreadsheet");
  if (!spreadsheet_node) {
    throw std::invalid_argument(
        "Invalid ODS structure: missing office:spreadsheet");
  }

  rapidxml::xml_node<> *rootNode = spreadsheet_node->first_node("table:table");
  if (!rootNode) {
    throw std::invalid_argument("Invalid ODS structure: missing table:table");
  }

  return read_cell_values_(rootNode, start_row, stop_row, start_col, stop_col,
                           sheet_index, formula_as_formula);
}
