#include "is_ods.h"
#include "read_ods_internals.h"

[[cpp4r::register]]
cpp4r::strings read_ods_(const std::string &file, int start_row, int stop_row,
                         int start_col, int stop_col, const int sheet_index,
                         const bool formula_as_formula) {
  // Validate inputs early
  if (sheet_index < 1) {
    throw std::invalid_argument("Cannot have sheet index less than 1");
  }

  if (!is_ods(file)) {
    throw std::invalid_argument(file + " is not a correct ODS file");
  }

  // Extract XML content from ZIP
  std::string xmlFile = zip_buffer(file, "content.xml");
  if (xmlFile.empty()) {
    throw std::invalid_argument("Could not extract content.xml from " + file);
  }

  // Parse XML with error handling
  rapidxml::xml_document<> spreadsheet;
  try {
    spreadsheet.parse<0>(&xmlFile[0]);
  } catch (const rapidxml::parse_error &e) {
    throw std::invalid_argument("XML parsing error: " + std::string(e.what()));
  }

  // Navigate to root node with null checks
  rapidxml::xml_node<> *doc_node = spreadsheet.first_node();
  if (!doc_node) {
    throw std::invalid_argument("Invalid ODS structure: missing document root");
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
