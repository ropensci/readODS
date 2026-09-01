#include "is_ods.h"
#include "read_ods_internals.h"
#include "readxl/zip.cpp"

cpp11::strings get_sheet_names_from_content(rapidxml::xml_node<> *rootNode,
                                            const bool include_external_data) {

  // Cache string literals to avoid repeated comparisons
  static const char *table_table = "table:table";
  static const char *table_table_source = "table:table-source";
  static const char *table_name = "table:name";

  // First pass: count sheets to avoid multiple reallocations
  int sheet_count = 0;
  for (rapidxml::xml_node<> *sheetData = rootNode->first_node(table_table);
       sheetData; sheetData = sheetData->next_sibling(table_table)) {
    if (!include_external_data && sheetData->first_node(table_table_source)) {
      continue;
    }
    sheet_count++;
  }

  // Pre-allocate with exact size
  cpp11::writable::strings sheetNames(sheet_count);

  // Second pass: collect sheet names
  int i = 0;
  for (rapidxml::xml_node<> *sheetData = rootNode->first_node(table_table);
       sheetData && i < sheet_count;
       sheetData = sheetData->next_sibling(table_table)) {

    if (!include_external_data && sheetData->first_node(table_table_source)) {
      continue;
    }

    rapidxml::xml_attribute<> *name = sheetData->first_attribute(table_name);
    sheetNames[i] =
        (name != NULL) ? Rf_mkCharCE(name->value(), CE_UTF8) : NA_STRING;
    i++;
  }

  return sheetNames;
}

[[cpp11::register]] cpp11::strings
get_sheet_names_(const std::string &file, const bool include_external_data) {
  if (!is_ods(file)) {
    throw std::invalid_argument(file + " is not a correct ODS file");
  }

  std::string xmlFile = zip_buffer(file, "content.xml");
  if (xmlFile.empty()) {
    throw std::invalid_argument("Could not extract content.xml from " + file);
  }

  rapidxml::xml_document<> spreadsheet;
  try {
    spreadsheet.parse<0>(&xmlFile[0]);
  } catch (const rapidxml::parse_error &e) {
    throw std::invalid_argument("XML parsing error: " + std::string(e.what()));
  }

  // Navigate with null checks
  rapidxml::xml_node<> *doc_node = spreadsheet.first_node();
  if (!doc_node) {
    throw std::invalid_argument("Invalid ODS structure: missing document root");
  }

  rapidxml::xml_node<> *body_node = doc_node->first_node("office:body");
  if (!body_node) {
    throw std::invalid_argument("Invalid ODS structure: missing office:body");
  }

  rapidxml::xml_node<> *rootNode = body_node->first_node("office:spreadsheet");
  if (!rootNode) {
    throw std::invalid_argument(
        "Invalid ODS structure: missing office:spreadsheet");
  }

  return get_sheet_names_from_content(rootNode, include_external_data);
}

[[cpp11::register]] cpp11::strings
get_flat_sheet_names_(const std::string &file,
                      const bool include_external_data) {
  if (!is_flat_ods(file)) {
    throw std::invalid_argument(file + " is not a correct FODS file");
  }

  // More efficient file reading
  std::ifstream in(file, std::ios::in | std::ios::binary);
  if (!in) {
    throw std::invalid_argument("No such file: " + file);
  }

  // Get file size efficiently
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

  rapidxml::xml_node<> *rootNode = body_node->first_node("office:spreadsheet");
  if (!rootNode) {
    throw std::invalid_argument(
        "Invalid ODS structure: missing office:spreadsheet");
  }

  return get_sheet_names_from_content(rootNode, include_external_data);
}
