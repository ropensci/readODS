#include "is_ods.h"
#include "rapidxml/rapidxml.hpp"

#include <cstring>
#include <fstream>
#include <stdexcept>

bool is_ods(const std::string &file) {
  /*Checks that file conforms to some of the spec at
  https://docs.oasis-open.org/office/OpenDocument/v1.3/.

  It's not all of them, but if it passes all of these and isn't a spreadsheet
  something is very wrong.

  We don't care about the file extension*/

  // Fast path: Check that it contains the proper files first
  if (!zip_has_file(file, "content.xml")) {
    /*Strictly speaking this isn't required in the spec, but
    we're only interested in files with content.*/
    return false;
  }

  std::string xmlFile = zip_buffer(file, "content.xml");
  if (xmlFile.empty()) {
    return false; // Could not extract content
  }

  rapidxml::xml_document<> workbook;
  try {
    workbook.parse<0>(&xmlFile[0]);
  } catch (const rapidxml::parse_error &e) {
    if (strcmp(e.what(), "expected <")) {
      throw std::invalid_argument(file +
                                  " does not contain a valid content.xml");
    } else {
      throw std::invalid_argument("XML parse error");
    }
  }

  rapidxml::xml_node<> *rootNode = workbook.first_node();
  if (!rootNode) {
    return false;
  }

  // Cache string literals for performance
  static const char *office_document_content = "office:document-content";
  static const char *office_body = "office:body";
  static const char *office_spreadsheet = "office:spreadsheet";

  /*Check Section 2.2.1 B) 2.1 - is this a well formed OpenDocument*/
  if (strcmp(rootNode->name(), office_document_content) != 0) {
    return false;
  }

  /*Check Section 3.3 C)*/
  rapidxml::xml_node<> *body_node = rootNode->first_node(office_body);
  if (!body_node) {
    return false;
  }

  /*Check Section 2.2.4 C) - this is a spreadsheet*/
  if (!body_node->first_node(office_spreadsheet)) {
    return false;
  }

  return true;
}

bool is_flat_ods(const std::string &file) {
  /*Checks that file conforms to some of the spec at
  https://docs.oasis-open.org/office/OpenDocument/v1.3/.*/

  // More efficient file reading
  std::ifstream in(file, std::ios::in | std::ios::binary);
  if (!in) {
    return false; // File doesn't exist - not a valid FODS
  }

  // Get file size efficiently
  in.seekg(0, std::ios::end);
  std::streamsize file_size = in.tellg();
  if (file_size <= 0) {
    return false; // Empty file can't be FODS
  }

  in.seekg(0, std::ios::beg);

  // Reserve memory with extra space for null terminator
  std::string xmlFile;
  xmlFile.reserve(static_cast<size_t>(file_size) + 1);
  xmlFile.resize(static_cast<size_t>(file_size));

  // Read file in one operation
  if (!in.read(&xmlFile[0], file_size)) {
    return false; // Read error
  }
  in.close();

  // Add null terminator for RapidXML
  xmlFile.push_back('\0');

  rapidxml::xml_document<> workbook;
  try {
    workbook.parse<0>(&xmlFile[0]);
  } catch (const rapidxml::parse_error &e) {
    if (strcmp(e.what(), "expected <")) {
      throw std::invalid_argument(file + " is not a flat XML file");
    } else {
      throw std::invalid_argument("XML parse error");
    }
  }

  // Cache string literals for performance
  static const char *office_document = "office:document";
  static const char *office_body = "office:body";
  static const char *office_spreadsheet = "office:spreadsheet";

  // Section 2.2.1C)
  rapidxml::xml_node<> *rootNode = workbook.first_node(office_document);
  if (!rootNode) {
    return false;
  }

  /*Check Section 3.3 C)*/
  rapidxml::xml_node<> *body_node = rootNode->first_node(office_body);
  if (!body_node) {
    return false;
  }

  /*Check Section 2.2.4 C) - this is a spreadsheet*/
  if (!body_node->first_node(office_spreadsheet)) {
    return false;
  }

  return true;
}
