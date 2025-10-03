#include "splice.h"

[[cpp11::register]]
std::string splice_sheet_(const std::string &original_xml,
                          const std::string &sheet_file, const bool flat) {
  try {
    rapidxml::xml_document<> spreadsheet1;

    // Use RAII for automatic memory management
    std::unique_ptr<rapidxml::file<>> xml_file(
        new rapidxml::file<>(original_xml.c_str()));
    spreadsheet1.parse<rapidxml::parse_fastest>((char *)xml_file->data());

    // Cache string literals for performance
    static const char *office_body = "office:body";
    static const char *office_spreadsheet = "office:spreadsheet";
    static const char *office_document = "office:document";
    static const char *table_table = "table:table";

    rapidxml::xml_node<> *root_node = nullptr;
    if (!flat) {
      rapidxml::xml_node<> *doc_node = spreadsheet1.first_node();
      if (!doc_node)
        throw std::runtime_error("Invalid XML structure");

      rapidxml::xml_node<> *body_node = doc_node->first_node(office_body);
      if (!body_node)
        throw std::runtime_error("Missing office:body");

      root_node = body_node->first_node(office_spreadsheet);
      if (!root_node)
        throw std::runtime_error("Missing office:spreadsheet");
    } else {
      rapidxml::xml_node<> *doc_node = spreadsheet1.first_node(office_document);
      if (!doc_node)
        throw std::runtime_error("Missing office:document");

      rapidxml::xml_node<> *body_node = doc_node->first_node(office_body);
      if (!body_node)
        throw std::runtime_error("Missing office:body");

      root_node = body_node->first_node(office_spreadsheet);
      if (!root_node)
        throw std::runtime_error("Missing office:spreadsheet");
    }

    rapidxml::xml_document<> spreadsheet2;
    std::unique_ptr<rapidxml::file<>> xml_file2(
        new rapidxml::file<>(sheet_file.c_str()));
    spreadsheet2.parse<rapidxml::parse_fastest>((char *)xml_file2->data());

    rapidxml::xml_node<> *root_node2 = spreadsheet2.first_node(table_table);
    if (!root_node2)
      throw std::runtime_error("Missing table:table in sheet file");

    rapidxml::xml_node<> *new_node = spreadsheet2.clone_node(root_node2);
    root_node->append_node(new_node);

    // More efficient file writing
    std::ofstream output_file(original_xml, std::ios::out | std::ios::trunc);
    if (!output_file)
      throw std::runtime_error("Cannot open output file");

    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;

    return original_xml;

  } catch (const std::exception &e) {
    throw std::runtime_error("Error in splice_sheet_: " +
                             std::string(e.what()));
  }
}

[[cpp11::register]]
std::string update_sheet_(const std::string &original_xml,
                          const std::string &sheet_file, const bool flat,
                          const int sheet_index) {
  try {
    if (sheet_index < 1) {
      throw std::invalid_argument("Sheet index must be >= 1");
    }

    rapidxml::xml_document<> spreadsheet1;
    std::unique_ptr<rapidxml::file<>> xml_file(
        new rapidxml::file<>(original_xml.c_str()));
    spreadsheet1.parse<rapidxml::parse_fastest>((char *)xml_file->data());

    // Cache string literals for performance
    static const char *office_body = "office:body";
    static const char *office_spreadsheet = "office:spreadsheet";
    static const char *office_document = "office:document";
    static const char *table_table = "table:table";

    rapidxml::xml_node<> *root_node = nullptr;
    rapidxml::xml_node<> *parent_node = nullptr;

    if (!flat) {
      rapidxml::xml_node<> *doc_node = spreadsheet1.first_node();
      if (!doc_node)
        throw std::runtime_error("Invalid XML structure");

      rapidxml::xml_node<> *body_node = doc_node->first_node(office_body);
      if (!body_node)
        throw std::runtime_error("Missing office:body");

      parent_node = body_node->first_node(office_spreadsheet);
      if (!parent_node)
        throw std::runtime_error("Missing office:spreadsheet");

      root_node = parent_node->first_node(table_table);
    } else {
      rapidxml::xml_node<> *doc_node = spreadsheet1.first_node(office_document);
      if (!doc_node)
        throw std::runtime_error("Missing office:document");

      rapidxml::xml_node<> *body_node = doc_node->first_node(office_body);
      if (!body_node)
        throw std::runtime_error("Missing office:body");

      parent_node = body_node->first_node(office_spreadsheet);
      if (!parent_node)
        throw std::runtime_error("Missing office:spreadsheet");

      root_node = parent_node->first_node(table_table);
    }

    if (!root_node)
      throw std::runtime_error("No sheets found");

    // Navigate to the target sheet more efficiently
    for (int i = 1; i < sheet_index; i++) {
      root_node = root_node->next_sibling(table_table);
      if (!root_node) {
        throw std::runtime_error("Sheet index " + std::to_string(sheet_index) +
                                 " not found");
      }
    }

    rapidxml::xml_document<> spreadsheet2;
    std::unique_ptr<rapidxml::file<>> xml_file2(
        new rapidxml::file<>(sheet_file.c_str()));
    spreadsheet2.parse<rapidxml::parse_fastest>((char *)xml_file2->data());

    rapidxml::xml_node<> *root_node2 = spreadsheet2.first_node(table_table);
    if (!root_node2)
      throw std::runtime_error("Missing table:table in sheet file");

    rapidxml::xml_node<> *new_node = spreadsheet2.clone_node(root_node2);
    parent_node->insert_node(root_node, new_node);
    parent_node->remove_node(root_node);

    // More efficient file writing
    std::ofstream output_file(original_xml, std::ios::out | std::ios::trunc);
    if (!output_file)
      throw std::runtime_error("Cannot open output file");

    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;

    return original_xml;

  } catch (const std::exception &e) {
    throw std::runtime_error("Error in update_sheet_: " +
                             std::string(e.what()));
  }
}
