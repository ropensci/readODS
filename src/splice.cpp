#include "splice.h"

[[cpp11::register]]
std::string splice_sheet_(const std::string original_xml, const std::string sheet_file, const bool flat) {
    rapidxml::xml_document<> spreadsheet1;
    // read the content in heap
    rapidxml::file<> *xml_file = new rapidxml::file<>(original_xml.c_str());
    spreadsheet1.parse<0>((char*)xml_file->data());
    rapidxml::xml_node<>* root_node;
    if (!flat) {
        root_node = spreadsheet1.first_node()->first_node("office:body")->
            first_node("office:spreadsheet");
    } else {
        root_node = spreadsheet1.first_node("office:document")->first_node("office:body")->
            first_node("office:spreadsheet");
    }
    rapidxml::xml_document<> spreadsheet2;
    rapidxml::file<> *xml_file2 = new rapidxml::file<>(sheet_file.c_str());
    spreadsheet2.parse<0>((char*)xml_file2->data());
    rapidxml::xml_node<> *root_node2;
    root_node2 = spreadsheet2.first_node("table:table");
    rapidxml::xml_node<> *new_node = spreadsheet2.clone_node(root_node2);
    root_node->append_node(new_node);
    std::ofstream output_file(original_xml);
    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;
    output_file.close();
    delete xml_file;
    delete xml_file2;
    return original_xml;
}

[[cpp11::register]]
std::string update_sheet_(const std::string original_xml, const std::string sheet_file, const bool flat, const int sheet_index) {
    rapidxml::xml_document<> spreadsheet1;
    rapidxml::file<> *xml_file = new rapidxml::file<>(original_xml.c_str());
    spreadsheet1.parse<0>((char*)xml_file->data());
    rapidxml::xml_node<>* root_node;
    rapidxml::xml_node<>* parent_node;

    if (!flat) {
        root_node = spreadsheet1.first_node()->first_node("office:body")->
            first_node("office:spreadsheet")->first_node("table:table");
        parent_node = spreadsheet1.first_node()->first_node("office:body")->
            first_node("office:spreadsheet");

    } else {
        root_node = spreadsheet1.first_node("office:document")->first_node("office:body")->
            first_node("office:spreadsheet")->first_node("table:table");
        parent_node = spreadsheet1.first_node("office:document")->first_node("office:body")->
            first_node("office:spreadsheet");
    }
    for (int i = 1; i < sheet_index; i++){
        root_node = root_node->next_sibling("table:table");
    }

    rapidxml::xml_document<> spreadsheet2;
    rapidxml::file<> *xml_file2 = new rapidxml::file<>(sheet_file.c_str());
    spreadsheet2.parse<0>((char*)xml_file2->data());
    rapidxml::xml_node<> *root_node2;
    root_node2 = spreadsheet2.first_node("table:table");
    rapidxml::xml_node<> *new_node = spreadsheet2.clone_node(root_node2);
    parent_node->insert_node(root_node, new_node);
    parent_node->remove_node(root_node);
    std::ofstream output_file(original_xml);
    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;
    output_file.close();
    delete xml_file;
    delete xml_file2;
    return original_xml;
}
