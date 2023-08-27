#include "splice.h"

[[cpp11::register]]
std::string splice_sheet(const std::string original_xml, const std::string sheet_xml, const bool flat) {
    rapidxml::xml_document<> spreadsheet1;
    // read the content in heap
    rapidxml::file<> *xmlFile = new rapidxml::file<>(original_xml.c_str());
    spreadsheet1.parse<0>((char*)xmlFile->data());
    rapidxml::xml_node<>* rootNode;
    if (!flat) {
        rootNode = spreadsheet1.first_node()->first_node("office:body")->
            first_node("office:spreadsheet");
    } else {
        rootNode = spreadsheet1.first_node("office:document")->first_node("office:body")->
            first_node("office:spreadsheet");
    }
    rapidxml::xml_document<> spreadsheet2;
    rapidxml::file<> *xmlFile2 = new rapidxml::file<>(sheet_xml.c_str());
    spreadsheet2.parse<0>((char*)xmlFile2->data());
    rapidxml::xml_node<> *rootNode2;
    rootNode2 = spreadsheet2.first_node("table:table");
    rapidxml::xml_node<> *newnode = spreadsheet2.clone_node(rootNode2);
    rootNode->append_node(newnode);
    std::ofstream output_file(original_xml);
    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;
    output_file.close();
    delete xmlFile;
    delete xmlFile2;
    return original_xml;
}

[[cpp11::register]]
std::string update_sheet(const std::string original_xml, const std::string sheet_xml, const bool flat, const int sheet) {
    rapidxml::xml_document<> spreadsheet1;
    rapidxml::file<> *xmlFile = new rapidxml::file<>(original_xml.c_str());
    spreadsheet1.parse<0>((char*)xmlFile->data());
    rapidxml::xml_node<>* rootNode;
    rapidxml::xml_node<>* parNode;

    if (!flat) {
        rootNode = spreadsheet1.first_node()->first_node("office:body")->
            first_node("office:spreadsheet")->first_node("table:table");
        parNode = spreadsheet1.first_node()->first_node("office:body")->
            first_node("office:spreadsheet");

    } else {
        rootNode = spreadsheet1.first_node("office:document")->first_node("office:body")->
            first_node("office:spreadsheet")->first_node("table:table");
        parNode = spreadsheet1.first_node("office:document")->first_node("office:body")->
            first_node("office:spreadsheet");
    }
    for (int i = 1; i < sheet; i++){
        rootNode = rootNode->next_sibling("table:table");
    }

    rapidxml::xml_document<> spreadsheet2;
    rapidxml::file<> *xmlFile2 = new rapidxml::file<>(sheet_xml.c_str());
    spreadsheet2.parse<0>((char*)xmlFile2->data());
    rapidxml::xml_node<> *rootNode2;
    rootNode2 = spreadsheet2.first_node("table:table");
    rapidxml::xml_node<> *newnode = spreadsheet2.clone_node(rootNode2);
    parNode->insert_node(rootNode, newnode);
    parNode->remove_node(rootNode);
    std::ofstream output_file(original_xml);
    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;
    output_file.close();
    delete xmlFile;
    delete xmlFile2;
    return original_xml;
}
