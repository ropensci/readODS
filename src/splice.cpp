#include "is_ods.h"
#include "read_ods_internals.h"
#include "write_sheet_.h"
#include "rapidxml/rapidxml_utils.hpp"

[[cpp11::register]]
std::string splice_sheet(std::string original_xml, std::string sheet_xml, bool flat) {
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
    // rootNode = spreadsheet1.first_node("office:spreadsheet");
    // std::cout << rootNode->name();

    rapidxml::xml_document<> spreadsheet2;
    rapidxml::file<> *xmlFile2 = new rapidxml::file<>(sheet_xml.c_str());
    spreadsheet2.parse<0>((char*)xmlFile2->data());
    rapidxml::xml_node<> *rootNode2;
    rootNode2 = spreadsheet2.first_node("table:table");
    rapidxml::xml_node<> *newnode = spreadsheet2.clone_node(rootNode2);
    // std::cout << newnode->name();
    // std::cout << rootNode->name();

    rootNode->append_node(newnode);

    std::ofstream output_file(original_xml);
    output_file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_file << spreadsheet1;
    output_file.close();
    delete xmlFile;
    delete xmlFile2;
    return original_xml;
}
