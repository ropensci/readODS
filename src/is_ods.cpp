#include "is_ods.h"
#include "rapidxml/rapidxml.hpp"

#include <cstring>
#include <fstream>

bool is_ods(const std::string file){
    /*Checks that file conforms to some of the spec at
    https://docs.oasis-open.org/office/OpenDocument/v1.3/.

    It's not all of them, but if it passes all of these and isn't a spreadsheet
    something is very wrong.

    We don't care about the file extension*/
    /*Check that it contains the proper files*/
    if (!zip_has_file(file, "content.xml")){
        /*Strictly speaking this isn't required in the spec, but 
        we're only interested in files with content.*/
        return false;
    }

    rapidxml::xml_document<> workbook;
    rapidxml::xml_node<>* rootNode;
    std:: string xmlFile = zip_buffer(file, "content.xml");
    try {
        workbook.parse<0>(&xmlFile[0]);
    } catch (const rapidxml::parse_error& e) {
        if (strcmp(e.what(), "expected <")){
            throw std::invalid_argument(file + " does not contain a valid content.xml");
        } else {
            throw std::invalid_argument("XML parse error");
        }
    }
    rootNode = workbook.first_node();
    /*Check Section 2.2.1 B) 2.1 - is this a well formed OpenDocument*/
    if (strcmp(rootNode->name(),"office:document-content") != 0){
        return false;
    }
    /*Check Section 3.3 C)*/
    if (!(rootNode->first_node("office:body"))){
        return false;
    }
    /*Check Section 2.2.4 C) - this is a spreadsheet*/ 
    if (!(rootNode->first_node("office:body")->first_node("office:spreadsheet"))){
        return false;
    }
    return true;
}

bool is_flat_ods(const std::string file){
    /*Checks that file conforms to some of the spec at
    https://docs.oasis-open.org/office/OpenDocument/v1.3/.*/
    rapidxml::xml_document<> workbook;
    rapidxml::xml_node<>* rootNode;
    std::string xmlFile;

    std::ifstream in(file, std::ios::in | std::ios::binary);
    if (in) {
        in.seekg(0, std::ios::end);
        xmlFile.resize(in.tellg());
        in.seekg(0, std::ios::beg);
        in.read(&xmlFile[0], xmlFile.size());
        in.close();
    } else{
        throw std::invalid_argument("No such file");
    }

    xmlFile.push_back('\0');
    
    try {
        workbook.parse<0>(&xmlFile[0]);
    } catch (const rapidxml::parse_error& e) {
        if (strcmp(e.what(), "expected <")){
            throw std::invalid_argument(file + " is not a flat XML file");
        } else {
            throw std::invalid_argument("XML parse error");
        }
    }

    rootNode = workbook.first_node();
    // Section 2.2.1C)
    while(rootNode != 0 && strcmp(rootNode->name(), "office:document") != 0){
        rootNode->next_sibling();
    }
    if (rootNode == 0){
        return false;
    }

    /*Check Section 3.3 C)*/
    if (!(rootNode->first_node("office:body"))){
        return false;
    }
    /*Check Section 2.2.4 C) - this is a spreadsheet*/ 
    if (!(rootNode->first_node("office:body")->first_node("office:spreadsheet"))){
        return false;
    }
    
    return true;
}
