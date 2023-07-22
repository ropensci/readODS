#include "cpp11.hpp"
#include "cpp11/r_string.hpp"
#include "cpp11/strings.hpp"

#include "rapidxml/rapidxml.hpp"
#include "readxl/zip.cpp"
#include "is_ods.h"


#include <string>
#include <fstream>
#include <iostream>

cpp11::strings get_sheet_names_from_content (rapidxml::xml_node<>* rootNode, const bool include_external_data){

    cpp11::writable::strings sheetNames(1);

    int i = 0;
    int n = 1;

    for (rapidxml::xml_node<>* sheetData = rootNode->first_node("table:table"); 
            sheetData;
            sheetData = sheetData->next_sibling("table:table")){


        if (!include_external_data && sheetData->first_node("table:table-source")){
            continue;
        }
        if (i >= n) {
            n *= 2;
            sheetNames = Rf_lengthgets(sheetNames, n);
        }
        rapidxml::xml_attribute<>* name = sheetData->first_attribute("table:name");
        sheetNames[i] = (name != NULL) ? Rf_mkCharCE(name->value(), CE_UTF8) : NA_STRING;
        i++;
    }

    if (i != n) {
        sheetNames = Rf_lengthgets(sheetNames, i);
        n = i;
    }

    return sheetNames;

}



[[cpp11::register]]
cpp11::strings get_sheet_names_(const std::string file, const bool include_external_data){
    if (!is_ods(file)){
        throw std::invalid_argument(file + " is not a correct ODS file");
    }
    std::string xmlFile = zip_buffer(file, "content.xml");

    rapidxml::xml_document<> spreadsheet;
    spreadsheet.parse<0>(&xmlFile[0]);
    rapidxml::xml_node<>* rootNode;

    rootNode = spreadsheet.first_node()->first_node("office:body")->
        first_node("office:spreadsheet");
    return (get_sheet_names_from_content(rootNode, include_external_data));

}

[[cpp11::register]]
cpp11::strings get_flat_sheet_names_(const std::string file, const bool include_external_data){
    if (!is_flat_ods(file)){
        throw std::invalid_argument(file + " is not a correct FODS file");
    }
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
    rapidxml::xml_document<> spreadsheet;

    xmlFile.push_back('\0');
    spreadsheet.parse<0>(&xmlFile[0]);

    rapidxml::xml_node<>* rootNode;
    rootNode = spreadsheet.first_node("office:document")->first_node("office:body")->
        first_node("office:spreadsheet");

    return (get_sheet_names_from_content(rootNode, include_external_data));
}
