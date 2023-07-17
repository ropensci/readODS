#include "cpp11.hpp"
#include "cpp11/r_string.hpp"
#include "cpp11/strings.hpp"

#include "rapidxml/rapidxml.hpp"
#include "readxl/zip.cpp"
#include "is_ods.h"


#include <string>



[[cpp11::register]]
cpp11::strings ods_get_sheet_names_(const std::string file, const bool include_external_data){
    if (!is_ods(file)){
        throw std::invalid_argument(file + " is not a correct ODS file");
    }
    cpp11::writable::strings sheetNames(1);

    std::string xmlFile = zip_buffer(file, "content.xml");

    rapidxml::xml_document<> spreadsheet;
    spreadsheet.parse<0>(&xmlFile[0]);
    rapidxml::xml_node<>* rootNode;


    int i = 0;
    int n = 1;
    rootNode = spreadsheet.first_node()->first_node("office:body")->
        first_node("office:spreadsheet");
    
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


