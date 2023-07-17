#include "is_ods.h"
#include "rapidxml/rapidxml.hpp"



#include <cstring>


bool is_ods(const std::string file, const bool strict){
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


    /*Mimetype is not in v1.0 so mostly we ignore this. Keeping this here in case it's useful later
    as it is a requirement of later versions*/
    if(strict) {
        if (!zip_has_file(file, "mimetype")){
            return false;
        }
        /*Check Section 2.2.4 B)*/
        std::string mimetype = zip_buffer(file, "mimetype");
        mimetype = mimetype.replace(mimetype.end()-1,mimetype.end(),""); // This is some very lazy string trimming
        if (!(strcmp(
            mimetype.c_str(),
            "application/vnd.oasis.opendocument.spreadsheet" // We also don't accept templates
        ) == 0)){
            return false;
        }
    }
    rapidxml::xml_document<> workbook;
    rapidxml::xml_node<>* rootNode;
    std:: string xmlFile = zip_buffer(file, "content.xml");
    workbook.parse<0>(&xmlFile[0]);
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