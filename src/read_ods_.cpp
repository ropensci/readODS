#include "is_ods.h"
#include "read_ods_internals.h"

[[cpp11::register]]
cpp11::strings read_ods_(const std::string file,
    int start_row,
    int stop_row,
    int start_col,
    int stop_col,
    const int sheet_index,
    const bool formula_as_formula) {
    if(!is_ods(file)){
        throw std::invalid_argument(file + " is not a correct ODS file");
    }
    if(sheet_index < 1){
        throw std::invalid_argument("Cannot have sheet index less than 1");
    }
    std::string xmlFile = zip_buffer(file, "content.xml");
    rapidxml::xml_document<> spreadsheet;
    spreadsheet.parse<0>(&xmlFile[0]);
    rapidxml::xml_node<>* rootNode;
    rootNode = spreadsheet.first_node()->first_node("office:body")->
        first_node("office:spreadsheet")->first_node("table:table");
    return read_cell_values_(rootNode,
                             start_row,
                             stop_row,
                             start_col,
                             stop_col,
                             sheet_index,
                             formula_as_formula);
}
