#include "is_ods.h"
#include "read_ods_internals.h"

[[cpp11::register]]
cpp11::strings read_flat_ods_(const std::string file,
                              int start_row, int stop_row, int start_col, int stop_col,
                              const int sheet_index,
                              const bool formula_as_formula) {
    if(!is_flat_ods(file)){
        throw std::invalid_argument(file + " is not a correct FODS file");
    }
    if(sheet_index < 1){
        throw std::invalid_argument("Cannot have sheet index less than 1");
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
        first_node("office:spreadsheet")->first_node("table:table");
    return read_cell_values_(rootNode,
                             start_row,
                             stop_row,
                             start_col,
                             stop_col,
                             sheet_index,
                             formula_as_formula);
}
