#include "cpp11.hpp"
#include "cpp11/r_string.hpp"

#include "rapidxml/rapidxml.hpp"
#include "is_ods.h"
#include "read_ods_internals.h"

#include <vector>
#include <string>

[[cpp11::register]]
cpp11::strings read_ods_(const std::string file,
    int start_row,
    int stop_row,
    int start_col,
    int stop_col,
 const int sheet,
    const bool formula_as_formula) {
    if(!is_ods(file)){
        throw std::invalid_argument(file + " is not a correct ODS file");
    } 
    if(sheet < 1){
        throw std::invalid_argument("Cannot have sheet index less than 1");
    }

    unsigned int out_width = 0;
    unsigned int out_length;

    std::string xmlFile = zip_buffer(file, "content.xml");
    rapidxml::xml_document<> spreadsheet;
    spreadsheet.parse<0>(&xmlFile[0]);
    rapidxml::xml_node<>* rootNode;
    rootNode = spreadsheet.first_node()->first_node("office:body")->
        first_node("office:spreadsheet")->first_node("table:table");

    for (int i = 1; i < sheet; i++){
        rootNode = rootNode->next_sibling("table:table");
    }

    std::vector<std::vector<rapidxml::xml_node<>*>> contents;

    contents = find_rows(rootNode, start_row,stop_row,start_col,stop_col);

    // Get dimensions of output
    out_length = contents.size();
    for (unsigned int i = 0; i < contents.size(); i++){
        if (contents[i].size() > out_width){
            out_width = contents[i].size();
        }
    }

    // If there is no content
    if (out_width * out_length == 0){
        cpp11::writable::strings cell_values(2);
        cell_values[0] = "0";
        cell_values[1] = "0";
        return cell_values;
    }

    cpp11::writable::strings cell_values(out_width*out_length + 2);
    cell_values[0] = std::to_string(out_width);
    cell_values[1] = std::to_string(out_length);

    int t = 2;
    for (unsigned int i = 0; i < contents.size(); i++){
        for (unsigned int j = 0; j < contents[i].size(); j++){
            cell_values[t] = (contents[i][j] != 0) ?
                Rf_mkCharCE(parse_single_cell(contents[i][j], formula_as_formula, true).c_str(), CE_UTF8) : NA_STRING;
            t++;
        }
        // Pad rows to even width
        if(contents[i].size() < out_width){
            unsigned int row_width = contents[i].size();
            for (unsigned int j = 0; j + row_width < out_width; j++){
                cell_values[t] = "";
                t++;
            }
        }
    }
    return cell_values;
 }

 