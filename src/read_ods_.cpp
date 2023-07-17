#include "cpp11.hpp"
#include "cpp11/r_string.hpp"

#include "rapidxml/rapidxml.hpp"
#include "is_ods.h"

#include <vector>
#include <string>
#include <iostream>






std::string parse_p(rapidxml::xml_node<>* node){
    /*Deal with text inside cells. Cells can contain just text (node_data), or a 
    mixture of text and other nodes (node_element). We usually just want the text
    from these nodes (e.g. if there's a link), but we also need to consider the 
    text:s node, which saves repeated spaces*/
    std::string out;
    char* name;
    int rep_space;
    for (rapidxml::xml_node<>* n = node->first_node(); n; n=n->next_sibling()){
        if (n->type() == rapidxml::node_element)
        {
            name = n->name();
            if (strcmp(name,"text:s") == 0){
                if(n->first_attribute("text:c") != NULL){
                    rep_space = atoi(n->first_attribute("text:c")->value());
                } else {
                    rep_space = 1;
                }
                out = out.append(std::string(rep_space, ' '));
            } else if (strcmp(name,"text:line-break") == 0){
                out = out.append("\n");
            } else if (strcmp(name, "text:a") == 0){
                if(!(n->first_node("text:a"))){ //Prevent crash by making pathological recursive links
                    out = out.append(parse_p(n));
                }
            } else {
            out = out.append(n->value());
            }
        }
        else if (n->type() == rapidxml::node_data){
            out = out.append(n->value());
        }
    }
    return out;
}

std::string parse_textp(rapidxml::xml_node<>* cell){
    //This isn't very efficient. It is theoretically faster to make a list of pointers, assign the 
    //memory first and then concatenate them all into the freed memory. However this is hard to understand
    //and not a significant problem. If you were looking for efficincies though, this would be a good choice.
    std::string out;
    int i = 0;
    for (rapidxml::xml_node<>* n = cell->first_node("text:p"); n ; n=n->next_sibling("text:p")){
        if (i > 0){
            out = out.append("\n");
        }
        out = out.append(parse_p(n));
        i++;
    }
    return out;
}

std::string parse_single_cell(rapidxml::xml_node<>* cell, bool formula_as_formula, bool use_office_value){
    std::string cell_value;

    char* value_type = (cell->first_attribute("office:value-type") != 0) ? 
        cell->first_attribute("office:value-type")->value() : NULL;
    if(formula_as_formula && cell->first_attribute("table:formula")){
        cell_value = cell->first_attribute("table:formula")->value();
    } else {
        cell_value = (cell->first_node("text:p") != 0) ? parse_textp(cell) : "";
        if((value_type) && 
            
            ((cell_value.length() == 0 && use_office_value) ||
            (strcmp(value_type, "float") == 0 ||
             strcmp(value_type, "currency") == 0||
             strcmp(value_type, "percentage") == 0))){

            cell_value = cell->first_attribute("office:value")->value();
        } 
    }
    return cell_value;
}

// Make an array of pointers to each cell
std::vector<std::vector<rapidxml::xml_node<>*>> find_rows(rapidxml::xml_node<>* sheet, 
                int start_row,
                const int stop_row,
                int start_col,
                const int stop_col){
    
    /*Rows and columns are 1-based because both Excel and R treat arrays
    this way*/
    int row_repeat_count;
    int col_repeat_count;
    rapidxml::xml_node<>* row = sheet->first_node("table:table-row");
    rapidxml::xml_node<>* cell;

    if (start_row < 1){
        start_row = 1;
    }
    if (start_col < 1){
        start_col = 1;
    }
    int nrows = stop_row - start_row + 1;

    std::vector<std::vector<rapidxml::xml_node<>*>> rows((nrows < 1) ? 1 : nrows);

    for (int i = 1; i <= stop_row || stop_row < 1; ){
        // i keeps track of what nominal row we are on


        // Check for row repeats
        if (row->first_attribute("table:number-rows-repeated") == nullptr){
            row_repeat_count = 1;    
        } else {
            row_repeat_count = std::atoi(row->first_attribute("table:number-rows-repeated")->value());
        }
        // Stop if all repeats done, or if we're at the last requested row
        for (int r_repeat = 0; r_repeat < row_repeat_count && (stop_row < 1 || r_repeat + i <= stop_row); r_repeat++){

            // Check size of container.
            if ((int)rows.size() < i - start_row + 1){
                rows.resize(rows.size() * 2);
            }
            // If this row is blank (i.e. it contains only one or no children, which have no contents)
            if (row->first_node()->next_sibling() == 0 && row->first_node()->first_node() == 0){
                // Look ahead. If this is the last row, stop, otherwise add a blank row
                if(row->next_sibling() == 0){
                    break;
                }
                // Otherwise leave the row blank

                // if row is not blank, and in range deal with cells
            } else if(i + r_repeat >= start_row) {
                unsigned int last_non_blank = 0;
                cell = row->first_node("table:table-cell");
                for (int j = 1; j <= stop_col || stop_col < 1; ){
                    // Check for column repeats
                    if (cell->first_attribute("table:number-columns-repeated")){
                        col_repeat_count = std::atoi(cell->first_attribute("table:number-columns-repeated")->value());
                    } else {
                        col_repeat_count = 1;
                    }

                    // Stop if all column repeats done, or if we're at the last requested row
                    for (int c_repeat = 0; c_repeat < col_repeat_count && (stop_col < 1 || c_repeat + j <= stop_col); c_repeat++){
                        bool is_blank = true;
                        // If this cell is blank (i.e. contains no children)
                        if (cell->first_node() == 0){
                            // Look ahead. If this is the last column, stop.
                            if(cell->next_sibling() == 0){
                                break;
                            }
                        } else {
                            // Otherwise mark that cell is not blank
                            is_blank = false;
                        }
                        // If we're in range add pointer to the array
                        if (stop_col < 1 || j + c_repeat >= start_col){
                            rows[i - start_row].push_back(cell);
                            if(!is_blank){
                                last_non_blank = rows[i - start_row].size();
                            }
                        }
    
                    j++;

                    }
                    cell = cell->next_sibling("table:table-cell");
                    // If that was the last cell, stop.
                    if (cell == 0){
                        break;
                    }

                }
                // Remove trailing blank cells
                rows[i - start_row].resize(last_non_blank);
                
            }
            i++;
        }
        row = row->next_sibling("table:table-row");
        // If that was the last row, stop.
        if (row == 0){
            break;
        }

    }
    // Remove trailing empty elements
    unsigned int rowsize = 0;
    for (unsigned int i = 0; i < rows.size(); i++){
        if(rows[i].size() > 0){
            rowsize = i;
        }
    }
    rows.resize(rowsize + 1);
    return rows;
}

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