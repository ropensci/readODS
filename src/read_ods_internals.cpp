#include "read_ods_internals.h"

std::string parse_p(rapidxml::xml_node<> *node) {
  /*Deal with text inside cells. Cells can contain just text (node_data), or a
  mixture of text and other nodes (node_element). We usually just want the text
  from these nodes (e.g. if there's a link), but we also need to consider the
  text:s node, which saves repeated spaces*/
  std::string out;
  out.reserve(256); // Pre-allocate memory to reduce reallocations
  char *name;
  int rep_space;

  // Cache commonly used string literals
  static const char *text_s = "text:s";
  static const char *text_line_break = "text:line-break";
  static const char *text_a = "text:a";
  static const char *text_c = "text:c";

  for (rapidxml::xml_node<> *n = node->first_node(); n; n = n->next_sibling()) {
    if (n->type() == rapidxml::node_element) {
      name = n->name();
      if (strcmp(name, text_s) == 0) {
        if (n->first_attribute(text_c) != NULL) {
          rep_space = atoi(n->first_attribute(text_c)->value());
        } else {
          rep_space = 1;
        }
        out.append(rep_space, ' '); // More efficient than creating temp string
      } else if (strcmp(name, text_line_break) == 0) {
        out += '\n'; // More efficient than append for single chars
      } else if (strcmp(name, text_a) == 0) {
        if (!(n->first_node(text_a))) { // Prevent crash by making pathological
                                        // recursive links
          out += parse_p(n);
        }
      } else {
        if (n->value())
          out += n->value(); // Check for null to avoid crashes
      }
    } else if (n->type() == rapidxml::node_data) {
      if (n->value())
        out += n->value();
    }
  }
  return out;
}

std::string parse_textp(rapidxml::xml_node<> *cell) {
  std::string out;
  out.reserve(512); // Pre-allocate memory
  bool first = true;

  static const char *text_p = "text:p";

  for (rapidxml::xml_node<> *n = cell->first_node(text_p); n;
       n = n->next_sibling(text_p)) {
    if (n->first_node()) {
      if (!first) {
        out += '\n';
      }
      out += parse_p(n);
      first = false;
    }
  }
  return out;
}

std::string parse_single_cell(rapidxml::xml_node<> *cell,
                              const bool formula_as_formula,
                              const bool use_office_value) {
  std::string cell_value;
  cell_value.reserve(64); // Pre-allocate small buffer

  static const char *office_value_type = "office:value-type";
  static const char *table_formula = "table:formula";
  static const char *text_p = "text:p";
  static const char *office_value = "office:value";
  static const char *float_type = "float";
  static const char *currency_type = "currency";
  static const char *percentage_type = "percentage";

  char *value_type = (cell->first_attribute(office_value_type) != 0)
                         ? cell->first_attribute(office_value_type)->value()
                         : NULL;

  if (formula_as_formula && cell->first_attribute(table_formula)) {
    cell_value = cell->first_attribute(table_formula)->value();
  } else {
    rapidxml::xml_node<> *text_node = cell->first_node(text_p);
    if (text_node != 0) {
      cell_value = parse_textp(cell);
    }

    if (value_type && ((cell_value.empty() && use_office_value &&
                        cell->first_attribute(office_value) != 0) ||
                       ((strcmp(value_type, float_type) == 0 ||
                         strcmp(value_type, currency_type) == 0 ||
                         strcmp(value_type, percentage_type) == 0)))) {
      rapidxml::xml_attribute<> *office_val =
          cell->first_attribute(office_value);
      if (office_val) {
        cell_value = office_val->value();
      }
    }
  }
  return cell_value;
}

// Make an array of pointers to each cell
std::vector<std::vector<rapidxml::xml_node<> *>>
find_rows(rapidxml::xml_node<> *sheet, const int start_row, const int stop_row,
          const int start_col, const int stop_col) {

  /*Rows and columns are 1-based because both Excel and R treat arrays
  this way*/
  int row_repeat_count;
  int col_repeat_count;

  rapidxml::xml_node<> *cell;

  // Make local copies that can be modified
  int actual_start_row = start_row;
  int actual_start_col = start_col;

  if (actual_start_row < 1) {
    actual_start_row = 1;
  }
  if (actual_start_col < 1) {
    actual_start_col = 1;
  }
  int nrows = stop_row - actual_start_row + 1;

  std::vector<std::vector<rapidxml::xml_node<> *>> rows;
  rows.reserve((nrows < 1) ? 100
                           : nrows); // Reserve space to avoid reallocations

  static const char *table_table_row = "table:table-row";
  static const char *table_table_cell = "table:table-cell";

  rapidxml::xml_node<> *row = sheet->first_node(table_table_row);

  // If table has no rows or cells, return blank
  if (row == 0 || row->first_node(table_table_cell) == 0) {
    return rows;
  }

  for (int i = 1; i <= stop_row || stop_row < 1;) {
    // i keeps track of what nominal row we are on

    // Check for row repeats
    if (row->first_attribute("table:number-rows-repeated") == nullptr) {
      row_repeat_count = 1;
    } else {
      row_repeat_count = std::atoi(
          row->first_attribute("table:number-rows-repeated")->value());
    }
    // Stop if all repeats done, or if we're at the last requested row
    for (int r_repeat = 0;
         r_repeat < row_repeat_count && (stop_row < 1 || i <= stop_row);
         r_repeat++) {

      // Check size of container and resize if needed
      int required_size = i - actual_start_row + 1;
      if ((int)rows.size() < required_size) {
        rows.resize(std::max((int)rows.size() * 2, required_size));
      }
      // If this row is blank (i.e. it contains only one or no children, which
      // have no contents)
      if (row->first_node()->next_sibling() == 0 &&
          row->first_node()->first_node() == 0) {
        // Look ahead. If this is the last row, stop, otherwise add a blank row
        if (row->next_sibling() == 0) {
          break;
        }
        // Otherwise leave the row blank

        // if row is not blank, and in range deal with cells
      } else if (i >= actual_start_row) {
        unsigned int last_non_blank = 0;
        cell = row->first_node();
        for (int j = 1; j <= stop_col || stop_col < 1;) {
          // find first cell or covered cell
          static const char *table_covered_table_cell =
              "table:covered-table-cell";

          while (cell != 0) {
            const char *cell_name = cell->name();
            if (strcmp(cell_name, table_table_cell) == 0 ||
                strcmp(cell_name, table_covered_table_cell) == 0) {
              break;
            } else {
              cell = cell->next_sibling();
            }
          }
          // Check for column repeats
          if (cell->first_attribute("table:number-columns-repeated")) {
            col_repeat_count =
                std::atoi(cell->first_attribute("table:number-columns-repeated")
                              ->value());
          } else {
            col_repeat_count = 1;
          }

          // Stop if all column repeats done, or if we're at the last requested
          // row
          for (int c_repeat = 0;
               c_repeat < col_repeat_count && (stop_col < 1 || j <= stop_col);
               c_repeat++) {
            bool is_blank = true;
            // If this cell is blank (i.e. contains no children)
            if (cell->first_node() == 0) {
              // Look ahead. If this is the last column, stop.
              if (cell->next_sibling() == 0) {
                break;
              }
            } else {
              // Otherwise mark that cell is not blank
              is_blank = false;
            }
            // If we're in range add pointer to the array
            if (stop_col < 1 || j >= actual_start_col) {
              auto &current_row = rows[i - actual_start_row];
              current_row.push_back(cell);
              if (!is_blank) {
                last_non_blank = current_row.size();
              }
            }
            j++;
          }
          cell = cell->next_sibling();
          // If that was the last cell, stop.
          if (cell == 0) {
            break;
          }
        }
        // Remove trailing blank cells
        rows[i - actual_start_row].resize(last_non_blank);
      }
      i++;
    }
    row = row->next_sibling(table_table_row);
    // If that was the last row, stop.
    if (row == 0) {
      break;
    }
  }
  // Remove trailing empty elements
  unsigned int rowsize = 0;
  for (unsigned int i = 0; i < rows.size(); i++) {
    if (rows[i].size() > 0) {
      rowsize = i;
    }
  }
  rows.resize(rowsize + 1);
  return rows;
}

// read cell_values (an R character vector) out of the rootNode of the XML
// document
cpp11::strings read_cell_values_(rapidxml::xml_node<> *rootNode,
                                 const int start_row, const int stop_row,
                                 const int start_col, const int stop_col,
                                 const int sheet_index,
                                 const bool formula_as_formula) {
  unsigned int out_width = 0;
  unsigned int out_length;
  for (int i = 1; i < sheet_index; i++) {
    rootNode = rootNode->next_sibling("table:table");
  }
  std::vector<std::vector<rapidxml::xml_node<> *>> contents;
  contents = find_rows(rootNode, start_row, stop_row, start_col, stop_col);
  // Get dimensions of output
  out_length = contents.size();
  for (unsigned int i = 0; i < contents.size(); i++) {
    if (contents[i].size() > out_width) {
      out_width = contents[i].size();
    }
  }
  // If there is no content
  if (out_width * out_length == 0) {
    cpp11::writable::strings cell_values(2);
    cell_values[0] = "0";
    cell_values[1] = "0";
    return cell_values;
  }
  cpp11::writable::strings cell_values(out_width * out_length + 2);
  cell_values[0] = std::to_string(out_width);
  cell_values[1] = std::to_string(out_length);

  int t = 2;
  static const SEXP empty_string = Rf_mkCharCE("", CE_UTF8);

  for (unsigned int i = 0; i < contents.size(); i++) {
    const auto &row = contents[i];
    for (unsigned int j = 0; j < row.size(); j++) {
      if (row[j] != 0) {
        std::string cell_content =
            parse_single_cell(row[j], formula_as_formula, true);
        cell_values[t] = Rf_mkCharCE(cell_content.c_str(), CE_UTF8);
      } else {
        cell_values[t] = NA_STRING;
      }
      t++;
    }
    // Pad rows to even width more efficiently
    unsigned int row_width = row.size();
    if (row_width < out_width) {
      for (unsigned int j = row_width; j < out_width; j++) {
        cell_values[t] = empty_string;
        t++;
      }
    }
  }
  return cell_values;
}
