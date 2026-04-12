#include "write_sheet_file_.h"

void cell_out(const cpp4r::r_string &value_type, const cpp4r::r_string &value,
              std::ofstream &xml_file) {
  const char *value_type_c = Rf_translateCharUTF8(value_type);
  const char *value_c = Rf_translateCharUTF8(value);

  // Pre-allocate string buffer to reduce allocations
  std::string cell_xml;
  cell_xml.reserve(256); // Reserve space for typical cell content

  cell_xml += "<table:table-cell office:value-type=\"";
  cell_xml += value_type_c;

  // Cache string comparison for performance
  static const char *string_type = "string";
  if (strcmp(value_type_c, string_type) != 0) {
    cell_xml += "\" office:value=\"";
    cell_xml += value_c;
  }
  cell_xml += "\" table:style-name=\"ce1\"><text:p>";
  cell_xml += value_c;
  cell_xml += "</text:p></table:table-cell>\n";

  // Single write operation for better performance
  xml_file << cell_xml;
}

void pad_rows(const bool &padding, const int &cols, const int &cmax,
              std::ofstream &xml_file) {
  if (cols < cmax && padding) {
    // More efficient single write with pre-constructed string
    std::string pad_xml = "<table:table-cell table:number-columns-repeated=\"" +
                          std::to_string(cmax - cols) + "\"/>\n";
    xml_file << pad_xml;
  }
}

cpp4r::strings dimnames(const cpp4r::data_frame &x, bool cols) {
  // Is there a better way?
  cpp4r::function dimnames_rfun =
      cpp4r::package("readODS")[".get_sanitized_dimnames"];
  return cpp4r::writable::strings(static_cast<SEXP>(dimnames_rfun(x, cols)));
}

cpp4r::list_of<cpp4r::strings> sanitize(const cpp4r::data_frame &x,
                                        const cpp4r::strings column_types) {
  cpp4r::function sanitize_rfun = cpp4r::package("readODS")[".sanitize_df"];
  return cpp4r::writable::list_of<cpp4r::strings>(
      static_cast<SEXP>(sanitize_rfun(x, column_types)));
}

cpp4r::strings get_column_types(const cpp4r::data_frame &x) {
  cpp4r::function get_column_types_rfun =
      cpp4r::package("readODS")[".get_column_types"];
  return cpp4r::writable::strings(static_cast<SEXP>(get_column_types_rfun(x)));
}

std::string escape_xml(const std::string &input) {
  cpp4r::sexp input_sexp = cpp4r::as_sexp(input);
  cpp4r::function escape_xml_rfun = cpp4r::package("readODS")[".escape_xml"];
  return cpp4r::as_cpp<std::string>(escape_xml_rfun(input_sexp));
}

void write_empty(std::ofstream &xml_file,
                 const std::string &escaped_sheet_name) {
  // Single write operation for better performance
  std::string empty_table = "<table:table table:name=\"" + escaped_sheet_name +
                            "\" table:style-name=\"ta1\"></table:table>";
  xml_file << empty_table;
}

void write_df(const cpp4r::data_frame &x, const std::string &sheet_name,
              const bool row_names, const bool col_names,
              const bool na_as_string, const bool padding,
              std::ofstream &xml_file) {
  std::string escaped_sheet_name = escape_xml(sheet_name);
  if (x.ncol() == 0 || (x.nrow() == 0 && !col_names && x.ncol() != 0)) {
    write_empty(xml_file, escaped_sheet_name);
    return;
  }
  cpp4r::strings column_types = get_column_types(x);
  cpp4r::strings rownames_x, colnames_x;
  cpp4r::list_of<cpp4r::strings> x_list = sanitize(x, column_types);
  if (row_names) {
    rownames_x = dimnames(x, false);
  }
  if (col_names) {
    colnames_x = dimnames(x, true);
  }
  int rows = col_names ? x_list[0].size() + 1 : x_list[0].size();
  int cols = row_names ? column_types.size() + 1 : column_types.size();
  int cmax = column_types.size() > 1024 ? 16384 : 1024;
  // gen_sheet_tag - more efficient single write
  std::string header_xml = "\n<table:table table:name=\"" + escaped_sheet_name +
                           "\" table:style-name=\"ta1\">\n";
  header_xml += "<table:table-column table:style-name=\"co1\" "
                "table:number-columns-repeated=\"";
  header_xml += std::to_string(padding ? cmax : cols);
  header_xml += "\" table:default-cell-style-name=\"ce1\"/>\n";
  xml_file << header_xml;
  // add_data - column headers
  if (col_names) {
    xml_file << "<table:table-row table:style-name=\"ro1\">";
    if (row_names) {
      cell_out("string", "", xml_file);
    }
    // Cache string literal for performance
    static const cpp4r::r_string string_type_r("string");
    for (int j = 0; j < colnames_x.size(); j++) {
      cell_out(string_type_r, colnames_x[j], xml_file);
    }
    pad_rows(padding, cols, cmax, xml_file);
    xml_file << "</table:table-row>\n";
  }
  for (int i = 0; i < x_list[0].size(); i++) {
    xml_file << "<table:table-row table:style-name=\"ro1\">\n";
    if (row_names) {
      static const cpp4r::r_string string_type_r("string");
      cell_out(string_type_r, rownames_x[i], xml_file);
    }
    for (int j = 0; j < column_types.size(); j++) {
      if (x_list[j][i] != NA_STRING) {
        cell_out(column_types[j], x_list[j][i], xml_file);
        continue;
      }
      if (!na_as_string) {
        xml_file << "<table:table-cell/>\n";
        continue;
      }
      static const cpp4r::r_string string_type_r("string");
      static const cpp4r::r_string na_value("NA");
      cell_out(string_type_r, na_value, xml_file);
    }
    pad_rows(padding, cols, cmax, xml_file);
    xml_file << "</table:table-row>\n";
  }
  // pad_columns - more efficient single write
  if (rows < 1048576 && padding) {
    std::string padding_xml = "<table:table-row table:style-name=\"ro1\" "
                              "table:number-rows-repeated=\"" +
                              std::to_string(1048576 - rows) + "\">\n";
    padding_xml += "<table:table-cell table:number-columns-repeated=\"" +
                   std::to_string(cmax) + "\"/></table:table-row>\n";
    xml_file << padding_xml;
  }
  xml_file << "</table:table>\n";
}

[[cpp4r::register]]
cpp4r::r_string
write_sheet_file_(const std::string &filename, const cpp4r::data_frame &x,
                  const std::string &sheet_name, const bool row_names,
                  const bool col_names, const bool na_as_string,
                  const bool padding, const std::string &header,
                  const std::string &footer) {
  std::ofstream xml_file(filename, std::ios::out | std::ios::trunc);
  if (!xml_file) {
    throw std::runtime_error("Cannot open file for writing: " + filename);
  }

  xml_file << header;
  write_df(x, sheet_name, row_names, col_names, na_as_string, padding,
           xml_file);
  xml_file << footer << "\n";

  return filename;
}

[[cpp4r::register]]
cpp4r::r_string write_sheet_file_list_(
    const std::string &filename, const cpp4r::list_of<cpp4r::data_frame> &x,
    const std::string
        &sheet_name, // wont use; just for maintain the same interface
    const bool row_names, const bool col_names, const bool na_as_string,
    const bool padding, const std::string &header, const std::string &footer) {
  std::ofstream xml_file(filename, std::ios::out | std::ios::trunc);
  if (!xml_file) {
    throw std::runtime_error("Cannot open file for writing: " + filename);
  }

  xml_file << header;
  cpp4r::strings sheet_names = x.names();
  for (int i = 0; i < sheet_names.size(); i++) {
    const cpp4r::data_frame &current_df = x[i]; // Use const reference
    const cpp4r::r_string &current_sheet_name =
        sheet_names[i]; // Use const reference
    write_df(current_df, current_sheet_name, row_names, col_names, na_as_string,
             padding, xml_file);
  }
  xml_file << footer << "\n";

  return filename;
}
