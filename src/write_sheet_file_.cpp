#include "write_sheet_file_.h"

void cell_out (const cpp11::r_string& value_type, const cpp11::r_string& value, std::ofstream& xml_file) {
    const char* value_type_c = Rf_translateCharUTF8(value_type);
    const char* value_c = Rf_translateCharUTF8(value);
    xml_file << "<table:table-cell office:value-type=\"";
    xml_file << value_type_c;
    if (strcmp(value_type_c, "string") != 0) {
        xml_file << "\" office:value=\"";
        xml_file << value_c;
    }
    xml_file << "\" table:style-name=\"ce1\"><text:p>";
    xml_file << value_c;
    xml_file << "</text:p></table:table-cell>\n";
}

void pad_rows (const bool& padding, const int& cols, const int& cmax, std::ofstream& xml_file) {
    if (cols < cmax && padding) {
        xml_file << "<table:table-cell table:number-columns-repeated=\"";
        xml_file << cmax - cols;
        xml_file << "\"/>\n";
    }
}

cpp11::strings dimnames(const cpp11::data_frame& x, bool cols) {
    // Is there a better way?
    cpp11::function dimnames_rfun = cpp11::package("readODS")[".get_sanitized_dimnames"];
    return cpp11::writable::strings(static_cast<SEXP>(dimnames_rfun(x, cols)));
}

cpp11::list_of<cpp11::strings> sanitize(const cpp11::data_frame& x, const cpp11::strings column_types) {
    cpp11::function sanitize_rfun = cpp11::package("readODS")[".sanitize_df"];
    return cpp11::writable::list_of<cpp11::strings>(static_cast<SEXP>(sanitize_rfun(x, column_types)));
}

cpp11::strings get_column_types(const cpp11::data_frame& x) {
    cpp11::function get_column_types_rfun = cpp11::package("readODS")[".get_column_types"];
    return cpp11::writable::strings(static_cast<SEXP>(get_column_types_rfun(x)));
}

std::string escape_xml(const std::string& input) {
    cpp11::sexp input_sexp = cpp11::as_sexp(input);
    cpp11::function escape_xml_rfun = cpp11::package("readODS")[".escape_xml"];
    return cpp11::as_cpp<std::string>(escape_xml_rfun(input_sexp));
}

void write_empty(std::ofstream& xml_file, const std::string& escaped_sheet_name) {
    xml_file << "<table:table table:name=\"";
    xml_file << escaped_sheet_name;
    xml_file << "\" table:style-name=\"ta1\">";
    xml_file << "</table:table>";
}

void write_df(const cpp11::data_frame& x, const std::string& sheet_name, const bool row_names, const bool col_names,
              const bool na_as_string, const bool padding, std::ofstream& xml_file) {
    std::string escaped_sheet_name = escape_xml(sheet_name);
    if (x.ncol() == 0 || (x.nrow() == 0 && !col_names && x.ncol() != 0)) {
        write_empty(xml_file, escaped_sheet_name);
        return;
    }
    cpp11::strings column_types = get_column_types(x);
    cpp11::strings rownames_x, colnames_x;
    cpp11::list_of<cpp11::strings> x_list = sanitize(x, column_types);
    if (row_names) {
        rownames_x = dimnames(x, false);
    }
    if (col_names) {
        colnames_x = dimnames(x, true);
    }
    int rows = col_names ? x_list[0].size() + 1 : x_list[0].size();
    int cols = row_names ? column_types.size() + 1 : column_types.size();
    int cmax = column_types.size() > 1024 ? 16384 : 1024;
    // gen_sheet_tag
    xml_file << "\n<table:table table:name=\"";
    xml_file << escaped_sheet_name;
    xml_file << "\" table:style-name=\"ta1\">\n";
    // column
    xml_file << "<table:table-column table:style-name=\"co1\" table:number-columns-repeated=\"";
    padding ? xml_file << cmax : xml_file << cols;
    xml_file << "\" table:default-cell-style-name=\"ce1\"/>\n";
    // add_data
    if (col_names) {
        xml_file << "<table:table-row table:style-name=\"ro1\">";
        if (row_names) {
            cell_out("string", "", xml_file);
        }
        for (int j = 0; j < colnames_x.size(); j++) {
            cell_out("string", colnames_x[j], xml_file);
        }
        pad_rows(padding, cols, cmax, xml_file);
        xml_file << "</table:table-row>\n";
    }
    for (int i = 0; i < x_list[0].size(); i++) {
        xml_file << "<table:table-row table:style-name=\"ro1\">\n";
        if (row_names) {
            cell_out("string", rownames_x[i], xml_file);
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
            cell_out("string", "NA", xml_file);
        }
        pad_rows(padding, cols, cmax, xml_file);
        xml_file << "</table:table-row>\n";
    }
    // pad_columns
    if (rows < 1048576 && padding) {
        xml_file << "<table:table-row table:style-name=\"ro1\" table:number-rows-repeated=\"";
        xml_file << 1048576 - rows;
        xml_file << "\">\n";
        xml_file << "<table:table-cell table:number-columns-repeated=\"";
        xml_file << cmax;
        xml_file << "\"/>";
        xml_file << "</table:table-row>\n";
    }
    xml_file << "</table:table>\n";
}

[[cpp11::register]]
cpp11::r_string write_sheet_file_(const std::string& filename,
                                  const cpp11::data_frame& x,
                                  const std::string& sheet_name,
                                  const bool row_names,
                                  const bool col_names,
                                  const bool na_as_string,
                                  const bool padding,
                                  const std::string& header,
                                  const std::string& footer) {
    std::ofstream xml_file(filename);
    xml_file << header;
    write_df(x, sheet_name, row_names, col_names, na_as_string, padding, xml_file);
    xml_file << footer;
    xml_file << "\n";
    xml_file.close();
    return filename;
}

[[cpp11::register]]
cpp11::r_string write_sheet_file_list_(const std::string& filename,
                                       const cpp11::list_of<cpp11::data_frame>& x,
                                       const std::string& sheet_name, // wont use; just for maintain the same interface
                                       const bool row_names,
                                       const bool col_names,
                                       const bool na_as_string,
                                       const bool padding,
                                       const std::string& header,
                                       const std::string& footer) {
    std::ofstream xml_file(filename);
    xml_file << header;
    cpp11::strings sheet_names = x.names();
    for (int i = 0; i < sheet_names.size(); i++) {
        cpp11::data_frame current_df = x[i];
        cpp11::r_string current_sheet_name = sheet_names[i];
        write_df(current_df, current_sheet_name, row_names, col_names, na_as_string, padding, xml_file);
    }
    xml_file << footer;
    xml_file << "\n";
    xml_file.close();
    return filename;
}
