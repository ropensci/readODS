# readODS 1.9.0

* Added a `NEWS.md` file to track changes to the package.
* Rewrote all reading functions in C++ for significant speed increase

## list_ods_sheets

* Added `include_external_data` as an argument (`FALSE` by default). This hides stored data from external sources not normally accessible to the user.

## read_ods

* Changed behaviour when only one row is read. The row now correctly appears as the top row in the dataframe, and a warning is given if column headers are requested that this would cause the output to be empty (**Note:** in this case column names are not assigned)
* Changed behaviour when only one column is read. Previously gave an error. If row names are requested, gives a warning that this would cause the output to be empty, and does not assign names.
* Sheets are now accepted as part of the `range` argument, e.g. `Range = "Sheet2!A2:B7"`. If this and the `sheets` argument are given, this is preferred.
* Merged cells now have their value places in the top-left cell. All other cells that would be covered by the merge are filled with `NA`.

## read_fods

* Reading (but not writing) flat ODS files is now supported using the functions `read_fods()`, `list_fods_sheets()`, `get_num_sheets_in_fods()`. These work the same way as their analogue ODS functions. The extension does not need to be `fods`, however they do need to conform to the OO specification.

## write_ods

* Attempted to fix writing non UTF-8 strings to files. This still has some issues in versions of R \< 4.2, however it should now correctly write most text within your current locale. See [readme](README.md) for more details.