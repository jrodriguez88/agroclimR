read_INPUT_data <-
function(file) {
    stopifnot(require(readxl))
    sheets <- readxl::excel_sheets(file)
    x <-    lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
    names(x) <- sheets
    x
}
