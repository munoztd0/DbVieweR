#' helpers 
#'
#' @description A connection function
#' 
#' 
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbWriteTable
#'
#' @return connection
#'
#' @noRd


create_conn <- function(){
  

  if(file.exists("test_db_file")) {
    db <- dbConnect(SQLite(), 'test_db_file')
  } else {
    db <- dbConnect(SQLite(), 'test_db_file')
    dbWriteTable(db, "iris", iris, overwrite =T)
    dbWriteTable(db, "mtcars", mtcars, overwrite =T)
    dbWriteTable(db, "starwars2", DbVieweR::starwars2, overwrite =T)
  }
  
  return(db)
}






#' Style My Workbook
#'
#'
#' @description
#' Apply a style to a given workbook
#'
#' @param wb workbook to apply the style
#' @param dt dataframe to put in the wb
#' @param SheetName name of the spreadsheet
#' @param alt_rows activate or not the color alternation
#' 
#'
#' @examples
#' \dontrun{
#' 
#' options(openxlsx.datetimeFormat = "yyyy-mm-dd")
#' wb <- openxlsx::createWorkbook()
#' style_my_workbook(wb,dt, "TestStyle", TRUE)
#' openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
#' }
#'
#' @return a workbook
#'
#' @importFrom openxlsx addWorksheet createStyle writeData addStyle setColWidths setRowHeights
#'

style_my_workbook <-function (wb, dt, SheetName, alt_rows = TRUE) 
{
    if (alt_rows == TRUE) {
        c_alt_row = "#F0F0F0"
    }
    else {
        c_alt_row = "white"
    }
    addWorksheet(wb, SheetName)
    cellStyle_even <- createStyle(numFmt = "##,##0.00", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = c_alt_row)
    cellStyle_even_DATE <- createStyle(numFmt = "yyyy/mm/dd", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = c_alt_row)
    cellStyle_odd <- createStyle(numFmt = "#,##0.00", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = "white")
    cellStyle_odd_DATE <- createStyle(numFmt = "yyyy/mm/dd", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = "white")
    headerStyle <- createStyle(fontSize = 14, fontColour = "white", 
        halign = "center", valign = "center", borderColour = "white", 
        fgFill = "#1D1861", textDecoration = "bold")
    width_vec <- apply(dt, 2, function(x) max(nchar(as.character(x)) + 
        9, na.rm = TRUE))

    width_vec_header <- nchar(colnames(dt)) + 15
    max_vec_header <- pmax(width_vec, width_vec_header)
    nr <- nrow(dt) + 1
    nc <- ncol(dt)
    rowsn <- 2:nr
    even <- list()
    odd <- list()

    for (i in rowsn) {
        if (i%%2 == 1) {
            odd <- append(odd, i)
        }
        else {
            even <- append(even, i)
        }
    }

    odd <- unlist(odd)
    even <- unlist(even)
   
    writeData(wb, sheet = SheetName, x = dt, withFilter = TRUE)
    addStyle(wb, sheet = SheetName, headerStyle, rows = 1:2, 
        cols = 1:ncol(dt), gridExpand = TRUE)

    x <- sapply(dt, lubridate::is.timepoint)

    if (sum(x) == 0) {

        addStyle(wb, sheet = SheetName, cellStyle_odd, rows = odd, 
            cols = 1:nc, gridExpand = TRUE)
        addStyle(wb, sheet = SheetName, cellStyle_even, rows = even, 
            cols = 1:nc, gridExpand = TRUE)
    }
    else {

        addStyle(wb, sheet = SheetName, cellStyle_odd, rows = odd, 
            cols = 1:nc, gridExpand = TRUE)
        addStyle(wb, sheet = SheetName, cellStyle_even, rows = even, 
            cols = 1:nc, gridExpand = TRUE)


        for (i in colnames(dt[x]) ) {
            idx <- grep(i, colnames(dt)) 
            addStyle(wb, sheet = SheetName, cellStyle_even_DATE, 
                rows = even, cols = idx, gridExpand = TRUE)
            addStyle(wb, sheet = SheetName, cellStyle_odd_DATE, rows = odd, 
            cols = idx, gridExpand = TRUE)
        }
    }
    setColWidths(wb, sheet = SheetName, cols = 1:nc, widths = max_vec_header)
    setRowHeights(wb, sheet = SheetName, 1, 40)
    return(wb)
}