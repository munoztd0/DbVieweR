#' add_sessionid_to_db 
#'
#' @description This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  
#'
#' @return This function saves to your database.
#'
#' @noRd
#' 
#' 

add_sessionid_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}


#' add_sessionid_to_db 
#'
#' @description Check user who has not visited the app for this many days. Check globals for number of days expiry
#'
#' @return users who have not visited the app for this many days
#'
#' @noRd
#' 
#' 
# 
get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}


#' Init logout module
#'
#' @description call the logout module with reactive trigger to hide/show
#'
#' @return logout module
#'
#' @noRd
#' 
#' 
# 

logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
    )



#' disconnect prompt
#'
#' @description nice disconnected promt
#' #'
#' @return sever disconnected prompt
#'
#' @noRd
#' 
#' 
# 

disconnected <- sever::sever_default(
	title = "Disconnected",
	subtitle = "Your session ended",
	button = "Reconnect"
	)


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


