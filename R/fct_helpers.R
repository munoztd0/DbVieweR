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
    #browser()
  } else {
    db <- dbConnect(SQLite(), 'test_db_file')
    #browser()
    dbWriteTable(db, "iris", iris, overwrite =T)
    dbWriteTable(db, "mtcars", mtcars, overwrite =T)
    dbWriteTable(db, "starwars", starwars, overwrite =T)
  }
  
  return(db)
}