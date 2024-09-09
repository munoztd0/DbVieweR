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
    print(getwd())
    db <- dbConnect(SQLite(), 'test_db_file')
    print("Database already exists")
  } else {
    print(getwd())
    db <- dbConnect(SQLite(), 'test_db_file')
    print("Creating test database")
    dbWriteTable(db, "iris", iris, overwrite =T)
    dbWriteTable(db, "mtcars", mtcars, overwrite =T)
    dbWriteTable(db, "starwars2", starwars2, overwrite =T)
  }
  
  return(db)
}




