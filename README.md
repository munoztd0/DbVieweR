# Database Management System

About this Shiny app:

This Shiny app simulates a database management system featuring functions like login/logout, save/create/delete tables, add/rename columns, etc.

Features of this app:

1. Back-end database: A SQLite database that stores dummy data. 
2. Authorization: Credit to the package [`shinyauthr`](https://github.com/paulc91/shinyauthr) which provides module functions that can be used to add an authentication layer to shiny apps.  
3. Highlights of the functions:
    - Save tables: save a store sales summary table to the database.
    - Update existing tables: rename tables, rename/add columns of tables.
    - Create new tables: table and column names can be customized. Provide 4 types of column to be added - integer, float, varchar(255), and boolean. 
    - Create entries to tables:  prompt columns contained in the selected table together with their types. 
    - Delete tables: the action is only accessible to specific authorization.
4. Robustness: Defense mechanism that prevents duplicates, invalid expressions, and conflicts with SQL keywords are set for all the input table and colunm names to ensure the smooth execution of SQL queries. Once errors are detected, prompt messages will show up suggesting possible failure reasons. 


[Check the Demo](https://munoztd0.shinyapps.io/DbVieweR)
