


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# Please use the usernames and passwords provided below to test this database management platform. \n
#   
#   We have created two kinds of accounts: manager and admin The admin account allows one to alter tables, columns, and records, and delete tables. The manager account allows one to do everything above except for deleting tables.
# 
# 
# username	password	permissions
# user1	pass1	manager
# user2	pass2	admin
shinymanager::set_labels(
   language = "en",
  "Please authenticate" = "Please authenticate",
  "Username:" = "Username: user1",
  "Password:" = "Password: pass1",
  "Login" = "Login"
)

# data.frame with credentials info
credentials <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "manager"),
  stringsAsFactors = FALSE,
  level = c(2, 0)
)


tracker <-  shinymetrics::Shinymetrics$new(token = "CQDPXTAORH7X7AP5DTXMLTL3GI")

tracker <- tracker$track_recommended()

# load data
data(starwars2, envir=environment())