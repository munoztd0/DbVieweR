# Description: This file contains the global variables and functions that are used in the shiny app.

# Set the language for the app and add the labels for the login page
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



# load data
data(starwars2, envir=environment())



# remove on inactivity
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
