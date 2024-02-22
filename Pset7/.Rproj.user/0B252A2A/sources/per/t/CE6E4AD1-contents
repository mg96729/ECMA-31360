#Abbreviated Pip function
`%>%` <- dplyr::`%>%`


# Function that installs one or more packages if they are not yet installed
install_packages_if_needed <- function(list_of_packages){
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)>0) install.packages(new_packages)
}

# Function to find standard error
std_err <- function(data_frame){
  return(sd(data_frame)/sqrt(length((data_frame))))
}