source("./Functions.R")

install_packages_if_needed(c("utils"))

################################################################################
#Part1, Q1: Import and Describe the Data

#Import the csv files
df1 <- data.frame(utils::read.delim(file = "nswre74_control.csv",
                                    sep = ","))
df2 <- data.frame(utils::read.delim(file = "nswre74_treated.csv",
                                    sep = ","))

#Combine them
df<- rbind(df1, df2)

#Tally the number of treated vs control
dplyr::tally(dplyr::group_by(df, treat))

#get mean for each variables, in treated vs control
dplyr::group_by(df, treat)%>% dplyr::summarise_all(.funs=list(mean))



