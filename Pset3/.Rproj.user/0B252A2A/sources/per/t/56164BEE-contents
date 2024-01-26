source("./Functions.R")

install_packages_if_needed(c("utils"))

################################################################################
#Part 1: Import and Describe the Data
#Q1

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


################################################################################
#Part 2: Test Balance

#Q1
#Get the list of OPV's
list_OPVs <- as.list(colnames(df))
list_OPVs <- list_OPVs[!list_OPVs %in% c("treat", "re78")]

#Now for each OPV, create a formula and test it
for (x in list_OPVs){
  xname = toString(x)
  fo <- as.formula(paste(xname, "~treat"))
  lm_model <- lm(formula = fo, data = df)
  print(fo)
  print(summary(lm_model)$coefficients)
}

#Q2 a
#Make the list of formulas
list_fo = list()
for (x in list_OPVs){
  xname = toString(x)
  fo <- as.formula(paste(xname, "~treat"))
  list_fo <- append(list_fo, fo)
}
#Now use SUR on it
install_packages_if_needed(c("systemfit"))
sur_fit <- systemfit::systemfit(formula = list_fo, data=df, method="SUR")
print(summary(sur_fit)$coefficients)

#Q2 b
#Create null_system
null_system = list()
for (x in list_OPVs){
  xname = toString(x)
  fo <- as.formula(paste(xname, "~1"))
  null_system <-append(null_system, fo)
}
null_system
#lmtest
install_packages_if_needed(c("lmtest"))
null_fit <- systemfit::systemfit(null_system, data =df, method = "SUR")
lrtest_obj <- lmtest::lrtest(null_fit,sur_fit)
print(lrtest_obj)
#test "by hand"
#Note: pchisq gives us the prob of chisq Below our value, not above.
1-stats::pchisq(20.0972901715468, 10)

#Q3
#create formula
f <- "treat ~"
for (x in list_OPVs){
  xname <- toString(x)
  f <- paste(f, xname, "+")
}
f <- substr(f, 1, nchar(f)-2)
lm_obj <- lm(formula = f, data = df)
summary(lm_obj)$fstatistic 

#Test "by hand"
1-stats::pf(q=2, df1=10, df2=434)


################################################################################
#Part 3

#Q1
#spec 0
dplyr::group_by(df, treat)%>% dplyr::summarise_at(.vars="re78", .funs=list(mean))
stats::t.test(re78 ~ treat, data=df)

#spec 1
summary(lm(re78 ~ treat, data=df))

#spec 2
summary(lm(re78 ~ treat + nodegree + edu, data=df))

#spec 3
#create formula
f <- "re78 ~"
for (x in list_OPVs){
  xname <- toString(x)
  f <- paste(f, xname, "+")
}
f <- paste(f, "treat")
summary(lm(formula = f, data = df))

#spec 4
#add a variable
mean(df$age)
df <-df %>% 
  dplyr::mutate(.data=df, agextreat = (age-mean(df$age))*treat)
f<- paste(f, "+ agextreat")
summary(lm(formula=f, data=df))

#Q2 d
#test joint hypotheses
install_packages_if_needed(c("car"))

# Test linear hypotheses
h_matrix = c("treat", "agextreat")
car::linearHypothesis(model = lm(formula=f, data=df), hypothesis.matrix=h_matrix)
