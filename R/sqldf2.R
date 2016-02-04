# This is all testing and not likely to work!
# Load some libraries
library(dplyr)
library(microbenchmark)
#library(data.table)
library(sqldf)
library(Rcpp)

# Load some data
data(mtcars)
#mtcars <- as.data.table(mtcars)

# Test query
#query <- "SELECT mpg, cyl FROM mtcars"

# Inline Rcpp function for comparison
cppFunction('List select_out(List x, CharacterVector y) {
  return x[y];
}')


# Benchmark the test function
microbenchmark(times=50,
               sqldf2Out <- sqldf2("SELECT mpg, cyl FROM mtcars"),
               sqldfOut <- sqldf("SELECT mpg, cyl FROM mtcars"),
               dplyrout <- dplyr::select(mtcars, mpg, cyl),
               baseout <- mtcars[c("mpg", "cyl")],
               rcppout <- select_out(mtcars, c("mpg", "cyl"))
               )

################################################################################################

# The function
sqldf2 <- function(query){
  querySplit <- scan(text =   # use scan to separate after insertion of commas
                       gsub("SELECT", "SELECT:",   # put commas in after "SELECT"
                            gsub("FROM", ":FROM",
                                 gsub("FROM", "FROM:",    query))) ,  # add commas before "FROM"
                     what="", sep=":")  # tell scan this character argument and separators are ","

  querySplit <- trimws(querySplit)

  cols <- strsplit(querySplit[2], ",")[[1]]
  cols <- trimws(cols)

  queryDf <- eval(parse(text = querySplit[4]))

  # Run Rcpp function
  # select_out(queryDf, c(cols))
  # Base R out
  as.data.frame(queryDf[c(cols)])
  # dplyr out
  #as.data.frame(dplyr::select(queryDf, cols))

}



