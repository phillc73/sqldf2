# This is all testing and not likely to work!
# Load some libraries
library(MonetDBLite)
library(DBI)
library(dplyr)
library(microbenchmark)
#library(data.table)
library(sqldf)
library(Rcpp)
library(compiler)
library(stringi)

# Load some data
data(mtcars)
#mtcars <- as.data.table(mtcars)

# Test query
#query <- "SELECT mpg, cyl FROM mtcars"

# Inline Rcpp function for comparison
cppFunction('List select_out(List x, CharacterVector y) {
  return x[y];
}')

# MonetDBList table initialisation
con <- dbConnect(MonetDBLite::MonetDBLite())
dbWriteTable(con, "mtcars", mtcars)

# Benchmark the test function
microbenchmark(times=500,
               sqldf2Out <- sqldf2("SELECT mpg, cyl FROM mtcars"),
               sqldf2compiledOut <- sqldf2compiled("SELECT mpg, cyl FROM mtcars"),
               sqldf2StringiOut <- sqldf2stringi("SELECT mpg, cyl FROM mtcars"),
               sqldf2StringicompiledOut <- sqldf2stringicompiled("SELECT mpg, cyl FROM mtcars"),
               sqldfOut <- sqldf("SELECT mpg, cyl FROM mtcars"),
               dplyrout <- dplyr::select(mtcars, mpg, cyl),
               baseout <- mtcars[c("mpg", "cyl")],
               rcppout <- select_out(mtcars, c("mpg", "cyl")),
               monetdbliteOut <- dbGetQuery(con, "SELECT mpg, cyl FROM mtcars")
               )

################################################################################################

# The function
sqldf2 <- function(query){
  querySplit <- scan(text =   # use scan to separate after insertion of commas
                       gsub("SELECT", "SELECT:",   # put colon in after "SELECT"
                            gsub("FROM", ":FROM",
                                 gsub("FROM", "FROM:",    query))) ,  # add colon before "FROM"
                     what="", sep=":")  # tell scan this character argument and separators are ":"

  querySplit <- trimws(querySplit)

  cols <- strsplit(querySplit[2], ",")[[1]]
  cols <- trimws(cols)

  queryDf <- eval(parse(text = querySplit[4]))

  # Run Rcpp function
   select_out(queryDf, c(cols))
  # Base R out
  # as.data.frame(queryDf[c(cols)])
  # dplyr out
  #as.data.frame(dplyr::select(queryDf, cols))

}

# Try compiling for speed
sqldf2compiled <- cmpfun(sqldf2)

# With stringi instead of scan and gsub
sqldf2stringi <- function(query){
  querySub <- stri_replace_all_fixed(query,
                                     c("SELECT", "FROM"), c("SELECT:", ":FROM:"), vectorize_all=FALSE)
  querySplit <- stri_split_fixed(querySub, ":", omit_empty=TRUE)
  querySplit <- trimws(unlist(querySplit))

  cols <- stri_split_fixed(querySplit[2], ",")[[1]]
  cols <- trimws(cols)

  queryDf <- eval(parse(text = querySplit[4]))

  # Run Rcpp function
   select_out(queryDf, c(cols))

  # as.data.frame(queryDf[c(cols)])

}

# Try compiling for speed
sqldf2stringicompiled <- cmpfun(sqldf2stringi)
