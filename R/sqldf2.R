# This is all testing and not likely to work!
# Load some libraries
library(dplyr)
library(microbenchmark)
library(data.table)

# Load some data
data(mtcars)
#mtcars <- as.data.table(mtcars)
fbHistoric <- readRDS("fbHistoricResults.rds")

fbHistoric <- as.data.frame(fbHistoric)

str(fbHistoric)

# Test query
#query <- "SELECT FBR, FBRr FROM fbHistoric"

# Benchmark the test function
microbenchmark(times=50,
               sqldf2Out <- sqldf2("SELECT FBR, FBRr FROM fbHistoric"),
               dplyrout <- dplyr::select(fbHistoric, FBR),
               baseout <- fbHistoric["FBR"]
               )

# The function
sqldf2 <- function(query){

  querySplit <- scan(text =   # use scan to separate after insertion of commas
                       gsub("SELECT", "SELECT:",   # put commas in after "SELECT"
                            gsub("FROM", ":FROM",
                                 gsub("FROM", "FROM:",    query))) ,  # add commas before "FROM"
                     what="", sep=":")  # tell scan this character argument and separators are ","

  querySplit <- trimws(querySplit)

  cols <- querySplit[2]
  queryDf <- eval(parse(text = querySplit[4]))

  as.data.frame(queryDf[c(cols)])
  #as.data.frame(dplyr::select(queryDf, one_of(cols)))

}
