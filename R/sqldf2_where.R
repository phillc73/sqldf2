library(dplyr)
library(microbenchmark)
#library(data.table)
library(sqldf)
library(Rcpp)
library(stringi)
library(compiler)
library(MonetDBLite)
library(DBI)
library(data.table)

# Load some data
data(mtcars)

# MonetDBList table initialisation
con <- dbConnect(MonetDBLite::MonetDBLite())
dbWriteTable(con, "mtcars", mtcars, overwrite = TRUE)

# Inline Rcpp function for comparison
cppFunction('List select_out(List x, CharacterVector y) {
  return x[y];
}')

mtcars_data_table <- data.table(mtcars)

# Benchmark the test function
microbenchmark(times=500,
               sqldf2_where_cpp_out <- sqldf2_where_cpp("SELECT mpg, cyl FROM mtcars WHERE disp >= 200"),
               sqldf2_where_compiled_out <- sqldf2_where_compiled("SELECT mpg, cyl FROM mtcars WHERE disp >= 200"),
               sqldf2_where_stringi_out <- sqldf2_where("SELECT mpg, cyl FROM mtcars WHERE disp >= 200"),
               sqldf_where_out <- sqldf("SELECT mpg, cyl FROM mtcars WHERE disp >= 200"),
               dplyr_out <- mtcars %>%
                 dplyr::filter(disp >= 200) %>%
                 dplyr::select(mpg,cyl),
               monetdblite_where_out <- dbGetQuery(con, "SELECT mpg, cyl FROM mtcars WHERE disp >= 200"),
               data_table_out <- mtcars_data_table[disp >= 200, c("mpg", "cyl"),]
)



sqldf2_where <- function(query){
  querySub <- stri_replace_all_fixed(query,
                                     c("SELECT", "FROM", "WHERE"), c("SELECT:", ":FROM:", ":WHERE:"), vectorize_all=FALSE)
  querySplit <- stri_split_fixed(querySub, ":", omit_empty=TRUE)
  querySplit <- trimws(unlist(querySplit))

  select_cols <- stri_split_fixed(querySplit[2], ",")[[1]]
  select_cols <- trimws(select_cols)

  where_calc <- stri_split_fixed(querySplit[6], ",")[[1]]
  where_calc <- trimws(where_calc)

  queryDf <- eval(parse(text = querySplit[4]))

  queryDf_where <- paste(querySplit[4],"$",where_calc, sep="")

  queryDf_where <- eval(parse(text = queryDf_where))

  where_df <- as.data.frame(queryDf[queryDf_where,])

  as.data.frame(where_df[c(select_cols)])

}

sqldf2_where_compiled <- cmpfun(sqldf2_where)

sqldf2_where_cpp <- function(query){
  querySub <- stri_replace_all_fixed(query,
                                     c("SELECT", "FROM", "WHERE"), c("SELECT:", ":FROM:", ":WHERE:"), vectorize_all=FALSE)
  querySplit <- stri_split_fixed(querySub, ":", omit_empty=TRUE)
  querySplit <- trimws(unlist(querySplit))

  select_cols <- stri_split_fixed(querySplit[2], ",")[[1]]
  select_cols <- trimws(select_cols)

  where_calc <- stri_split_fixed(querySplit[6], ",")[[1]]
  where_calc <- trimws(where_calc)

  queryDf <- eval(parse(text = querySplit[4]))

  queryDf_where <- paste(querySplit[4],"$",where_calc, sep="")

  queryDf_where <- eval(parse(text = queryDf_where))

  where_df <- as.data.frame(queryDf[queryDf_where,])

  #as.data.frame(where_df[c(select_cols)])

  # Run Rcpp function
  select_out(where_df, c(select_cols))

}
