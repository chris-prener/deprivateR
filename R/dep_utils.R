
dep_percent_rank <- function(x){
  out <- (rank(x, ties.method = "min", na.last = "keep") - 1)/(sum(!is.na(x)) -1)
  return(out)
}
