# function to rbind two data.frames with different column names/orders
merge_columns<-function(x, y){
  rbind(
    data.frame(
      c(
        x,
        sapply(
          setdiff(names(y), names(x)),
          function(a) NA
        )
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      c(
        y,
        sapply(
          setdiff(names(x), names(y)),
          function(a) NA
        )
      ),
      stringsAsFactors = FALSE
    )
  )
}
