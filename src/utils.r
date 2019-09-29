calc_time <-
  function(x) {
    if (attr(x,"units") == "mins") {
      r <- as.vector(x) * 60
    } else {
      r <- as.vector(x)
    }
    r
  }

replace_inf <- function(df) {
  do.call(data.frame,
          lapply(df, function(j) {
            replace(j, is.infinite(j), NA)
          })
  )
}

holdout_ <- function(x, beta) {
  len <- NROW(x)
  train <- x[ seq_len(beta * len), ]
  test <-  x[-seq_len(beta * len), ]
  
  list(train=train, test=test)
}
