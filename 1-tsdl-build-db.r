library(tsdl)
library(forecast)

min_len <- 1000
tsdl_list <- tsdl[sapply(tsdl, length) > min_len]
tsdl_list <-
  tsdl_list[!sapply(tsdl_list,
                    function(x) {
                      any(is.na(x))
                    })]

tsdl_list[] <- lapply(tsdl_list, head, 1000)

is_univar <- sapply(tsdl_list, class) == "ts"

tsdl_list <- tsdl_list[is_univar]

load("./data/tseries_vcerq.rdata")

ids2rm <- c(14:21,24,28,35:42,53:61)

ts_list <- ts_list[-ids2rm]

tdiff <- 
  sapply(ts_list,
       function(x) {
         xt <-
           tryCatch(
             as.POSIXct(head(rownames(as.data.frame(x)))),
             error = function(e)
               NA
           )
         
         difftime(xt[2],xt[1], units = "hours")
       })

#lapply(ts_list[is.na(tdiff)], head, 3)
tdiff[is.na(tdiff)] <- 1

FRQs <- tdiff
FRQs[tdiff == 1] <- 24
FRQs[tdiff == .5] <- 48
FRQs[tdiff == 24] <- 365

ts_list <-
  lapply(1:length(ts_list),
       function(i) {
         x <- ts_list[[i]]
         x <- ts(as.vector(x), frequency = FRQs[i])
         x <- head(x, min_len)
         x
       })

ts_list <- c(tsdl_list,ts_list)

#save(ts_list, file = "data/tsl_uni_90_mix.rdata")
