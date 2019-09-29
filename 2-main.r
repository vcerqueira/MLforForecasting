load("data/tsl_uni_90_mix.rdata")

source("src/workflows.r")
source("src/utils.r")
source("src/analysis.r")
source("src/statistical-methods.r")
source("src/regression-models.r")

library(tsensembler)
library(forecast)
library(Metrics)

ts_len <- 1000

# forecasting horizon
h <- 18

#online behavior
update_every_ <- 1

ngroups <- ts_len %/% update_every_
max_len <- ngroups * update_every_
len <- length(ts_list)

IDS <- 1:90
EXPRES <- vector("list",len)
for (i in IDS) {
  cat("TIME SERIES",i,"/",len,"\n\n\n")
  x <- ts_list[[i]]
  x <- head(x, max_len)
  
  r_perf <- workflow_comparison(x = x, 
                                h = h, 
                                update_every_ = update_every_)
  
  EXPRES[[i]] <- r_perf
  save(EXPRES, file = "EXPRES.rdata")
}

# save(EXPRES, file = paste0("EXPRES_",IDS[1],"_",
#                            IDS[length(IDS)],".rdata"))