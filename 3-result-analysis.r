source("src/analysis.r")
source("src/utils.r")
library(tsensembler)

load("EXPRES.rdata")

# Computing mase by testing point
## h=18
resultsMultiStep <- lapply(EXPRES, eval_by_block_multi)
## h=1
resultsOneStep <- lapply(EXPRES, eval_by_block_single)

# Compute ranks h=1
ranksOneStep <- 
  lapply(resultsOneStep,
            function(x) {
              t(apply(x,1,rank))
            })

# Compute ranks h=1, without Naive
ranksOneStep_withoutNaive <- 
  lapply(resultsOneStep,
         function(x) {
           t(apply(x[,-5],1,rank))
         })

# Compute ranks h=18
ranksMultiStep <- 
  lapply(resultsMultiStep,
         function(x) {
           t(apply(x,1,rank))
         })

# Compute ranks h=18, without Naive
ranksMultiStep_withoutNaive <- 
  lapply(resultsMultiStep,
         function(x) {
           t(apply(x[,-5],1,rank))
         })

# ranksOneStep <- resultsOneStep
# Computing avg ranks by data point across time series
avgRankOS <- apply(simplify2array(ranksOneStep), 1:2, mean, na.rm=TRUE)
# same, but without naive 
avgRankOS_woNaive <- apply(simplify2array(ranksOneStep_withoutNaive), 1:2, mean, na.rm=TRUE)
# results itself
avgResOS <- apply(simplify2array(resultsOneStep), 1:2, median, na.rm=TRUE)

# to df
avgRankOS <- as.data.frame(avgRankOS)
avgRankOS_woNaive <- as.data.frame(avgRankOS_woNaive)
avgResOS <- as.data.frame(avgResOS)

# same as above, for h=18
avgRankMS <- apply(simplify2array(ranksMultiStep), 1:2, mean, na.rm=TRUE)
avgRankMS_woNaive <- apply(simplify2array(ranksMultiStep_withoutNaive), 1:2, mean, na.rm=TRUE)
avgResMS <- apply(simplify2array(resultsMultiStep), 1:2, median, na.rm=TRUE)
avgRankMS <- as.data.frame(avgRankMS)
avgRankMS_woNaive <- as.data.frame(avgRankMS_woNaive)
avgResMS <- as.data.frame(avgResMS)

# Smoothing using a moving avg of period 50,
## mostly for viz purposes
avgRankOS_Sm <- roll_mean_matrix(avgRankOS, 50)
avgRankOS_woNaive_Sm <- roll_mean_matrix(avgRankOS_woNaive, 50)
avgResOS_Sm <- roll_mean_matrix(avgResOS, 50)
avgRankMS_Sm <- roll_mean_matrix(avgRankMS, 50)
avgRankMS_woNaive_Sm <- roll_mean_matrix(avgRankMS_woNaive, 50)
avgResMS_Sm <- roll_mean_matrix(avgResMS, 50)


# Plotting one step
## Avg rank by sample size
# plot_learning_curve(avgRankOS)
## smoothed avg rank by sample size 
plot_learning_curve(avgRankOS_Sm)
## smoothed avg rank by sample size, w/o naive 
plot_learning_curve(avgRankOS_woNaive_Sm)
## smoothed avg res by sample size
plot_learning_curve(avgResOS_Sm) + 
  ylab("MASE") 

# Plotting multi step
## Avg rank by sample size
# plot_learning_curve(avgRankMS)
## smoothed avg rank by sample size 
plot_learning_curve(avgRankMS_Sm)
## smoothed avg rank by sample size, w/o naive 
plot_learning_curve(avgRankMS_woNaive_Sm)
## smoothed avg res by sample size
plot_learning_curve(avgResMS_Sm) + 
  ylab("MASE") 


### CComplexity
computational_complexity(X = EXPRES)

### Results by individual model

avgr <- round(apply(avgRankOS_Sm,2,mean),2)
names(avgr) <-
  c("ARIMA","ETS","Tbats","Theta","Naive2",
    "RF","GP","RBR","MARS","GLM")

avgr <- sort(avgr)
avgr

avgrMS <- round(apply(avgRankMS_Sm,2,mean),2)
names(avgrMS) <- 
  c("ARIMA","ETS","Tbats","Theta","Naive2",
    "RF","GP","RBR","MARS","GLM")

avgrMS <- sort(avgrMS)
avgrMS


