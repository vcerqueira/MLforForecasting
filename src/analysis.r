plot_learning_curve <- 
  function(x) {
    require(ggplot2)
    require(reshape2)
    
    x <- as.data.frame(x)
    
    x$ID <- 18*(1:nrow(x))
    #x$ID <- 18:(nrow(x)+18-1)
    
    df <- melt(x,id.vars = "ID")
    
    df$Model_Type <- 
      !df$variable %in% 
      c("arima","ets","tbats","naive","theta")
    
    df$Model_Type <- 
      ifelse(df$Model_Type,
             "Machine Learning Method",
             "Statistical Method")
    
    colnames(df) <- c("Data_Size","Model","AvgRank","Type")
    
    ggplot(df, mapping = aes(x=Data_Size,y=AvgRank)) +
      geom_smooth(aes(color=Type),lwd=2) + 
      geom_line(aes(group=Model,color=Type),lwd=.5) +
      theme_minimal() +
      geom_vline(xintercept = 144) +
      theme(legend.position = "top") + 
      xlab("Training Sample Size") + 
      ylab("Avg. Rank") 
  }

avg_rank_plot <- 
  function(avg, sdev) {
    require(reshape2)
    require(ggplot2)
    
    ord <- names(sort(avg))
    
    methods <- names(avg)
    
    ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
    ds$methods <- factor(ds$methods, levels = ord)
    
    #ds <- melt(ds)
    
    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat="identity",
               fill="#33CCCC") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 35,
                                        size = 12,
                                        hjust = 1)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      # geom_errorbar(aes(ymin = avg - sdev,
      #                   ymax = avg + sdev),
      #               width = .5,
      #               position = position_dodge(.9)) +
      labs(x="",
           y="Avg Rank",
           title = "")
  }


cc_plot <- 
  function(x) {
    require(reshape2)
    require(ggplot2)
    
    avg_ <- colMeans(x)
    avg <- log(colMeans(x)+1)
    sdev <- log(apply(x,2,sd)+1)
    
    ord <- names(sort(avg))
    
    methods <- names(avg)
    
    ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
    ds$methods <- factor(ds$methods, levels = ord)
    
    #ds <- melt(ds)
    
    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat="identity",
               fill="#33CCCC") +
      geom_text(aes(label=round(avg_)), 
                vjust=1.3, 
                color="black", 
                size=4)+
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 35,
                                        size = 12,
                                        hjust = 1)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      # geom_errorbar(aes(ymin = avg - sdev,
      #                   ymax = avg + sdev),
      #               width = .5,
      #               position = position_dodge(.9)) +
      labs(x="",
           y="CC relative to Naive2",
           title = "")
  }

computational_complexity <- 
  function(X) {
    cc <- 
      sapply(X,
             function(x) {
               x <- x[!sapply(x,is.null)]
               
               cc_ <-
                 sapply(x,
                        function(y) {
                          y$Time
                        })
               
               rowSums(cc_)
             })
    
    cc <- as.data.frame(t(cc))
    cc[] <- lapply(cc, function(x) x / cc$naive)
    colnames(cc) <-
      c("ARIMA","ETS","Tbats","Theta",
        "Naive2","RF","GP","RBR","MARS","GLM")
    
    cc_plot(cc)
  }


eval_by_block_multi <- 
  function(x) {
    x <- x[!sapply(x,is.null)]
    
    r <- lapply(x, 
                function(o) {
                  tr <- o$Train
                  frq <- frequency(o$Test)
                  
                  if (length(tr) > frq) {
                    frq_ <- frq
                  } else {
                    frq_ <- 1
                  }
                  tr <- ts(tr, frequency = frq_)
                  
                  apply(o$Y_hat,1, function(yh) {
                    mean(mase_cal(insample = tr,
                                  outsample = as.vector(o$Test),
                                  forecasts = yh),na.rm = T)
                  })
                })
    
    r <- do.call(rbind, r)
    
    r
  }

eval_by_block_single <- 
  function(x) {
    x <- x[!sapply(x,is.null)]
    
    r <- lapply(x, 
                function(o) {
                  tr <- o$Train
                  frq <- frequency(o$Test)
                  
                  if (length(tr) > frq) {
                    frq_ <- frq
                  } else {
                    frq_ <- 1
                  }
                  tr <- ts(tr, frequency = frq_)
                  
                  se_mdl <- 
                    vapply(o$Y_hat[,1], 
                           function(yh) {
                             mean(mase_cal(insample = tr,
                                           outsample = as.vector(o$Test[1]),
                                           forecasts = yh),na.rm = T)
                           }, double(1L))
                  
                  names(se_mdl) <- rownames(o$Y_hat)
                  
                  se_mdl
                })
    
    r <- do.call(rbind, r)
    
    r
  }

#https://github.com/M4Competition/M4-methods
smape_cal <- function(outsample, forecasts) {
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}

#https://github.com/M4Competition/M4-methods
mase_cal <- function(insample, outsample, forecasts) {
  stopifnot(stats::is.ts(insample))
  #Used to estimate MASE
  frq <- stats::frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}