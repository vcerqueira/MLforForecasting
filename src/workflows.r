workflow_comparison <- 
  function(x, h, update_every_) {
    require(randtests)
    
    len <- length(x)
    frq <- frequency(x)
    ngroups <- len %/% update_every_
    
    split_x <- split(x, rep(1:ngroups, each = update_every_))
    
    results_by_block <- vector("list", ngroups)
    for (i in 18:(ngroups-h)) {## this needs to change if update_every_ != 1
      cat("Block",i,"/",ngroups-h,"\n")
      
      tr <- split_x[seq_len(i)]
      tr <- unname(do.call(c, tr))
      tst <- split_x[(i+1):(i+h)]
      tst <- unname(do.call(c, tst))
      
      ts_proc <-
        preprocessing_timeseries(tr = tr,
                                 tst = tst,
                                 frq = frq,
                                 k = 10)
      
      
      results <-
        apply_forecasting(train_adj = ts_proc$train_adj,
                          train_adj_xi = ts_proc$train_adj_xi,
                          train = ts_proc$train,
                          train_xi = ts_proc$train_xi,
                          embedded_tr = ts_proc$embedded_tr,
                          seasonal_factor = ts_proc$seasonal_factor,
                          lambda = ts_proc$lambda,
                          h = h)
      
      Y_hat <- results$Y_hat
      Y_hat <- do.call(rbind, Y_hat)
      Y_hat <- as.data.frame(Y_hat)
      
      raw_tst <- InvBoxCox(ts_proc$test, ts_proc$lambda)
      
      results_by_block[[i]] <-
        list(Y_hat = Y_hat,
             Time = results$Time,
             Test = raw_tst,
             TestBC = ts_proc$test,
             Train = tr)
    }
    results_by_block
  }


apply_forecasting <- 
  function(train_adj,
           train_adj_xi,
           train,
           train_xi,
           embedded_tr,
           seasonal_factor,
           lambda,
           h) {
    
    form <- target ~.
    
    modeltypes <- 
      c("rf",
        "gprocess",
        "rbr",
        "mars",
        "lasso")
    
    yh_ml <-
      lapply(modeltypes,
             function(nm) {
               cat(nm,"\n")
               train_prediction(
                 form = form,
                 train = embedded_tr,
                 h = h,
                 modeltype = nm)
             })
    
    names(yh_ml) <- modeltypes
    
    statmtype1 = c("tbats","theta","naive")
    yh_st1 <-
      lapply(statmtype1,
             function(nm) {
               cat(nm,"\n")
               stat_prediction(train_adj, h, nm)
             })
    names(yh_st1) <- statmtype1
    
    statmtype2 <- c("arima","ets")
    yh_st2 <-
      lapply(statmtype2,
             function(nm) {
               cat(nm,"\n")
               stat_prediction(train, h, nm)
             })
    names(yh_st2) <- statmtype2
    
    if (!is.null(train_adj_xi)) {
      yh_ml <- 
        lapply(yh_ml,
               function(x) {
                 x$y_hat <- diffinv(x$y_hat,xi = train_adj_xi)[-1]
                 x
               })
      
      yh_st1 <- 
        lapply(yh_st1,
               function(x) {
                 x$y_hat <- diffinv(x$y_hat,xi = train_adj_xi)[-1]
                 x
               })
    }
    
    if (!is.null(seasonal_factor)) {
      yh_ml <- 
        lapply(yh_ml,
               function(x) {
                 x$y_hat <- x$y_hat * seasonal_factor
                 x
               })
      
      yh_st1 <- 
        lapply(yh_st1,
               function(x) {
                 x$y_hat <- x$y_hat * seasonal_factor
                 x
               })
    }
    
    if (!is.null(train_xi)) {
      yh_st2 <- 
        lapply(yh_st2,
               function(x) {
                 x$y_hat <- diffinv(x$y_hat,xi = train_xi)[-1]
                 x
               })
    }
    
    Res <- c(yh_st2, yh_st1, yh_ml)
    
    Res <- 
      lapply(Res,
           function(x) {
             x$y_hat <- InvBoxCox(x$y_hat, lambda)
             x
           })
    
    Y_hat <- lapply(Res, function(x) x$y_hat)
    Time <- lapply(Res, function(x) x$time)
    Time <- sapply(Time, calc_time)
    
    list(Y_hat=Y_hat, Time=Time)
  }


preprocessing_timeseries <- 
  function(tr, tst, frq, k) {
    l <- length(tst)
    
    tr <- ts(tr, frequency = frq)
    tst <- ts(tst, frequency = frq)
    
    lambda <- BoxCox.lambda(tr)
    tr <- BoxCox(tr, lambda)
    tst <- BoxCox(tst, lambda)
    
    seas_test <- tryCatch(nsdiffs(tr), error = function(e) 0)
    if (seas_test > 0) {
      decomp <- 
        tryCatch(decompose(tr, "multiplicative"),
                 error=function(e) {
                   NULL
                 })
      
      if (!is.null(decomp)) {
        tr_adj = tr / decomp$seasonal
        
        seasonal_factor <- rep(tail(decomp$seasonal, frq), trunc(1 + l/frq))[1:l]
        
        tr_adj <- ts(tr_adj, frequency = frq)
      } else {
        tr_adj <- tr
        seasonal_factor <- NULL
      }
    } else {
      tr_adj <- tr
      seasonal_factor <- NULL
    }
    
    tr_adj[is.infinite(tr_adj)] <- NaN
    tr_adj[is.na(tr_adj)] <- mean(tr_adj, na.rm=TRUE)
    
    tr[is.infinite(tr)] <- NaN
    tr[is.na(tr)] <- mean(tr, na.rm=TRUE)
    
    coxstuart <- cox.stuart.test(tr_adj)$p.value
    if (coxstuart < .05) {
      tr_adj_xi <- tr_adj[length(tr_adj)]
      tr_adj <- diff(tr_adj, 1)
    } else {
      tr_adj_xi <- NULL
    }
    
    coxstuart2 <- cox.stuart.test(tr)$p.value
    if (coxstuart2 < .05) {
      tr_xi <- tr[length(tr)]
      tr <- diff(tr, 1)
    } else {
      tr_xi <- NULL
    }
    
    trk <- embed_timeseries_t(as.numeric(tr_adj), k)
    
    list(
      embedded_tr = trk,
      train_adj = tr_adj,
      train = tr,
      train_xi = tr_xi,
      train_adj_xi = tr_adj_xi,
      test = tst,
      seasonal_factor = seasonal_factor,
      lambda = lambda
    )
  }


embed_timeseries_t <- 
  function(x, k) {
    tryCatch(embed_timeseries(x,k),
             error = function(e) {
               embed_timeseries(x,k/2)
             })
  }

