stat_prediction <-
  function(train,h,modeltype) {
    t0 <- Sys.time()
    
    modelf <- 
      switch(modeltype,
             "arima" = arima_model,
             "naive" = naive_model,
             "ets" = ets_model,
             "tbats" = tbats_model,
             "theta" = theta_model,
             naive_model)
    
    yh <- modelf(train, h)
    
    t1 <- Sys.time() - t0
    
    list(y_hat=yh,time=t1)
  }

arima_model <-
  function(train,h) {
    require(forecast)
    
    model <- tryCatch(
      forecast::auto.arima(y = train),
      error = function(e)
        naive(train,h=h)
    )
    
    yh <- forecast::forecast(model, h=h)
    
    as.vector(yh$mean)
  }


ets_model <-
  function(train,h) {
    require(forecast)
    
    model <- tryCatch(
      forecast::ets(y = train),
      error = function(e)
        naive(train,h=h)
    )
    
    yh <- forecast::forecast(model, h=h)
    
    as.vector(yh$mean)
  }

tbats_model <-
  function(train,h) {
    require(forecast)
    
    model <- tryCatch(
      forecast::tbats(y = train),
      error = function(e)
        naive(train,h=h)
    )
    
    yh <- forecast::forecast(model, h=h)
    
    as.vector(yh$mean)
  }


naive_model <-
  function(train,h) {
    require(forecast)
    
    yh <- snaive(train,h=h)
    
    as.vector(yh$mean)
  }

theta_model <-
  function(train,h) {
    require(forecast)
    
    yh <- tryCatch(thetaf(y = train,h=h),
                   error = function(e) {
                     naive(train,h=h)
                   })
    
    as.vector(yh$mean)
  }
