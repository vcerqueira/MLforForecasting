train_prediction <- 
  function(form,train,h,modeltype) {
    
    complete_par_list <- 
      list(rf= list(num.trees = c(50,100,250,500)),
           gprocess = list(kernel = c("rbfdot","vanilladot", "polydot","laplacedot"), 
                           tolerance=c(0.01,0.001)),
           rbr = list(committees = c(1, 5, 10, 25, 50)),
           lasso = list(alpha = c(0,.25,.5,.75,1)),
           mars = list(pmethod = c("forward","backward"), 
                       degree = c(1,2,3), 
                       nk =c(2,5,7,15))
      )
    
    t0 <- Sys.time()
    
    m <- tryCatch(train_model(form,train,modeltype, complete_par_list[[modeltype]]),
                  error = function(e) {
                    modeltype <<- "rf"
                    train_model(form,train,modeltype, complete_par_list[[modeltype]])
                  })

    
    test <- train[nrow(train),]
    test[,2:ncol(test)] <- unlist(test[,1:(ncol(test)-1)])
    test[,"target"] <- -1
    
    yh <- numeric(h)
    for (i in 1:h) {
      yh[i] <- predict_model(m,test,modeltype,form)
      
      if (is.na(yh[i])) {
        yh[i] <- mean(unlist(test[,-1]))
      }
      
      test[,3:ncol(test)] <- test[,2:(ncol(test)-1)]
      test$Tm1 <- yh[i]
    }
    
    names(yh) <- paste0("h_",1:h)
    
    t1 <- Sys.time() - t0
    
    list(y_hat=yh,time=t1)
  }

train_model <- 
  function(form,train,modeltype, par_list) {
    
    model <- 
      switch(modeltype,
           "rf" = lrandomforest(form, train,par_list),
           "gprocess" = gprocess(form, train,par_list),
           "rbr" = rbr(form, train,par_list),
           "lasso" = lasso(form, train,par_list),
           "mars" = mars(form, train,par_list),
           lrandomforest(form, train,par_list))
  }

predict_model <-
  function(model,newdata,modeltype,form) {
    newX <- model.matrix(form, newdata)
    yh <- 
      switch(modeltype,
           "rf" = predict(model, newdata)$predictions,
           "gprocess" = predict(model,newdata)[,1],
           "mars" = predict(model,newdata)[,1],
           "rbr" = predict(model,newX),
           "lasso" = unname(predict(model,newX)[,1]),
           predict(model, newdata)$predictions)
    
    yh
  }


lrandomforest <- 
  function(form,train, par_list) {
    require(ranger)
    mtry_ <- floor(ncol(train) / 3)
    
    in_tr_ts <- holdout_(train, .9)
    in_tr <- in_tr_ts$train
    in_ts <- in_tr_ts$test
    in_y <- get_y(in_ts, form)
    
    exp_pl <- expand.grid(par_list)
    
    seq. <- 1:nrow(exp_pl)
    
    in_results <- numeric(nrow(exp_pl))
    for (i in seq.) {
      in_m <- ranger(form,in_tr,
                     num.trees = exp_pl[i,"num.trees"], 
                     mtry = mtry_)
      
      in_yh <- predict_model(in_m, in_ts, "rf", target ~.)
      in_results[i] <- mean(smape_cal(in_y, in_yh))
    }
    
    best_var <- which.min(in_results)
    if (length(best_var) < 1) best_var <- 4
    
    ranger(form,train,
           num.trees = exp_pl[best_var,"num.trees"], 
           mtry = mtry_)
  }

gprocess <- 
  function(form,train, par_list) {
    require(kernlab)
    
    in_tr_ts <- holdout_(train, .9)
    in_tr <- in_tr_ts$train
    in_ts <- in_tr_ts$test
    in_y <- get_y(in_ts, form)
    
    exp_pl <- expand.grid(par_list)
    
    seq. <- 1:nrow(exp_pl)
    
    in_results <- numeric(nrow(exp_pl))
    for (i in seq.) {
      in_m <- gausspr(form,in_tr,
                      type = "regression",
                      kernel=as.character(exp_pl[i,"kernel"]),
                      tol=exp_pl[i,"tolerance"])
      
      in_yh <- predict_model(in_m, in_ts, "gprocess", target ~.)
      in_results[i] <- mean(smape_cal(in_y, in_yh))
    }
    
    best_var <- which.min(in_results)
    if (length(best_var) < 1) best_var <- 4
    
    gausspr(form,train,
            type = "regression",
            kernel=as.character(exp_pl[best_var,"kernel"]),
            tol=exp_pl[best_var,"tolerance"])
  }


rbr <- 
  function(form,train, par_list) {
    require(Cubist)
    
    in_tr_ts <- holdout_(train, .9)
    in_tr <- in_tr_ts$train
    in_ts <- in_tr_ts$test
    in_y <- get_y(in_ts, form)
    
    exp_pl <- expand.grid(par_list)
    
    seq. <- 1:nrow(exp_pl)
    
    in_results <- numeric(nrow(exp_pl))
    for (i in seq.) {
      in_X <- stats::model.matrix(form, in_tr)
      in_Y <- get_y(in_tr, form)
      
      in_m <- cubist(in_X, in_Y, 
                     committees = exp_pl[i,"committees"])
      
      in_yh <- predict_model(in_m, in_ts, "rbr", target ~.)
      in_results[i] <- mean(smape_cal(in_y, in_yh))
    }
    
    best_var <- which.min(in_results)
    if (length(best_var) < 1) best_var <- 4
    
    X <- stats::model.matrix(form, train)
    Y <- get_y(train, form)
    
    cubist(X, Y, 
           committees = exp_pl[best_var,"committees"])
  }


lasso <- 
  function(form,train, par_list) {
    require(glmnet)
    
    in_tr_ts <- holdout_(train, .9)
    in_tr <- in_tr_ts$train
    in_ts <- in_tr_ts$test
    in_y <- get_y(in_ts, form)
    
    exp_pl <- expand.grid(par_list)
    
    seq. <- 1:nrow(exp_pl)
    
    in_results <- numeric(nrow(exp_pl))
    for (i in seq.) {
      in_X <- stats::model.matrix(form, in_tr)
      in_Y <- get_y(in_tr, form)
      
      m.all <- glmnet(in_X, in_Y, 
                      alpha = exp_pl[i,"alpha"], 
                      family = "gaussian")
      
      in_m <- glmnet(in_X,in_Y,
        alpha = exp_pl[i,"alpha"],
        lambda = min(m.all$lambda),
        family = "gaussian")
      
      in_yh <- predict_model(in_m, in_ts, "lasso", target ~.)
      in_results[i] <- mean(smape_cal(in_y, in_yh))
    }
    
    best_var <- which.min(in_results)
    if (length(best_var) < 1) best_var <- 4
    
    X <- stats::model.matrix(form, train)
    Y <- get_y(train, form)
    
    m.all <- glmnet(X, Y, 
                    alpha = exp_pl[best_var,"alpha"], 
                    family = "gaussian")
    
    glmnet(
      X,
      Y,
      alpha = exp_pl[best_var, "alpha"],
      lambda = min(m.all$lambda),
      family = "gaussian"
    )
  }


mars <- 
  function(form,train, par_list) {
    require(earth)
    
    in_tr_ts <- holdout_(train, .9)
    in_tr <- in_tr_ts$train
    in_ts <- in_tr_ts$test
    in_y <- get_y(in_ts, form)
    
    exp_pl <- expand.grid(par_list)
    
    seq. <- 1:nrow(exp_pl)
    
    in_results <- numeric(nrow(exp_pl))
    for (i in seq.) {
      in_m <- earth(form,in_tr, 
                    pmethod = as.character(exp_pl[i,"pmethod"]),
                    degree = exp_pl[i,"degree"],
                    nk = exp_pl[i,"nk"])
      
      in_yh <- predict_model(in_m, in_ts, "mars", target ~.)
      in_results[i] <- mean(smape_cal(in_y, in_yh))
    }
    
    best_var <- which.min(in_results)
    if (length(best_var) < 1) best_var <- 4
    
    earth(form,train, 
          pmethod = as.character(exp_pl[best_var,"pmethod"]),
          degree = exp_pl[best_var,"degree"],
          nk = exp_pl[best_var,"nk"])
  }

