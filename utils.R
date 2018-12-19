## UTILS ##
library(gridExtra)
library(grid)
library(gtable)
library(pastecs)
library(mlr)

lagged_df<- function(df,lags,exception){
  if (is.null(exception)){	
    columns <- colnames(df)	
  } else {	
    columns <- colnames(df)[!(colnames(df) %in% exception)]
  }	
  for (col in columns){	
    for (lagK in lags){	
      df <- data.frame(df,dplyr::lag(df[,col],lagK))	
      colnames(df)[ncol(df)] <- paste0("lag",lagK,"_",col)	
    }	
  }	
  return(df)	
}

plot_day_ahead_load_curve_prediction <- function(df_to_predict, prediction, prediction_v, model, classification, validation){	
  
  g<-tableGrob(prediction)	
  
  # Coloured cells to highlight the real and most probable daily load curve.	
  find_cell <- function(table, row, col, name="core-fg"){	
    l <- table$layout	
    which(l$t==row & l$l==col & l$name==name)	
  }	
  for (i in 1:nrow(prediction)){	
    assign(paste0("real",i),find_cell(g, i+1, as.numeric(as.character(df_to_predict[validation,"s"][i]))+1,name="core-bg"))	
    assign(paste0("est",i),find_cell(g, i+1, as.numeric(as.character(prediction_v))[i]+1,name="core-fg"))	
    g$grobs[get(paste0("real",i))][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)	
    g$grobs[get(paste0("est",i))][[1]][["gp"]] <- gpar(fontsize=12, fontface="bold",col="red")	
  }	
  
  # Add a title to the table	
  title <- textGrob(	
    paste0(model," - Avg/Median probability of real day-ahead profile: ",	
           round(mean(mapply(function(x){	
             prediction[x,as.numeric(as.character(df_to_predict[validation,"s"][x]))]	
           },1:length(validation))),2),"%/",	
           round(median(mapply(function(x){	
             prediction[x,as.numeric(as.character(df_to_predict[validation,"s"][x]))]	
           },1:length(validation))),2),	
           "%\nAverage position of the real day-ahead profile: ",	
           round(mean(prediction$position),2),	
           "\nPercentage of days in position 5 or above: ",	
           round((sum(prediction$position<=5)/nrow(prediction))*100,2),"%"),	
    gp=gpar(fontsize=14))	
  padding <- unit(5,"mm")	
  g <- gtable_add_rows(	
    g, 	
    heights = grobHeight(title) + padding,	
    pos = 0)	
  g <- gtable_add_grob(	
    g, 	
    title, 	
    1, 1, 1, ncol(g))	
  
  # Print to PDF
  if(!is.null(classification)){
    ggsave(paste0(model,".pdf"),	
         grid.arrange(	
           classification$plot,	
           g, nrow=2,heights=c(1,nrow(prediction)/15)),width=12,height=8+0.23*nrow(prediction))
  } else {
    ggsave(paste0(model,".pdf"), g,width=10,height=2+0.23*nrow(prediction))
  }
  
}

xgboost_framework <- function (X, plots=F){
  
  # Training and validation period
  tr <- sample(1:nrow(X),nrow(X)*0.9,replace = F)
  vl <- (1:nrow(X))[!((1:nrow(X)) %in% tr)]
  
  # Definition of features and data input
  features <- setdiff(names(X),c("d","s"))
  # Y
  y_labels <- as.factor(X$s)
  levels(y_labels)<-sprintf("%02i",ifelse(is.na(as.numeric(levels(y_labels)))==T,max(as.numeric(levels(y_labels)),na.rm=T),as.numeric(levels(y_labels))-1))
  y_labels <- as.numeric(as.character(y_labels))
  y_labels_t <- y_labels[tr]
  y_labels_v <- y_labels[vl]
  # X
  x_ <- model.matrix(as.formula(paste0("s ~ 0 +",paste(features,collapse="+"))),X)
  x_t <- x_[tr,]
  x_v <- x_[vl,]
  # Input to the model
  dtrain <- xgb.DMatrix(
    data = x_t,
    label = y_labels_t
  )
  traintask <- makeClassifTask(data = data.frame("s"=as.factor(X$s)[tr],x_t),target="s")
  
  # XGboost parameters set
  watchlist <- list(train = dtrain)
  param <- list(  booster = "gbtree",
                  objective = "multi:softprob",
                  eval_metric = "mlogloss"
  )
  lrn <- makeLearner("classif.xgboost",predict.type = "prob")
  lrn$par.vals <- list( objective=param$objective, booster=param$booster, eval_metric=param$eval_metric,
                        subsample=param$subsample, colsample_bytree = param$colsample_bytree,
                        nrounds=param$nrounds, verbose=0)
  params <- makeParamSet( makeIntegerParam("max_depth",lower = 2L,upper = 8L), 
                          makeIntegerParam("min_child_weight",lower = 2L,upper = 20L),
                          makeIntegerParam("nrounds",lower = 20L,upper = 400L),
                          makeNumericParam("gamma",lower = 0,upper = 0.5),
                          makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                          makeNumericParam("subsample",lower = 0.5,upper = 1),
                          makeNumericParam("lambda",lower = 0,upper = 0.4),
                          makeNumericParam("alpha",lower = 0.7,upper = 1),
                          makeNumericParam("eta",lower = 0.01,upper = 0.3) )
  rdesc <- makeResampleDesc("CV",iters=5L)
  ctrl <- makeTuneControlRandom(maxit = 100L)
  mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = logloss, 
                       par.set = params, control = ctrl, show.info = F)
  param <- append(param, mytune$x)
  
  if(plots==T){
    xgb_cv_1 = xgb.cv(params=param,
                      data=dtrain,
                      nfold = 10,
                      num_class= 10,
                      nrounds = param$nrounds)
    plot(xgb_cv_1$evaluation_log$train_mlogloss_mean,type="l",ylab="logloss",main="XGboost -- Training (Black) vs. Validation (Red) logloss")
    lines(xgb_cv_1$evaluation_log$test_mlogloss_mean,col=2)
  }
  features <- setdiff(names(new_data),c("value",columns_to_delete,"daypart","season"))
  pred <- exp(as.data.frame(predict(mod, newdata = as.data.frame(tonumeric_transformation(new_data[features]))))$response)
  
  lrn = setHyperPars(lrn, par.vals = param)
  mod = train(lrn, task = traintask)
  
  return(mod)
}

detect_days_with_no_usage <- function(df, value_column, time_column, plot_density=T,tz="UTC"){
  
  df_ini <- df
  # aggregate the data to daily
  df <- aggregate(data.frame("v"=df[,value_column]),list("d"=df[,time_column]),function(x){sum(x,na.rm=T)})
  # do not consider the values greater than percentile 70.
  df <- df[df[,"v"]<=quantile(df[,"v"],0.7,na.rm=T),]
  # calculate the Probability Density Function
  d <- density(df[,"v"],na.rm=T)
  # calculate the local extremes
  tp<-turnpoints(ts(d$y))
  # defines the local maximums and minimums
  importance <- d$y[tp$tppos]
  cons <- d$x[tp$tppos] 
  shifted_importance <- c(0,shift(importance,1)[is.finite(shift(importance,1))])
  min_max <- ifelse(importance-shifted_importance>0,"max","min")
  cons_max <- c(NA,cons[min_max=="max"])
  importance_max <- c(NA,importance[min_max=="max"])
  # If no more than 2 local extremes are detected in the PDF, assume a consumption limit of 0. 
  cons_limit <- 0.0
  if (length(importance)>=3){
    # Initially assume the first minimum as the consumption limit if the first maximum is lower or equal to the
    # percentile 20 and its importance in the PDF is higher than the 20% of the maximum importance detected.
    if (cons[1]<=quantile(df[,"v"],0.2,na.rm=T) & importance[1] > 0.2*max(importance)) cons_limit <- cons[2]
    # Select as the consumption limit, the minimum before the first important, or relevant, mode of the distribution. 
    # Select the first important mode considering that: 
    # - It must has a difference higher than a 200% over its previous maximum
    # - It cannot be the first maximum (then, no first important mode is detected, thus any day is deleted from the dataset),
    # - Its consumption must be higher than the percentile 10 of the daily consumption distribution.
    first_important_mode <- mapply(function(i){
      differ <- ((importance_max[i+1]-importance_max[i])/importance_max[i])
      ((differ*100 > 200  & is.finite(differ)) & cons_max[i+1]>quantile(df[,"v"],0.1,na.rm=T))
    },1:(sum(min_max=="max")))
    if(any(first_important_mode)==T) cons_limit <- cons[which(importance == importance_max[which.max(first_important_mode)+1])-1]
    # if the first important mode has a daily consumption of less than 0.5kWh (15 kWh per month) then select as
    # cons_limit the higher minimum closest to this first important mode 
    if (cons_max[which.max(first_important_mode)+1][1]<0.5) cons_limit <- cons[which(importance == importance_max[which.max(first_important_mode)+1])+1]
  }
  if (length(cons_limit)==0){ cons_limit <- 0 }
  if (length(cons)==1){
    if(cons < 0.5) cons_limit <- cons
  }
  days_detected <- as.Date(df[df[,"v"] < cons_limit & is.finite(df[,"v"]),"d"], tz=tz)
  # plot the PDF and the local extremes
  if(plot_density==T){
    plot(d, main="",xlab="Consumption [kWh]")
    points(d$x[tp$tppos],d$y[tp$tppos],col="red",cex=2)
    abline(v = cons_limit,col=3)
  }
  # return
  if(length(days_detected)==0){
    return(df_ini)
  }else{
    df_ini$nouse <- (df_ini[,time_column] %in% unique(days_detected))
    return(df_ini)
  }
}

delete_days_with_no_usage <- function(df, days_to_delete, value_column, time_column){
  if(!is.null(days_to_delete)){
    dates <- as.Date(df[,time_column], tz="UTC")
    df[(dates %in% days_to_delete),value_column] <- NA
  }
  return(df)
}

detect_outliers_znorm <- function(df,znorm_threshold,rolling_width){
  znorm <- c(rep(0,floor(rolling_width/2)),
             zoo::rollapply(data=df$v,
                 width=rolling_width,
                 align="center",
                 FUN=function(x){
                      (x[1]-median(x,na.rm=T))/
                        sd(x[x>quantile(x,0.1,na.rm=T) & x<quantile(x,0.9,na.rm=T)],na.rm=T)
                     }),
             rep(0,floor(rolling_width/2)))
  df$znorm <- znorm
  df$out <- ifelse(znorm>=znorm_threshold,T,F)
  print(ggplot(df) + geom_line(aes(t,znorm)) + geom_point(data=df[df$out==T,],aes(t,znorm),col=2,size=2))
  return(df)
}
