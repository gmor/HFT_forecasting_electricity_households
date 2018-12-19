library(data.table)
library(ggplot2)
library(padr)
library(zoo)
library(lubridate)
library(xgboost)
library(Ckmeans.1d.dp)
library(oce)
library(jsonlite)
source("clustering.R")
source("meteo.R")
source("utils.R")

params <- fromJSON("config.json")
df <- fread("data/Minute_Haus_12_Export_Summer18_12cpGesamt@EnViSaGe-router00012.csv", data.table = F)
tz <- "Europe/Berlin"
colnames(df)<- c("t","v")
df$t <- as.POSIXct(df$t,tz = "UTC")
df <- pad(df,by="t")

# Aggregation of the minutary data to hourly.
df <- aggregate(data.frame("v"=df$v),by = list("t"=as.POSIXct(strftime(df$t,"%Y-%m-%d %H:00:00",tz="UTC"),tz="UTC")),FUN = function(x){mean(x,na.rm=T)})
df$v <- na.locf(na.locf(df$v), fromLast = TRUE)
df$h <- hour(with_tz(df$t,tz="Europe/Berlin"))
df$d <- as.Date(df$t,tz = "Europe/Berlin")

# Detect days with no usage
df <- detect_days_with_no_usage(df,value_column = "v",time_column = "d",plot_density = T,tz="Europe/Berlin")

# Detect the outliers
df <- detect_outliers_znorm(df, znorm_threshold=20, rolling_width = 25)

# Smoothing the hourly consumption (Moving average from t-2 to t+2).
df$vs <- rollmean(x= df$v, k=5, align = "center",fill = c(NA,NA,NA))

# Plots (time series and daily load curves).
ggplot(df) + geom_line(aes(t,v)) + theme_bw()
ggplot(df) + geom_line(aes(h,v,group=d),alpha=0.3) + theme_bw()
ggplot(df) + geom_line(aes(h,vs,group=d),alpha=0.3) + theme_bw()

# Clustering the daily load curves.
cl <- GaussianMixtureModel_clustering(df[df$out==F & df$nouse==F,],time_column = "t",value_column = "vs",k = 2:20,tz = "UTC",plot_file = "results_clustering.pdf", 
                                      latex_font= params$latex_font)
cl_days <- cl$df[!duplicated(cl$df[,"day"]),c("day","s")]
cl_days$s <- ifelse(is.na(cl_days$s),"NA",cl_days$s)
colnames(cl_days)=c("d","s")
cl_days[is.na(cl_days$s),"s"] <- "NA"

# Generate the daily consumption description dataframe
df_pr <- do.call(rbind,lapply(FUN=function(day){
  data.frame(
      "d" = day,
      "sum"=sum(df$v[df$d==day]),
      "sd"=sd(df$v[df$d==day]),
      "q95"=quantile(df$v[df$d==day],0.95),
      "q75"=quantile(df$v[df$d==day],0.75),
      "q50"=quantile(df$v[df$d==day],0.5),
      "q25"=quantile(df$v[df$d==day],0.25),
      "q5"=quantile(df$v[df$d==day],0.05),
      "max"=max(df$v[df$d==day]),
      "min"=min(df$v[df$d==day]),
      "weekend"= ifelse(strftime(day,"%w") %in% c("0","6"),1,0),
      "month"= as.numeric(strftime(day,"%m")),
      "weekday"= as.factor(strftime(day,"%w")),
      stringsAsFactors = F
    )
},X = unique(df$d)))
rownames(df_pr) <- NULL

# Add the clustering results
df_pr <- merge(df_pr,cl_days,by = "d")

# Obtain the weather conditions for the location
# weather <- get_weather(ts_from=paste0(min(df$d)," 00:00:00"),
#                        ts_to=paste0(max(df$d)," 23:59:59"),
#                        tz=tz,
#                        lat = 49.08,
#                        lon = 9.46)
weather <- fread("meteo_data/49.08_9.46_hist_hourly.csv",data.table = F)
weather$time <- with_tz(as.POSIXct(substr(weather$time,1,19),tz="UTC",
                                   format="%Y-%m-%d %H:%M:%S"), tz=tz)
sun_df<-as.data.frame(do.call(cbind,sunAngle(t=weather$time,latitude = 49.08,longitude = 9.46,useRefraction = T)))
sun_df$altitude <- ifelse(sun_df$altitude<0,0,sun_df$altitude)
weather$sunElev <- sun_df$altitude
weather$sunAzimuth <- ifelse(sun_df$altitude>0,sun_df$azimuth,0)

# Daily weather data
df_pr_w <- data.frame(
  "d"= aggregate(weather$temperature,by=list(as.Date(weather$time,tz=tz)),FUN=mean)$Group.1,
  "temp"= aggregate(weather$temperature,by=list("day"=as.Date(weather$time,tz=tz)),FUN=mean)$x,
  "wind"= aggregate(weather$windSpeed,by=list(as.Date(weather$time,tz=tz)),FUN=mean)$x,
  "GHI"= aggregate(weather$GHI,by=list(as.Date(weather$time,tz=tz)),FUN=sum)$x,
  "sunElev"= aggregate(weather$sunElev,by=list(as.Date(weather$time,tz=tz)),FUN=max)$x,
  stringsAsFactors=F
)

# Create the final dataframe to train the decision tree
df_pr <- merge(df_pr,df_pr_w,stringsAsFactors=F)
df_pr <- lagged_df(df=df_pr, lags=1:7, exception=c("d","weekend","weekday","month"))
df_pr <- df_pr[, grepl("lag",colnames(df_pr)) | (colnames(df_pr) %in% c("d","s"))]
df_pr <- df_pr[complete.cases(df_pr),]

# Define X (Input of the model)
X<-df_pr

# Calculate the XGBoost model
mod <- xgboost_framework(X,plots = T)


features <- colnames(x_v)
pred <- as.data.frame(predict(mod, newdata = x_v ))$response

prediction0<-matrix(predict(mod,x_v),
                    ncol=length(unique(X$s)), byrow=T)
colnames(prediction0)<-c(sprintf("p%02i",1:ncol(prediction0)))
prediction_v <- df_pr$s[vl]
prediction <- round(prediction0*100,2)
prediction <- as.data.frame(prediction[,sort(colnames(prediction))],stringsAsFactors=F)
prediction$position <- mapply(function(x){which(order(prediction[x,],decreasing = T) %in% as.numeric(as.character(y_labels_v[x]+1)))},1:nrow(x_data_v))

plot_day_ahead_load_curve_prediction(df_to_predict = df_pr, 
                                     prediction = prediction,
                                     prediction_v = mapply(function(x){which.max(prediction[x,])},1:nrow(prediction)),
                                     model="XGBoost",classification = NULL, validation=vl)












# 
x_data = model.matrix(as.formula(paste0(y,"~0+",paste(x,collapse="+"))),df_pr)

x_data_t <- x_data[tr,]
x_data_v <- x_data[vl,]

y_labels <- as.factor(df_pr$s)
levels(y_labels)<-sprintf("%02i",as.numeric(levels(y_labels))-1)
y_labels <- as.numeric(as.character(y_labels))
y_labels_t <- y_labels[tr]
y_labels_v <- y_labels[vl]

dtrain <- xgb.DMatrix(data = x_data_t, label = y_labels_t)
colnames(dtrain)


mod <- xgboost(data = dtrain, 
               max.depth = 7, eta = 0.03, nthread =8, 
               nrounds=400, min_child_weight = 1, gamma=0,
               colsample_bytree = 0.5,
               objective = "multi:softprob",
               eval_metric = "mlogloss",
               #scale_pos_weight = 1,
               num_class=length(unique(df_pr$s)))

xgboost::xgb.ggplot.importance(
  xgb.importance(feature_names =colnames(dtrain),
                 model = mod),top_n = 25
  )

prediction0<-matrix(predict(mod,x_data_v),
                    ncol=length(unique(df_pr$s)), byrow=T)
colnames(prediction0)<-c(sprintf("p%02i",1:ncol(prediction0)))
prediction_v <- df_pr$s[vl]
prediction <- round(prediction0*100,2)
prediction <- as.data.frame(prediction[,sort(colnames(prediction))],stringsAsFactors=F)
prediction$position <- mapply(function(x){which(order(prediction[x,],decreasing = T) %in% as.numeric(as.character(y_labels_v[x]+1)))},1:nrow(x_data_v))

plot_day_ahead_load_curve_prediction(df_to_predict = df_pr, 
                                     prediction = prediction,
                                     prediction_v = mapply(function(x){which.max(prediction[x,])},1:nrow(prediction)),
                                     model="XGBoost",classification = NULL, validation=vl)
