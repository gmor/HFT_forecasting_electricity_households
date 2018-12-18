## CLUSTERING FUNCTIONS ##

library(clusterCrit)
require(mclust)
library(tidyr)
library(cluster)
library(pracma)

normalize_range_int <- function(x,inf,sup,threshold_for_min=NULL){
  if(sup>inf){
    if (max(x)==min(x)){
      rep(inf,length(x))
    } else {
      r <- ((x-min(x))/(max(x)-min(x)))*(sup-inf)+inf
      if(!is.null(threshold_for_min)){
        r <- ifelse(x>=threshold_for_min,r,inf)
      }
      return(r)
    }
  }
}

normalize_perc_cons_day <- function(df){
  return(
    as.data.frame(
      t(
        mapply(
          function(i){as.numeric(df[i,]/sum(df[i,]))},
          1:nrow(df)
        )
      )
    )
  )
}

normalize_znorm <- function(df, spec_mean_sd=NULL){
  if (is.null(spec_mean_sd)){
    return(
      as.data.frame(
        mapply(
          function(i){(as.numeric(df[,i])-mean(df[,i],na.rm=T))/sd(df[,i],na.rm=T)},
          1:ncol(df)
        )
      )
    )
  } else {
    return(
      as.data.frame(
        mapply(
          function(i){(as.numeric(df[,i])-spec_mean_sd["mean",i])/spec_mean_sd["sd",i]},
          1:ncol(df)
        )
      )
    )
  }
}

GaussianMixtureModel_clustering<-function(df,time_column="t",value_column="v",k=NULL,title="",tz="UTC",plot_file=NULL){
  df_agg<-aggregate(as.numeric(df[,value_column]),by=list(
    strftime(strptime(df[,time_column],format="%Y-%m-%d %H:%M:%S",tz=tz),format = "%Y-%m-%d %H",tz=tz)),
    FUN=sum)
  df_agg<-data.frame(
    "time"= strptime(df_agg$Group.1,"%Y-%m-%d %H",tz=tz),
    "day"=as.Date(strptime(df_agg$Group.1,"%Y-%m-%d %H",tz=tz),tz=tz),
    "value"=df_agg$x,
    "dayhour"=sprintf("%02i:00",as.integer(substr(df_agg$Group.1,12,13))),
    stringsAsFactors = F
  )
  df_spread<-spread(df_agg[c("day","value","dayhour")],"dayhour","value")
  days <- df_spread[,1]
  df_spread<-df_spread[,-1]
  
  # percentage of consumption in the day
  df_spread_norm<- normalize_perc_cons_day(df_spread)
  # znorm
  df_spread_norm<- normalize_znorm(df_spread_norm)
  #df_spread_norm<- normalize_znorm(df_spread)
  
  
  # Generate the final spreated dataframe normalized
  complete_cases <- complete.cases(df_spread_norm)
  df_spread_norm<- df_spread_norm[complete_cases,]

  # Initialize the objects
  # Test a clustering from 2 to 10 groups, if k is NULL (default).
  if(is.null(k)) k=seq(2,12)
  
  # Clustering model
  mclust_results <- Mclust(apply(df_spread_norm,1:2,as.numeric),G = k, modelNames = c("VII","VEI"))
  
  mclust_results[['training_init']] = df_spread
  mclust_results[['value_column']] = value_column
  clustering <- list("cluster"=predict(mclust_results)$classification,"centers"=t(mclust_results$parameters$mean),"k"=mclust_results$G)
  
  # Delete those individuals far from its centroid.
  distance <-distmat(apply(df_spread_norm,1:2,as.numeric),clustering$centers)
  
  df_distances <- data.frame(
    "distance"=mapply(function(r){distance[r,clustering$cluster[r]]},1:nrow(df_spread_norm)),
    "k"=clustering$cluster
  )
  
  for(j in unique(clustering$cluster)){
    if(max(df_distances[df_distances$k==j,"distance"])>2*quantile(df_distances[df_distances$k==j,"distance"],0.025)){
    df_distances[df_distances$k==j,"k_new"] <- ifelse(
      (df_distances[df_distances$k==j,"distance"]>quantile(df_distances[df_distances$k==j,"distance"],0.975) |
         df_distances[df_distances$k==j,"distance"]<quantile(df_distances[df_distances$k==j,"distance"],0.025)),
      NA,
      j)
    } else {
      df_distances[df_distances$k==j,"k_new"] <- j
    }
  }
  
  # Generate the df_centroids dataset
  clustering_results <- data.frame(
    "day"=days[complete_cases],
    "s"=as.character(df_distances$k_new)
  )
  df_structural = merge(df_agg, clustering_results, by="day")
  
  df_centroids<-aggregate(df_structural$value,
                          by=list(df_structural$dayhour,df_structural$s),FUN=mean)
  colnames(df_centroids)<-c("dayhour","s","value")
  
  df_structural$s <- as.character(df_structural$s)
  
  # Plot
  if (!is.null(plot_file)){
    print(paste0("Clustering results saved in: ",plot_file))
    library(extrafont)
    #first download the fonts and unzip them  https://www.fontsquirrel.com/fonts/download/computer-modern
    #extrafont::font_import(pattern = "cmun*",paths = "/usr/share/fonts",recursive = T,prompt = F)
    loadfonts(quiet = T)
    pdf(plot_file,height = 6.5,width = 6.5)
    # print(ggplot(df_distances)+geom_histogram(aes(distance),binwidth=0.3)+
    #         xlim(c(min(df_distances$distance,na.rm=T),max(df_distances$distance,na.rm=T)))+
    #         facet_wrap(~k,nrow=2)+theme_bw() + theme(text= element_text(size=20, family="CM Roman")))
    # print(ggplot(df_distances[!is.na(df_distances$k_new),])+geom_histogram(aes(distance),binwidth=0.3)+
    #         xlim(c(min(df_distances$distance,na.rm=T),max(df_distances$distance,na.rm=T)))+
    #         facet_wrap(~k_new,nrow=2)+theme_bw() + theme(text= element_text(size=20, family="CM Roman")))
    print(ggplot(df_structural)+geom_line(aes(time,value,col=as.character(s),group=1))+
            theme_bw()+theme(legend.position="bottom",axis.text=element_text(size=20),
                             axis.title=element_text(size=20))+
            labs(x="time", y="Hourly consumption [kWh]")+
            theme(text= element_text(size=20, family="CM Roman")) + labs(col="Seasonality: "))
    print(ggplot(df_structural)+#[!is.na(df_structural$s),])+
            geom_line(aes(x=as.numeric(substr(dayhour,1,2)),y=as.numeric(value),group=as.factor(day)),alpha=0.2)+
            geom_line(data=df_centroids,aes(x=as.numeric(substr(dayhour,1,2)),y=as.numeric(value)),size=0.5,col='red')+
            facet_wrap(~s,nrow=2)+ #,nrow=length(levels(as.factor(df_structural$s)))
            theme_bw()+theme(legend.position="none",axis.text=element_text(size=20),
                             axis.title=element_text(size=20))+labs(x="Hour of the day", y="Hourly consumption (kWh)") +
            theme(text= element_text(size=20, family="CM Roman")))
    bic_df <- data.frame(k,BIC=mclust_results$BIC[,])
    if (!("BIC" %in% colnames(bic_df))){ bic_df$BIC <- bic_df[,paste0("BIC.",mclust_results$modelName)] }
    print(ggplot(bic_df)+
            geom_line(aes(k,BIC)) + geom_point(aes(k,BIC)) +
            geom_point(aes(k,BIC), col=2, cex=4, data=bic_df[which.max(bic_df$BIC),])+
            theme_bw() + theme(text= element_text(size=30, family="CM Roman")))
    dev.off()
  }
  embed_fonts(file = plot_file, outfile=plot_file)
  return(list("df"=df_structural,"mod"=mclust_results))
}

GaussianMixtureModel_prediction <- function(clustering_obj, new_data=NULL, value_column=NULL) {
  if (is.null(value_column)){ value_column <- clustering_obj$mod$value_column }
  mean_sd_to_normalize <- mapply(function(i) c("mean"=mean(clustering_obj$mod$training_init[,i],na.rm=T),"sd"=sd(clustering_obj$mod$training_init[,i],na.rm=T)),
                       1:ncol(clustering_obj$mod$training_init))
  new_data <- new_data[!duplicated(new_data$time),]
  df_spread<-spread(new_data[c("date",value_column,"hour")],"hour",value_column)
  days <- df_spread[,1]
  df_spread<-df_spread[,-1]
  # percentage of consumption in the day
  #df_spread_norm<- normalize_perc_cons_day(df_spread)
  # znorm
  #df_spread_norm<- normalize_znorm(df_spread_norm)
  df_spread_norm<- normalize_znorm(df_spread,spec_mean_sd = mean_sd_to_normalize)
  
  # Generate the final spreated dataframe normalized
  complete_cases <- complete.cases(df_spread_norm)
  new_data_norm<- df_spread_norm[complete_cases,]
  day_clusters = predict(clustering_obj$mod, newdata=new_data_norm)$classification
  results_prediction <- data.frame(
    "date"=days[complete_cases],
    "s"=as.character(day_clusters)
  )
  results <- merge(new_data,results_prediction,by.x="date",by.y="date",stringsAsFactors=F)
  results <- results[,colnames(results)!=value_column]
  colnames(results) <- c("day","time","value","dayhour","s")
  return(results)
}

clustering_load_curves_kmeans<-function(df,value_column="v",time_column="t",k=NULL,title="",plots=T,tz="UTC"){
  df_agg<-aggregate(data.frame("v"=df[,value_column]),by=list(
    "t" = strftime(strptime(df[,time_column],format="%Y-%m-%d %H:%M:%S",tz=tz),format = "%Y-%m-%d %H",tz=tz)),
    FUN=sum)
  df_agg<-data.frame(
    "time"= strptime(df_agg$t,"%Y-%m-%d %H",tz=tz),
    "day"=as.Date(strptime(df_agg$t,"%Y-%m-%d %H",tz=tz),tz=tz),
    "value"=df_agg$v,
    "dayhour"=sprintf("%02i:00",as.integer(substr(df_agg$t,12,13))),
    stringsAsFactors = F
  )
  df_spread<-spread(df_agg[c("day","value","dayhour")],"dayhour","value")
  days <- df_spread[,1]
  df_spread<-df_spread[,-1]
  
  # percentage of consumption in the day
  df_spread_norm<- normalize_perc_cons_day(df_spread)
  # znorm
  df_spread_norm<- normalize_znorm(df_spread_norm)
  #df_spread_norm<- normalize_znorm(df_spread)
  
  
  # Generate the final spreated dataframe normalized
  complete_cases <- complete.cases(df_spread_norm)
  df_spread_norm<- df_spread_norm[complete_cases,]
  
  # Initialize the objects
  # Test a clustering from 2 to 10 groups, if k is NULL (default).
  if(is.null(k)) k=seq(2,10)
  df_structural <- list()
  best_case <- 0
  silhouette_avg <- c()
  elem <- 0
  indexes<-list()
    
  if (plots==T) pdf(paste0("clustering_results_",title,".pdf"),height = 6.5,width = 8.5)
  print(paste0("Clustering results saved in: clustering_results_",title,".pdf"))
  
  for(i in 1:length(k)){
    
    cat('.')
    
    clustering = kmeans(apply(df_spread_norm,1:2,as.numeric),k[i],nstart = 10000,iter.max = 1000)
    # Delete those individuals far from its centroid.
    distance <-distmat(apply(df_spread_norm,1:2,as.numeric),clustering$centers)
    
    df_distances <- data.frame(
      "distance"=mapply(function(r){distance[r,clustering$cluster[r]]},1:nrow(df_spread_norm)),
      "k"=clustering$cluster
    )
    ggplot(df_distances)+geom_histogram(aes(distance))+facet_wrap(~k,nrow=1)+theme_bw()
    for(j in unique(clustering$cluster)){
      df_distances[df_distances$k==j,"k_new"] <- ifelse(
        (df_distances[df_distances$k==j,"distance"]>=quantile(df_distances[df_distances$k==j,"distance"],0.95) |
        df_distances[df_distances$k==j,"distance"]<=quantile(df_distances[df_distances$k==j,"distance"],0.05)),
        NA,
        j)
    }
    ggplot(df_distances)+geom_histogram(aes(distance))+facet_wrap(~k_new,nrow=1)+theme_bw()
    
    # Generate the df_centroids dataset
    clustering_results <- data.frame(
      "day"=days[complete_cases],
      "s"=as.character(df_distances$k_new)
    )
    df_structural[[i]] = merge(df_agg, clustering_results, by="day")
    
    df_centroids<-aggregate(df_structural[[i]]$value,
                            by=list(df_structural[[i]]$dayhour,df_structural[[i]]$s),FUN=mean)
    colnames(df_centroids)<-c("dayhour","s","value")
    
    df_structural[[i]]$s <- as.character(df_structural[[i]]$s)
    
    dist_for_silhouette <- dist(df_spread_norm[which(!is.na(df_distances$k_new)),])
    clusters_for_silhouette <- df_distances$k_new[!is.na(df_distances$k_new)]
    silhouette <- cluster::silhouette(clusters_for_silhouette,dist_for_silhouette)
    silhouette_avg <- c(silhouette_avg,mean(silhouette[,"sil_width"]))
    indexes[[i]] <- unlist(clusterCrit::intCriteria(as.matrix(df_spread_norm[which(!is.na(df_distances$k_new)),]),
                                             df_distances$k_new[!is.na(df_distances$k_new)],
                                             clusterCrit::getCriteriaNames(isInternal = T)))
    # best_case based only on the silhouette index
    if (best_case==0){
      best_case <- k[1]
    } else if(((silhouette_avg[i]-silhouette_avg[i-1])/silhouette_avg[i-1])*100>5){ # Only select a new best case if 5%
      best_case <- k[i]
    }
    elem = elem + 1
    
    if (plots==T){
      print(ggplot(df_structural[[i]])+geom_line(aes(time,value,col=as.character(s),group=1))+
              theme_bw()+theme(legend.position="none",axis.text=element_text(size=12),
                               axis.title=element_text(size=14))+
              labs(x="time", y="Consumption (kWh)")+
              ggtitle(paste0("Clustering k=",k[i])))
      print(ggplot(df_structural[[i]])+geom_line(aes(x=as.numeric(substr(dayhour,1,2)),y=as.numeric(value),group=as.factor(day)),alpha=0.2)+
              geom_line(data=df_centroids,aes(x=as.numeric(substr(dayhour,1,2)),y=as.numeric(value)),size=0.5)+
              facet_wrap(~s,nrow=length(levels(as.factor(df_structural[[i]]$s))))+
              theme_bw()+theme(legend.position="none",axis.text=element_text(size=12),
                               axis.title=element_text(size=14))+labs(x="Hour of the day", y="Consumption (kWh)")+
              ggtitle(paste0("Clustering k=",k[i])))
      plot(silhouette,col=1:i,main=paste0("Silhouette index. Clustering k=",k[i]))
    }
    
  }
  
  best_cases <- as.data.frame(t(mapply(function(index){
    index <- tolower(index)
    elem <- clusterCrit::bestCriterion(as.data.frame(do.call(rbind,indexes))[,index],index)
    if (!is.na(k[elem])){
      c(index,k[elem],unlist(indexes[[elem]])[index])
    } else {
      c(index,"NA","NA")
    }},
    clusterCrit::getCriteriaNames(isInternal = T)
    )),stringsAsFactors = F)
  rownames(best_cases) <- NULL
  colnames(best_cases) <- c("index", "k","value")
  best_cases$k <- as.numeric(best_cases$k)
  best_cases$value <- as.numeric(best_cases$value)
  summary_bc <- table(best_cases$k)
  #best_case <- names(summary_bc)[summary_bc==max(summary_bc)]
  
  #library(parallel)
  # Select the best case using the Gap Statistic.
  #mcmapply(function(i){
  item <- clusGap(apply(df_spread_norm,1:2,as.numeric),FUNcluster = kmeans, K.max = 20,B=50,nstart=1300,verbose = F)
  best_case <- maxSE(item$Tab[,"gap"],item$Tab[,"SE.sim"],method = c("globalmax"))
  #maxSE(item$Tab[,"gap"],item$Tab[,"SE.sim"],method = c("Tibs2001SEmax"))
  #},1:10,mc.cores = 8)
  
  if (plots==T){
    indexes_plot <- as.data.frame(do.call(rbind,indexes))
    rownames(indexes_plot) <- k
    indexes_plot <- reshape2::melt(as.matrix(indexes_plot))
    colnames(indexes_plot) <- c("k","index","value")
    print(ggplot(indexes_plot)+
      geom_line(aes(k, value))+
      geom_point(data=best_cases,aes(k,value), col="red")+
      geom_vline(aes(xintercept=best_case),col="blue")+
      scale_x_continuous(breaks=k)+
      facet_wrap(~index,scales = "free_y")+
      theme_bw())
    # print(ggplot(data.frame("silhouette"=silhouette_avg, "k"=k)) + geom_line(aes(k,silhouette)) + theme_bw()+ 
    #   geom_point(aes(k,silhouette),col='red',size=3, data=data.frame("silhouette"=silhouette_avg[k==best_case], "k"=best_case)) +
    #   theme(legend.position="none",axis.text=element_text(size=12),
    #         axis.title=element_text(size=14))+labs(y="Silhouette index", x="k") +
    #   ggtitle(paste0("Clustering k=",i)))
  }
  
  if (plots==T) dev.off()
  
  names(df_structural) <- as.character(k)
  
  return(df_structural[[as.character(best_case)]])
}

update_str_change<-function(df,results_clustering,time.x="time",time.y="time"){
  if(sum(colnames(df)=="s")>0) df <- df[,-(which(colnames(df)=="s"))]
  df <- merge(df,
              results_clustering[,c(time.y,"s")],
              by.x = time.x, by.y = time.y)
  return(df)
}
