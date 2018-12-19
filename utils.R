## UTILS ##
library(gridExtra)
library(grid)
library(gtable)
library(dplyr)

lagged_df<- function(df,lags,exception){	
  if (is.null(exception)){	
    columns <- colnames(df)	
  } else {	
    columns <- colnames(df)[!(colnames(df) %in% exception)]	
  }	
  for (col in columns){	
    for (lagK in lags){	
      df <- data.frame(df,dplyr::lag(df[,col],lagK))	
      colnames(df)[ncol(df)] <- paste0(col,"_l",lagK)	
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
    paste0(model," - Average and median estimation probability of real day-ahead profile: ",	
           round(mean(mapply(function(x){	
             prediction[x,as.numeric(as.character(df_to_predict[validation,"s"][x]))]	
           },1:length(validation))),2),"% and ",	
           round(median(mapply(function(x){	
             prediction[x,as.numeric(as.character(df_to_predict[validation,"s"][x]))]	
           },1:length(validation))),2),	
           "%\nAverage position of the real day-ahead profile: ",	
           round(mean(prediction$position),2),	
           " - Percentage of days in position 5 or above: ",	
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
    ggsave(paste0(model,".pdf"), g,width=12,height=2+0.23*nrow(prediction))
  }
  
}

