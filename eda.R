num_cols <- useful_data[sapply(useful_data , is.numeric)] 

col_list <- c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")
cor_num <- cor(num_cols)

num_cols <- num_cols[, col_list]
# none of the columns are highly correlated to each other, we can rule out risk of collinearity 

library(corrplot)
# d3heatmap(tbl_ready,Rowv = FALSE, Colv=FALSE)

corrplot(cor_num, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

summary(useful_data)
diab_Conthist<-function(VarName,NumBreaks,xlab,main,lengthxfit) ## xlab and main should be mentioned under quotes as they are characters
{
  hist(VarName,breaks=NumBreaks,col="yellow",xlab=xlab,main=main)

  # xfit<-seq(min(VarName),max(VarName),length=lengthxfit)
  # yfit<-dnorm(xfit,mean=mean(VarName),sd=sd(VarName))
  # yfit<-yfit*diff(h$mids[1:2])*length(VarName)
  # lines(xfit,yfit,col="red",lwd=3)
  
}


diab_Conthist(num_cols$time_in_hospital, 30, "time in hospital", "hosp_time", 100)
diab_Conthist(as.matrix(num_cols[, num_col]), 30, "num_lab_procedures", "num_lab_procedures", 100)

# histograms for continuous
for (num_col in colnames(num_cols)){
  jpeg(paste(num_col, ".jpeg", sep = ""), width = 350, height = 350)
  # 2. Create the plot
  diab_Conthist(as.matrix(num_cols[, num_col]),20, num_col, num_col,20)

  # 3. Close the file
  dev.off()
}


# bar plots for nominal variables

char_cols <- useful_data[sapply(useful_data , is.character)] 

diab_barplot<-function(VarName,main) ## xlab and main should be mentioned under quotes as they are characters
{
  VarName <- order(VarName, decreasing = TRUE)
  barplot(table(VarName),col="blue",main= main)
}



diab_barplot(useful_data[, "race"], "race")

