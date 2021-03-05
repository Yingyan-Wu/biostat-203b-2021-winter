
proc_sum<-function(variable){
  var_naomit<-na.omit(variable) # remove missing values
  summ<-data.frame()# create vector to save info
  summ[1,1]<-length(var_naomit) # N
  summ[1,2]<-mean(var_naomit) # mean
  summ[1,3]<-median(var_naomit) # median
  summ[1,4]<-sd(var_naomit) # standard deviation
  summ[1,5]<-min(var_naomit) # minimum
  summ[1,6]<-max(var_naomit) # maximum
  summ[1,7]<-sum(is.na(variable)) # number of missing values
  names(summ)<-c("N","Mean","Median","Std",
                 "Minimum","Maximum","N_missing")
  return(round(summ,2))
}

