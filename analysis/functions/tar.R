


# A Function to estimate non-linear autoregressions (TAR)
# y is the dependent variable, x is the independent
# regime is the number of thresholds (1 or 2), defining the number of regimes
# minpct restricts the minimum number of observations in each regime (as percentage of total nr of obs, eg 0.1)
# grid is used to speed up estimation for large samples. If 0, thresholds are evaluated at each value of the independent variable. If a positive number n is used, an equally spaced n-point grid is used. 
# 10-19-2006 - Bjorn Van Campenhout - GNU licence

tar<-function (p_dif,minpct=0.2,grid=0, bw=100, iter=0) {

#take absolute values: assumption of symmetric market integration
y <- abs(p_dif)


	ressave=Inf
	data<-data.frame(y)
	data$ylag <- NA
	for (i in 2:length(data$y)) { data$ylag[i] <- data$y[i-1] }  
	data <- data[complete.cases(data),]
	dataord<-data.frame(data[order(data$ylag,data$y),])
	i<-round(minpct*length(data$ylag))
	while (i<round((1-minpct)*length(data$ylag))) {
		
		ind<-data$ylag>dataord[i,2]
		indep <- data$ylag*ind
		dep <- data$y-data$ylag
		

		res<-lm(dep~indep-1)
		#print(summary(res))
		#look for threshold that minimizes rss
		if (residuals(res)%*%residuals(res)<ressave) {
			ressave<-residuals(res)%*%residuals(res)
			
			thres<-dataord[i,2] 
			print(c(ressave, thres))
		}
	if (grid==0) {
		i<-i+1
	}
	else {
	i<-i+round((round((1-minpct)*length(data$ylag))-round(minpct*length(data$ylag)))/grid)
	}
}

#show final regression (1 threshold)
ind1<-as.integer(data$ylag>thres)

indep<-ind1*data$ylag
dep <- data$y-data$ylag
res<-lm(dep~indep-1)
print(summary(res))


res<<-res
thres<<-thres

}




