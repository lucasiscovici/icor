# partial correlation
old.pcor.test <- function(x,y,z,use="mat",method="p",na.rm=T){
	# The partial correlation coefficient between x and y given z
	#
	# pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
	#
	# x and y should be vectors
	#
	# z can be either a vector or a matrix
	#
	# use: There are two methods to calculate the partial correlation coefficient.
	#	 One is by using variance-covariance matrix ("mat") and the other is by using recursive formula ("rec").
	#	 Default is "mat".
	#
	# method: There are three ways to calculate the correlation coefficient, 
	#	    which are Pearson's ("p"), Spearman's ("s"), and Kendall's ("k") methods.
	# 	    The last two methods which are Spearman's and Kendall's coefficient are based on the non-parametric analysis.
	#	    Default is "p".
	#
	# na.rm: If na.rm is T, then all the missing samples are deleted from the whole dataset, which is (x,y,z).
	#        If not, the missing samples will be removed just when the correlation coefficient is calculated.
	#	   However, the number of samples for the p-value is the number of samples after removing 
	#	   all the missing samples from the whole dataset.
	#	   Default is "T".

	x <- c(x)
	y <- c(y)
	z <- as.data.frame(z)

	if(use == "mat"){
		p.use <- "Var-Cov matrix"
		pcor = old.pcor.mat(x,y,z,method=method,na.rm=na.rm)
	}else if(use == "rec"){
		p.use <- "Recursive formula"
		pcor = old.pcor.rec(x,y,z,method=method,na.rm=na.rm)
	}else{
		stop("\'use\' should be either \"rec\" or \"mat\"!\n")
	}

	# print the method
	if(gregexpr("p",method)[[1]][1] == 1){
		p.method <- "Pearson"
	}else if(gregexpr("s",method)[[1]][1] == 1){
		p.method <- "Spearman"
	}else if(gregexpr("k",method)[[1]][1] == 1){
		p.method <- "Kendall"
	}else{
		stop("\'method\' should be \"pearson\" or \"spearman\" or \"kendall\"!\n")
	}

	# sample number
	n <- dim(na.omit(data.frame(x,y,z)))[1]
	
	# given variables' number
	gn <- dim(z)[2]

	# p-value
	if(p.method == "Kendall"){
		statistic <- pcor/sqrt(2*(2*(n-gn)+5)/(9*(n-gn)*(n-1-gn)))
		p.value <- 2*pnorm(-abs(statistic))

	}else{
		statistic <- pcor*sqrt((n-2-gn)/(1-pcor^2))
  		p.value <- 2*pt(-abs(statistic),(n-2-gp))
	}

	data.frame(estimate=pcor,p.value=p.value,statistic=statistic,n=n,gn=gn,Method=p.method,Use=p.use)
}			

# By using var-cov matrix
old.pcor.mat <- function(x,y,z,method="p",na.rm=T){

	x <- c(x)
	y <- c(y)
	z <- as.data.frame(z)

	if(dim(z)[2] == 0){
		stop("There should be given data\n")
	}

	data <- data.frame(x,y,z)

	if(na.rm == T){
		data = na.omit(data)
	}

	xdata <- na.omit(data.frame(data[,c(1,2)]))
	Sxx <- cov(xdata,xdata,m=method)

	xzdata <- na.omit(data)
	xdata <- data.frame(xzdata[,c(1,2)])
	zdata <- data.frame(xzdata[,-c(1,2)])
	Sxz <- cov(xdata,zdata,m=method)

	zdata <- na.omit(data.frame(data[,-c(1,2)]))
	Szz <- cov(zdata,zdata,m=method)

	# is Szz positive definite?
	zz.ev <- eigen(Szz)$values
	if(min(zz.ev)[1]<0){
		stop("\'Szz\' is not positive definite!\n")
	}

	# partial correlation
	Sxx.z <- Sxx - Sxz %*% solve(Szz) %*% t(Sxz)
	
	rxx.z <- cov2cor(Sxx.z)[1,2]

	rxx.z
}

# By using recursive formula
old.pcor.rec <- function(x,y,z,method="p",na.rm=T){
	# 

	x <- c(x)
	y <- c(y)
	z <- as.data.frame(z)

	if(dim(z)[2] == 0){
		stop("There should be given data\n")
	}

	data <- data.frame(x,y,z)

	if(na.rm == T){
		data = na.omit(data)
	}

	# recursive formula
	if(dim(z)[2] == 1){
		tdata <- na.omit(data.frame(data[,1],data[,2]))
		rxy <- cor(tdata[,1],tdata[,2],m=method)

		tdata <- na.omit(data.frame(data[,1],data[,-c(1,2)]))
		rxz <- cor(tdata[,1],tdata[,2],m=method)

		tdata <- na.omit(data.frame(data[,2],data[,-c(1,2)]))
		ryz <- cor(tdata[,1],tdata[,2],m=method)

		rxy.z <- (rxy - rxz*ryz)/( sqrt(1-rxz^2)*sqrt(1-ryz^2) )
		
		return(rxy.z)
	}else{
		x <- c(data[,1])
		y <- c(data[,2])
		z0 <- c(data[,3])
		zc <- as.data.frame(data[,-c(1,2,3)])

		rxy.zc <- pcor.rec(x,y,zc,method=method,na.rm=na.rm)
		rxz0.zc <- pcor.rec(x,z0,zc,method=method,na.rm=na.rm)
		ryz0.zc <- pcor.rec(y,z0,zc,method=method,na.rm=na.rm)
		
		rxy.z <- (rxy.zc - rxz0.zc*ryz0.zc)/( sqrt(1-rxz0.zc^2)*sqrt(1-ryz0.zc^2) )
		return(rxy.z)
	}			
}
old.pcor_ <- function(x, method = c("pearson", "kendall", "spearman"))
{
  # correlation method
  method <- match.arg(method)
  
  # check the data
  if (is.data.frame(x)) 
    x <- as.matrix(x)
  if (!is.matrix(x)) 
    stop("supply a matrix-like 'x'")
  if (!(is.numeric(x) || is.logical(x))) 
    stop("'x' must be numeric")
  stopifnot(is.atomic(x))
  
  # sample number
  n <- dim(x)[1]
  
  # given variables' number
  gp <- dim(x)[2]-2
  
  # covariance matrix
  cvx <- cov(x,method=method)
  
  # inverse covariance matrix
  icvx <- solve(cvx)
  
  # partial correlation
  pcor <- -cov2cor(icvx)
  diag(pcor) <- 1
  
  # p-value
  if(method == "kendall"){
    statistic <- pcor/sqrt(2*(2*(n-gp)+5)/(9*(n-gp)*(n-1-gp)))
    p.value <- 2*pnorm(-abs(statistic))
    
  }else{r=
    statistic <- pcor*sqrt((n-2-gp)/(1-pcor^2))
    p.value <- 2*pt(-abs(statistic),n-2-gp)
  }
  
  diag(statistic) <- 0
  diag(p.value) <- 0
  
  list(estimate=pcor,p.value=p.value,statistic=statistic,n=n,gp=gp,method=method)
}

# semi-partial (part) correlation
old.spcor <- function(x, method = c("pearson", "kendall", "spearman"))
{
  # correlation method
  method <- match.arg(method)
  
  # check the data
  if (is.data.frame(x)) 
    x <- as.matrix(x)
  if (!is.matrix(x)) 
    stop("supply a matrix-like 'x'")
  if (!(is.numeric(x) || is.logical(x))) 
    stop("'x' must be numeric")
  stopifnot(is.atomic(x))
  
  # sample number
  n <- dim(x)[1]
  
  # given variables' number
  gp <- dim(x)[2]-2
  
  # covariance matrix
  cvx <- cov(x,method=method)
  
  # inverse covariance matrix
  icvx <- solve(cvx)
  
  # semi-partial correaltion
  spcor <- -cov2cor(icvx)/sqrt(diag(cvx))/sqrt(abs(diag(icvx)-t(t(icvx^2)/diag(icvx))))
  diag(spcor) <- 1
  
  # p-value
  if(method == "kendall"){
    statistic <- spcor/sqrt(2*(2*(n-gp)+5)/(9*(n-gp)*(n-1-gp)))
    p.value <- 2*pnorm(-abs(statistic))
    
  }else{
    statistic <- spcor*sqrt((n-2-gp)/(1-spcor^2))
    p.value <- 2*pnorm(-abs(statistic))
  }
  
  diag(statistic) <- 0
  diag(p.value) <- 0
  
  list(estimate=spcor,p.value=p.value,statistic=statistic,n=n,gp=gp,method=method)
}

# pairwise partial correlation
old.pcor.test_ <- function(x,y,z,method=c("pearson", "kendall", "spearman"))
{
  # The partial correlation coefficient between x and y given z
  #
  # pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix
  
  # correlation method
  method <- match.arg(method)
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  # merge into a matrix
  xyz <- data.frame(x,y,z)
  
  # partial correlation
  pcor = pcor_(xyz,method=method)
  
  data.frame(estimate=pcor$est[1,2],p.value=pcor$p.value[1,2],statistic=pcor$statistic[1,2],n=pcor$n,gp=pcor$gp,Method=method)
}	

# pairwise semi-partial (part) correlation
old.spcor.test <- function(x,y,z,method=c("pearson", "kendall", "spearman"))
{
  # The semi-partial (part) correlation coefficient between x and y given z
  #
  # spcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix
  
  # correlation method
  method <- match.arg(method)
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  # merge into a matrix
  xyz <- data.frame(x,y,z)
  
  # semi-partial (part) correlation
  spcor = spcor(xyz,method=method)
  
  data.frame(estimate=spcor$est[1,2],p.value=spcor$p.value[1,2],statistic=spcor$statistic[1,2],n=spcor$n,gp=spcor$gp,Method=method)
}
