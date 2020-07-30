  library(datasets)
  ds <- longley
  ds
  
  b<-ds[0,1:length(ds)-1]
  bs<-colnames(ds)
  bs
  for (i in 1:length(b)){
    plot(x = ds[,i],y=ds$Employed,xlab = bs[i])
    
  }
  #the 3 variables most correlated to Employed:Year/Population/GNP
  ###year mod:
  year.mod <- lm(ds$Employed~ds$Year)
  year.mod
  plot(x = fitted.values(year.mod),y=ds$Employed)
  abline(a=0,b=1,col='red')
  
  ###Population mod:
  Population.mod <- lm(ds$Employed~ds$Population)
  Population.mod
  plot(x = fitted.values(Population.mod),y=ds$Employed)
  abline(a=0,b=1,col='red')
  
  ###GNP mod:
  GNP.mod <- lm(ds$Employed~ds$GNP)
  GNP.mod
  plot(x = fitted.values(GNP.mod),y=ds$Employed)
  abline(a=0,b=1,col='red')
  
  
  ###best coloration: GNP
  
  x<- ds$GNP
  X<-as.matrix(cbind(rep(1,length(x)),x))
  y<-ds$Employed
  
  error <- function(beta) {
    sum((X %*% beta - y)^2) # Sum of squared errors
  }
  # Define partials for b0 and b1
  del.b0 <- function(b0, b1) {
    sum(-y + (b1*x + b0)) * (2 / length(x))
  }
  del.b1 <- function(b0, b1) {
    sum(x %*% (-y + (b1*x + b0))) * (2 / length(x))
  }
   
  
 
   
  b0 <- 0
  b1 <- 0
  alpha = 0.1
  n.iter <- 500
  error.history <- numeric(n.iter)
  b0.history <- numeric(n.iter)
  b1.history <- numeric(n.iter)
  
  for (i in 1:n.iter) {
    b0.gradient <- del.b0(b0, b1)
    b1.gradient <- del.b1(b0, b1)
    
    b0 <- b0 - alpha*b0.gradient
    b1 <- b1 - alpha*b1.gradient
    beta <- as.matrix(c(b0, b1))
    
    error.history[i] <- error(beta)
    b0.history[i] <- b0
    b1.history[i] <- b1
  }
  
   
  error.history[n.iter]
  b0.history[n.iter]
  b1.history[n.iter] 
  
  