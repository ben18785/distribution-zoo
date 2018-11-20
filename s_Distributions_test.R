rm(list=ls())
library(shiny)
setwd("C:/Users/bclamber/Desktop/distribution-viewer")
runApp("App-1")

runApp(list(
  ui = bootstrapPage(
    sliderInput("mu", "Mean", min=-30, max=30, value=0, step=0.2),
    uiOutput('chunk')
  ),
  server = function(input, output) {
    output$chunk <- renderUI({ 
      HTML(markdown::markdownToHTML(text=paste0("```{r}",
                                                "\n dnorm(0, ", input$mu,", 2)"), 
                                    options=c("highlight_code"))) })
  }
))


n <- 10000
aMu <- rnorm(n)
aVar <- rgamma(n,shape = 1)
lResult <- rnorm(n,aMu,aVar)
hist(rexp(1000,aVar),100)

fSelectDistribution <- function(aDist){
  lResult <- switch(aDist,
                    normal=dnorm,
                    gamma=dgamma,
                    uniform=dunif,
                    t = dt,
                    exponential=dexp,
                    beta=dbeta,
                    dnorm)
  return(lResult)
}

aDist <- fSelectDistribution("t")
plot(seq(0,10,0.1),unlist(lapply(seq(0,10,0.1), function(x) aDist(x,df=1))))

lPMF <- dpois(lScale,1)
dataF <- data.frame(a=lScale,pmf=lPMF)
ggplot(data=dataF, aes(x=factor(a), y=pmf)) +
  geom_bar(stat="identity", position=position_dodge()) + xlab('X')

library(mvtnorm)
x.points <- seq(-3,3,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(1,1,1,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- mvtnorm::dmvt(c(x.points[i],y.points[j]),
                      delta=c(0,0),sigma=matrix(c(1,0,0,1),nrow=2),df=10,log=FALSE)
  }
}
dataF <- melt(z)
dataF$X1<-rep(x.points,100)
dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,100)))
names(dataF) <- c("x", "y", "z")
v <- ggplot(dataF, aes(x, y, z = z))
v + stat_contour(geom="polygon", aes(fill=..level..))+ stat_contour()

library(reshape)

pmvnorm(lower=c(-Inf,-Inf),upper=c(0,0),mean=c(0,0),sigma=matrix(c(1,0,0,1),nrow=2))[1]
install.packages('plotly')
runApp("Plotly-test",display.mode = "showcase")

library(plotly)

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
p <- plot_ly(midwest, x = percollege, color = state, type = "box")
p
Sys.setenv("plotly_username"="ben18785")
Sys.setenv("plotly_api_key"="gjue7w0w41")

n <- 10000
lWishart<-rWishart(n,8,diag(6))
lSigma1 <- sqrt(lWishart[1,1,])
lSigma2 <- sqrt(lWishart[2,2,])
lRho12 <- lWishart[1,2,]/(lSigma1 * lSigma2)

hist(lRho12)
hist(lWishart[1,1,])
n <- 10000
lList<-rWishart(n,5,diag(4))
lOffD <- vector(length=n)
lOffD1 <- vector(length=n)
lDiag <- vector(length=n)
for (i in 1:n){
  aMatrix <- solve(lList[,,i])
  lOffD[i] <- aMatrix[1,2]/sqrt(aMatrix[1,1]*aMatrix[2,2])
  lOffD1[i] <- aMatrix[2,3]/sqrt(aMatrix[2,2]*aMatrix[3,3])
  lDiag[i] <- aMatrix[1,1]
}
hist(lOffD)
hist(lOffD1)
hist(log(lDiag))

library(MASS)
a<-kde2d(lOffD,lOffD1,n=50)
image(a)

h1 <- qplot(lOffD)
h2 <- hist(lOffD1, breaks=25, plot=F)
top <- max(h1$counts, h2$counts)
k <- kde2d(lOffD, lOffD1, n=50)
k <- m
# margins
oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
k #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)

library(ggplot2)
library(ggExtra)
library(grid)
library(gridExtra)
aDataF <- data.frame(x=lOffD,y=lOffD1)
m <- ggplot(aDataF, aes(x = x, y = y)) +
  geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
p <- m + geom_density2d(size=1)
p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))


grid.arrange(h1,p1,ncol = 2, top = "Main title")
p1 = qplot(1:10, rnorm(10))
p2 = qplot(1:10, rnorm(10))
hist(lList[1,2,]/sqrt(lList[1,1,]*lList[2,2,]))

lIWishart <- lapply(seq(1,10000,1),function(x) rinvwishart(8,diag(4)))
lVec <- vector(length = 10000)
for (i in 1:10000){
  lVec[i]<-lIWishart[[i]][1,1]
}
hist(lVec)
install.packages('VarianceGamma')
library(VarianceGamma)
install.packages('grDevices')

### Code for plotting inverse Wishart
lList<-rWishart(input$sampleSizeInvWish,1,diag(4))
lOffD <- vector(length=input$sampleSizeInvWish)
lOffD1 <- vector(length=input$sampleSizeInvWish)
lDiag <- vector(length=input$sampleSizeInvWish)
for (i in 1:input$sampleSizeInvWish){
  aMatrix <- lList[,,i]
  lOffD[i] <- aMatrix[1,2]/sqrt(aMatrix[1,1]*aMatrix[2,2])
  lOffD1[i] <- aMatrix[2,3]/sqrt(aMatrix[2,2]*aMatrix[3,3])
  lDiag[i] <- aMatrix[1,1]
}
h1 <- qplot(log(lDiag),fill=I("blue")) + xlab(expression(paste("log(",sigma[1]^2,")")))
aDataF <- data.frame(x=lOffD,y=lOffD1)
m <- ggplot(aDataF, aes(x = x, y = y)) +
  geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
p <- m + geom_density2d(size=1)
p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
grid.arrange(h1,p1,ncol = 2)


a1<- a2<- a3<- 1
 a2<- .5
   a3<- .5
   x1<- x2<- seq(0.01, .99, by=.01)

   f<- function(x1, x2){
           term1<- gamma(a1+a2+a3)/(gamma(a1)*gamma(a2)*gamma(a3))
           term2<- x1^(a1-1)*x2^(a2-1)*(1-x1-x2)^(a3-1)
           term3<- (x1 + x2<  1)
           term1*term2*term3
           }

   z<- outer(x1, x2, f)
 z[z<=0]<- NA

   persp(x1, x2, z,main = "Dirichlet Distribution",
        col = "lightblue",
        theta = 100,
        phi = 20,
        r = 50,
        d = 0.1,
        expand = 0.5,
        ltheta = 90,
        lphi = 180,
        shade = 0.75,
        ticktype = "detailed",
        nticks = 5,
        zlim = if(length(na.omit(unique(as.vector(z))))<  2){ c(0,2.1) }else { NULL})

   library(DirichletReg)
   plot(DR_data(rdirichlet(1000, c(1,1))))
   plot(DR_data(rdirichlet(1000, c(4,1,1))), a2d = list(colored = FALSE))
   output$myWebGL <- renderWebGL({
     points3d(1:10, 1:10, 1:10)
     axes3d()
   })
   install.packages('shinyRGL')
   library(shinyRGL)
   
   library(scatterplot3d)
   X <- t(as.matrix(expand.grid(0:6, 0:6)))
   X <- X[ , colSums(X) <= 6]; X <- rbind(X, 6 - colSums(X))
   Z <- round(apply(X, 2, function(x) dmultinom(x, prob = 1:3)), 3)
   A <- data.frame(x = X[1, ], y = X[2, ], probability = Z)
   scatterplot3d(A, type = "h", lwd = 3, box = FALSE,angle=300)
   lUpperSigma <- rgamma(input$sampleSize,1,1)
   lData <- rnorm(1000)
   lData <- data.frame(data=lData)
   
   ggplot(lData, aes(data)) +
     geom_histogram(fill=I("blue"))
   

   install.packages('d3Network')
   