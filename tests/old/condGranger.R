#' @export
condGranger <-
function(data,nx=1,ny=1,order=1,perm=FALSE,prob=TRUE,bs=100){
data <- as.matrix(data)
X <- t(data)
X <- X-rowMeans(X)
data <- t(X)

  if (perm == FALSE){
     x <- as.matrix(data[,1:nx])

     y <- as.matrix(data[,-(1:nx)])[,1:ny]

       if (sum(as.matrix(data[,-(1:(nx+ny))]))!=0)
         {
           z <- as.matrix(data[,-(1:(nx+ny))])
           nz <- ncol(z)

# Compute F2

                x  <-  embed(x,order+1)
                x2 <- as.matrix(x[,1:nx])
                xlag <- as.matrix(x[,-(1:nx)])

                y  <-  embed(y,order+1)
                y2 <- as.matrix(y[,1:ny])
                ylag <- as.matrix(y[,-(1:ny)])

                z  <-  embed(z,order+1)
                z2 <- as.matrix(z[,1:nz])
                zlag <- as.matrix(z[,-(1:nz)])


	    fitF <-lm(y2~cbind(ylag,zlag,xlag))
	    fitR <- lm(y2~cbind(ylag,zlag))
	    SSER <- sum(fitR$res^2)
	    SSEF <- sum(fitF$res^2)
            f <- log(SSER/SSEF)
	    prb <- anova(fitF,fitR)$Pr[2]
	    #prb <- 1-pf(exp(F),dfr,df2)
	    if(prob==TRUE){out <- list()
	    out$orig <- f
	    out$prob <- prb}
	    else{out <- f}
	    out
	    }
		  else{
		  x  <-  embed(x,order+1)
		  x2 <- as.matrix(x[,1:nx])
		  xlag <- as.matrix(x[,-(1:nx)])

		  y  <-  embed(y,order+1)
		  y2 <- as.matrix(y[,1:ny])
		  ylag <- as.matrix(y[,-(1:ny)])

		  fitF <-lm(y2~cbind(ylag,xlag))
		  fitR <- lm(y2~cbind(ylag))
		  SSER <- sum(fitR$res^2)
		  SSEF <- sum(fitF$res^2)
		  f <- log(SSER/SSEF)
			      prb <- anova(fitF,fitR)$Pr[2]
			      if(prob==TRUE){out <- list()
			      out$orig <- f
			      out$prob <- prb}
			      else{out <- f}
			      out
		  }
                          }

else{
# if(nx==1 & ny==1){
# l <- ncol(data)
# r <- nrow(data)
# out<-list()
# out$prob <- matrix(0,l,l)
# out$orig <- matrix(0,l,l)
# cg <- array(0,dim=c(l,l,bs))
# ll <- b.star(data,round=TRUE)[,1]
#
#
# for (bss in 1:bs){
# XX  <- matrix(0,l,r)
#   for (pp in 1:l){
#
#     nwin<- floor(r/ll[pp])
#     temp<- sample(nwin)
#     inx <- rep(temp,each=ll[pp])
#     data_ind <- cbind(inx,data[1:(ll[pp]*nwin),pp])
#     XX[pp,1:(ll[pp]*nwin)] <- data_ind[order(data_ind[,1]),-1]
#
#                 }
#   XX <- XX[,1:max(which(apply(XX,2,prod)!=0))]
#
#
# for(ii in 1:(l-1)) {
# for (jj in (ii+1):l){
# if (bss==1){
# out$orig[jj,ii]=condGranger(cbind(data[,ii],data[,jj],data[,-c(ii,jj)]),nx=nx,ny=ny,order=order)$orig
# out$orig[ii,jj]=condGranger(cbind(data[,jj],data[,ii],data[,-c(ii,jj)]),nx=nx,ny=ny,order=order)$orig
# }
#
# Xi <- t(rbind(XX[ii,],XX[jj,],XX[-c(ii,jj),]))
# cg[ii,jj,bss]=  condGranger(Xi,nx=nx,ny=nx,order=order)$orig
# Xj <- t(rbind(XX[jj,],XX[ii,],XX[-c(ii,jj),]))
# cg[jj,ii,bss]=  condGranger(Xj,nx=nx,ny=nx,order=order)$orig
# }}
# }
# for(ii in 1:(l-1)) {
# for (jj in (ii+1):l){
# #if (quantile(cg[ii,jj,],1-p/(ncol(data)^2-ncol(data)))>out$orig[ii,jj])
# #{out$prb[jj,ii]=0} else {out$prb[jj,ii]=1}
#
# #if (quantile(cg[jj,ii,],1-p/(ncol(data)^2-ncol(data)))>out$orig[jj,ii])
# #{out$prb[ii,jj]=0} else {out$prb[ii,jj]=1}
#
# out$prob[ii,jj] <- length(cg[ii,jj,][cg[ii,jj,]>=out$orig[ii,jj]])/length(cg[ii,jj,])
# out$prob[jj,ii] <- length(cg[jj,ii,][cg[jj,ii,]>=out$orig[jj,ii]])/length(cg[jj,ii,])
#
# }}
# #if(out$prb==numeric(0)){out$prb<-0}
# out
# }
#
# else{
l <- ncol(data)
r <- nrow(data)
out<-list()
cg <- matrix(0,bs,1)
ll <- b.star(data,round=TRUE)[,1]


for (bss in 1:bs){
XX  <- matrix(0,l,r)
  for (pp in 1:l){

    nwin<- floor(r/ll[pp])
    temp<- sample(nwin)
    inx <- rep(temp,each=ll[pp])
    data_ind <- cbind(inx,data[1:(ll[pp]*nwin),pp])
    XX[pp,1:(ll[pp]*nwin)] <- data_ind[order(data_ind[,1]),-1]

                }
  XX <- XX[,1:max(which(apply(XX,2,prod)!=0))]

if (bss==1){
out$orig  <- condGranger(data,nx=nx,ny=ny,order=order)$orig
           }
cg[bss,]    <- condGranger(t(XX),nx=nx,ny=ny,order=order)$orig

}
#if (quantile(cg,1-p/(ncol(data)^2-ncol(data)))>out$orig)
#{out$prb=0} else {out$prb=1}
out$prob <- length(cg[cg>=out$orig])/length(cg)
out
}
}
#}

