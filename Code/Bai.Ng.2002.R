Bai.Ng.2002 <- function(data,k,criterion=NULL)
{
  # Estimates the principal components of the covariance matrix of data. data may be a data.frame,
  # ts or matrix. The function first zero-means the data. It returns the factor estimates F.bar and F.tilde
  # and the factor loadings Lambda.bar and Lambda.tilde, as in Bai and Ng (2008) p.99. It also
  # returns the eigenvalues and eigenvectors. Also, it returns the factor order estimated using the 6
  # methods proposed by Bai and Ng (2002). If k is an integer, then that is the order used, and the criterion
  # should be set to NULL or not specified. Alternatively, criterion may be one of "PC.1", "PC.2", "PC.3",
  # "IC.1", "IC.2", "IC.3", as defined in Bai and Ng (2002) p.201. In this case, k is interpreted as the 
  # maximum value of k considered (defined as kmax in Bai and Ng (2002). If criterion is not specified
  # then kmax is set to the minimum of 5*k and N/3.
  #
  # I have checked the results of this function against the code on Serena Ng's website
  # (http://www.columbia.edu/~sn2294/research.html) and code written by Christophe Hurlin
  # (http://www.runmycode.org/companion/view/69) (Note: Hurlin's code does not zero-mean the
  # data. This function does).
  #
  #                                              Chris Heaton, March 2016.

  # Remove NAs
  
  data <- na.omit(data)
  
  # Zero-mean the data and get the sample size
  
  data.mat <- scale(data,center=TRUE,scale=FALSE)
  T <- nrow(data.mat)
  N <- ncol(data.mat)
  
  # Compute the covariance matrix, and its eigenvalues and eigenvectors
  
  cov.mat <- t(data.mat)%*%data.mat/(T*N)
  eig.decomp <- eigen(cov.mat,symmetric=TRUE)
  eigs <- as.matrix(diag(eig.decomp$values))
  vecs <- as.matrix(eig.decomp$vectors)
  
  # Set up k and k.max
  
  if (is.null(criterion))
  {
    k.max <- min(5*k,floor(N/3))
  }else
  {
    k.max <- k
  }
  
  # Estimate order using Bai and Ng (2008) criteria. Set k equal to the requested estimate, or the
  # integer value supplied.
  
  small.eig.sum <- rev(cumsum(rev(diag(eigs))))[2:N]
  small.eig.sum <- abs(small.eig.sum)  # Numerical inaccuracies sometimes make the smallest eigenvalue negative.
  C.NT <- min(sqrt(T),sqrt(N))
  sig2 <- sum(diag(eigs))
  sig2.kmax <- small.eig.sum[k.max]
  ks <- seq(from=1,to=(N-1),by=1)
  
  g1.NT <- ((N+T)*log((N*T)/(N+T)))/(N*T)
  g2.NT <- ((N+T)*log(C.NT^2))/(N*T)
  g3.NT <- log(C.NT^2)/(C.NT^2)
#  g4.NT <- (N+T-k)*log(N*T)/(N*T)
  
  PC.1 <- small.eig.sum + ks*sig2.kmax*g1.NT
  PC.2 <- small.eig.sum + ks*sig2.kmax*g2.NT
  PC.3 <- small.eig.sum + ks*sig2.kmax*g3.NT
  IC.1 <- log(small.eig.sum) + ks*g1.NT
  IC.2 <- log(small.eig.sum) + ks*g2.NT
  IC.3 <- log(small.eig.sum) + ks*g3.NT

  k.PC.1 <- which.min(PC.1[1:k.max])
  k.PC.2 <- which.min(PC.2[1:k.max])
  k.PC.3 <- which.min(PC.3[1:k.max])
  
  k.IC.1 <- which.min(IC.1[1:k.max])
  k.IC.2 <- which.min(IC.2[1:k.max])
  k.IC.3 <- which.min(IC.3[1:k.max])

  est.k <- list(PC.1 = k.PC.1, PC.2 = k.PC.2, PC.3 = k.PC.3, IC.1 = k.IC.1, IC.2 = k.IC.2, IC.3 = k.IC.3)
  criteria <- list(PC.1 = PC.1, PC.2 = PC.2, PC.3 = PC.3, IC.1 = IC.1, IC.2 = IC.2, IC.3 = IC.3)

  if (identical(is.null(criterion),FALSE))
  {
    if (identical((criterion %in% c("PC.1","PC.2","PC.3","IC.1","IC.2","IC.3")),FALSE))
    {
      print(paste(criterion," is not a valid criterion. I'm using IC.1 instead."))
      criterion <- "IC.1"
    }
    k <- est.k[[criterion]]
  }
  
  # Now compute the estimates of Lambda.bar, F.bar, Lambda.tilde and F.tilde using the formulas from
  # Bai and Ng (2008).
  
  Lambda.bar <- as.matrix(vecs[,1:k]*sqrt(N))
  F.bar <- as.matrix(data.mat%*%Lambda.bar/N)
  Lambda.tilde <- Lambda.bar%*%sqrt(as.matrix(eigs[1:k,1:k]))
  F.tilde <- F.bar%*%solve(sqrt(as.matrix(eigs[1:k,1:k])))
  
  # Return all the estimates, the eigenvalues, eigenvectors, and the estimated factor orders.
  
  output <- list(eigs = eigs, vecs = vecs, F.bar = F.bar, Lambda.bar = Lambda.bar, F.tilde = F.tilde, Lambda.tilde = Lambda.tilde, est.k = est.k, criteria = criteria)
  return(output)
}
