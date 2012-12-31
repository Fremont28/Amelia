## Code for bootstrapped Amelia ported to R
## 17/09/05 jh - Added "subset" routine for idvars and completely missing observations
## 22/09/05 jh - Changed stack function to optionally fix column positions, changed bootx to reject some bootstraps, changed emarch to work when no data missing
## 23/09/05 mb - Added "amcheck" function to change data and check for errors, "impdata" now in format given to amelia.
## 24/09/05 jh - Modified "amcheck," added polynomials of time, added ability to impute "logicals" from data frames
## 25/09/05 jh - Finalized plumbing for observational priors
## 26/09/05 mb - Added "frontend" argument and screen box to amelia and emarch functions
## 27/09/05 jh - Added observational and empirical priors
## 28/09/05 mb - Fixed "frontend" to update the GUI after each print.
## 30/09/05 mb - "amcheck" expanded, priors passed as individual matrices
## 07/10/05 mb - Added passing of lags and multiple levels of polynomials;  expanded "amcheck" to cover these
## 08/10/05 jh - Enabled variable degree of polynomials of time, enabled interaction with cross-section
## 14/10/05 mb - Put "amcheck" into its own file
## 21/10/05 mb - Changed "stack" to "amstack" (and "unstack"); added log transformations in "amtransform"; adding "archive" option that saves a list of the settings
## 21/10/05 mb - Added a soft-crash that will print and output the error number and message.
## 24/10/05 mb - Added "sqrt" option for square root transformations, "lgstc" for logistic transformations
## 27/10/05 mb - Enabled lags and leads
## 9//11/05 mb - Enabled nominals;  added "incheck" to allow skipping amcheck;  moved dataframe->matrix conversion to framemat function.
## 15/12/05 mb - new (fixed) impute function;
## 21/02/06 mb - added positive definite check to "startvals", now defaults to identity if not pd.
## 22/02/06 mb - penrose inverse function added in sweep; soft-crashes on a non invertible covariance matrix at the end of EM
## 23/02/06 mb - empri increases if EM hits a non-monotonic section; added 'startvals' option; added iteration history to archive;
## 21/03/06 mb - added "ords" option and added ordinal support in "unsubset"; fixed a bug in nominals that wouldn't fill in imputations;
## 22/03/06 mb - character/factors can be specified and returned correctly as ordinals;
## 08/04/06 jh - revised functions to handle large datasets, merged with parallel emb.r version
## 10/04/06 mb - added "nametonumber" function that converts column names to numbers in all of the list options
## 28/04/06 mb - extracted transformation functions to prep.r
## 29/04/06 jh - changed screen output for "p2s", ivector and icap in "indxs", revised "diff" convergence monitor to upper triangular
## 01/05/06 mb - removed "rbind" calls in emfred, impute.
## 01/06/06 mb - added "allthetas" option to emarch for overdispersion diagnostic
## 15/06/06 jh - merged with priors version changing all EM and impute procs, modified how lists are generated in indxs("icap") and amelia("impdata").
## 27/06/06 mb - added arglist argument to load in output from amelia or the gui.
## 13/07/06 mb - moved gc() calls out of emfred into emarch
## 02/08/06 mb - removed data.matrix() call when calling unsubset (moved to prep), fixed impfill for char.
## 29/08/06 jh - changed tolerance defaults
## 20/09/06 mb - new option (temp?) keep.data that will trash datasets from memory
## 01/10/06 mb - added additional info to p2s=2.
## 27/11/06 mb - new priors format
## 15/01/07 jh/mb - final version changes, degrees of freedom messages,autoprior option, modified comments, rearranged core arguments
## 10/05/07 mb - changed 'impute' to 'amelia.impute'
## 04/07/07 jh - added "emburn" option to modify convergence criteria
## 04/06/08 mb - changed the writing to GUI ('if (frontend)' calls) to remove globals
## 17/07/08 mb - fixed frontend error bug (dumping output to screen
## 22/07/08 mb - good coding update: T->TRUE/F->FALSE
## 27/03/10 jh - small changes to arguments of functions to deal with "splinetime" option in same fashion as "polytime"


## Draw from a multivariate normal distribution
##   n: number of draws
##   mu: vector of means
##   vcv: variance-covariance matrix
rmvnorm <- function(n,mu,vcv){
  return(matrix(rnorm(n*length(mu)),n,length(mu)) %*% (chol(vcv)) + (matrix(1,n,1) %*% mu ) )
}

## Returns the data matrix without the rows with missing values
## (Same return as na.omit, without any attributes)
##   x: data matrix
##   can't send it a vector right now
packr<-function(x) {
  r<-is.na(x)
  sumr<-rowSums(r)
  x2<-x[sumr==0, , drop=FALSE]
  return(x2)
}

## Moore-Penrose Inverse function (aka Generalized Inverse)
##   X:    symmetric matrix
##   tol:  convergence requirement
mpinv <- function(X, tol = sqrt(.Machine$double.eps)) {
  s <- svd(X)
  e <- s$d
  e[e > tol] <- 1/e[e > tol]
  s$v %*% diag(e,nrow=length(e)) %*% t(s$u)
}

## Create dataset bootstrapped from original dataset
## Rejects Bootstraps where an entire variable becomes missing
##   x:          data (matrix)
##   priors:     matrix of priors about means for observations
bootx<-function(x,priors=NULL){
  flag <- TRUE
  AMn <- nrow(x)
  while (flag){
    order<-trunc(runif(nrow(x), min=1, max=nrow(x)+1))
    xboot<-x[order,]
    if (!identical(priors,NULL)){
      sigPriors <- matrix(NA,nrow(x),ncol(x))
      muPriors <- matrix(NA,nrow(x),ncol(x))
      muPriors[priors[,1:2]] <- priors[,3]
      sigPriors[priors[,1:2]] <- priors[,4]
      muPriors <- muPriors[order,]
      sigPriors <- sigPriors[order,]
      prior.ind <- which(!is.na(muPriors), arr.ind = TRUE)
      priors <- cbind(prior.ind, muPriors[prior.ind], sigPriors[prior.ind])
                                        # priors[,1]<-match(priors[,1],order)
                                        #priors <- priors[!is.na(priors[,1]),,drop=FALSE]
    }

    flag<-any(colSums(is.na(xboot))==AMn & !((1:ncol(xboot)) %in% priors[,2]))
  }
  return(list(x=xboot,priors=priors))
}

## Put imputations into the original data format
## Converts integer values back to factors or characters
impfill<-function(x.orig,x.imp,noms,ords,priors) {
  if (!is.null(priors)) {
    is.na(x.orig)[priors[,c(1,2)]] <- TRUE
  }
  AMr1.orig <- is.na(x.orig)
  orig.fact <- sapply(x.orig, is.factor)
  orig.char <- sapply(x.orig, is.character)
  x.imp<-as.data.frame(x.imp[,1:ncol(x.orig)])
  for (i in 1:ncol(x.orig)) {
    if (is.logical(x.orig[,i]) & sum(!is.na(x.orig[,i])) > 0) {
      x.imp[,i]<-as.logical(x.imp[,i]>0.5)
    }
  }

  possibleFactors <- unique(c(noms,ords))

  if (!is.null(possibleFactors)) {
    if (ncol(x.orig) > length(possibleFactors)) {
      AMr1.orig <-is.na(x.orig[,-possibleFactors])
      x.orig[,-possibleFactors][AMr1.orig]<-x.imp[,-possibleFactors][AMr1.orig]
    }
    for (i in possibleFactors) {
      if (orig.fact[i])
        x.orig[is.na(x.orig[,i]),i]<- levels(x.orig[,i])[x.imp[is.na(x.orig[,i]),i]]
      else if (orig.char[i])
        x.orig[,i]<-unique(na.omit(x.orig[,i]))[x.imp[,i]]
      else
        x.orig[is.na(x.orig[,i]),i] <- x.imp[is.na(x.orig[,i]),i]
    }
  } else {
    x.orig[AMr1.orig]<-x.imp[AMr1.orig]
  }
  new.char <- sapply(x.orig, is.character)
  char.match <- orig.char!=new.char
  if (sum(char.match)!=0)
    for (i in 1:length(char.match))
      if (char.match[i])
        x.orig[,i]<-as.numeric(x.orig[,i])

  return(x.orig)
}

## Create Starting Values for EM Chain
startval<-function(x,startvals=0,priors=NULL){

  AMp<-ncol(x)
  if (!is.null(priors)) {
    ## fill in prior means
    x[(priors[,2]-1)*nrow(x)+priors[,1]] <- priors[,3]
  }
  if (ncol(as.matrix(startvals)) == AMp+1 && nrow(as.matrix(startvals)) == AMp+1)       #checks for correct size of start value matrix
    if (startvals[1,1]==-1)                                       #checks for the -1 necessary for sweep
      return(startvals)

  thetast<-matrix(0,nrow=AMp+1,ncol=AMp+1)  # Create matrix of zeros
  thetast[row(thetast)==col(thetast)] <- 1  # Create Identity matrix
  thetast[1,1]<-(-1)

  if (startvals==0){                            # Defaults to Identity if too few rows fully observed
    cmpr<-packr(x)
    if (nrow(cmpr)>AMp){
      means<-colMeans(cmpr)
      if (all(eigen(cov(cmpr))$values > 10*.Machine$double.eps)) {   #Checks for positive definiteness (positive eigenvalues)
        thetast[2:(AMp+1),2:(AMp+1)]<-cov(cmpr)                   #.Machine$double.eps instead of 0 to account for rounding.
        thetast[2:(AMp+1),1]<-means
        thetast[1,2:(AMp+1)]<-means
      }
    }
  }
  return(thetast)
}

## Create certain indicies.  Only needs to be called once, not every pattern.
## o,m,icap come from omiindxs
## ivector is i from indexm
indxs<-function(x){

  AMn<-nrow(x)
  AMr1<-is.na(x)       # True if missing.
  AMr2<-unique(AMr1)
  o<- !AMr2            # (or o<-AMr2==1)  Maybe == is not robust to small fluctuations
  m<- AMr2             # so put in check procedure (m<-)

  ## The following can be replaced by fortran .dll, although this has only moderate time savings ##
  ivector<-1
  for(i in 2:AMn){
    ischange<- !identical(AMr1[i,],AMr1[i-1,])
    if(ischange){
      ivector<-c(ivector,i)
    }
  }
  ivector<-c(ivector,AMn+1)
#####################################################

  ##  ivector<-.Fortran("indxs",1*AMr1,as.integer(AMn),as.integer(ncol(x)),as.integer(nrow(AMr2)+1),ivector=integer(nrow(AMr2)+1))$ivector

  icap<-vector(mode="list",nrow(AMr2))                     # This is a useful index, although no longer currently used
  for (i in 2:length(ivector)){
    icap[[i]]<-seq(ivector[i-1],ivector[i]-1)
  }

  return(list(AMr1=AMr1,AMr2=AMr2,o=o,m=m,icap=icap,ivector=ivector))
}

## Sweep function (NOTE does not take sign as sign to reverse sweep)
amsweep<-function(g,m,reverse=FALSE){
  if (identical(m,vector(mode='logical',length=length(m)))) # This is check for sweeping on no elements
    {return(g)} else {
      p<-nrow(g)
      rowsm<-sum(m)

                                        # Add Checks of Inputs About Here

      if (rowsm==p){
        h<-solve(g)
        h<-(-h)
      } else {
        kseq<-1:p
        k<-kseq[m]
        kcompl<-kseq[-k]     # we could do everything with m and !m, but only with small numbers of variables
        g11<-g[k,k,drop=FALSE]          # can not subset matricies with long logical vectors
        g12<-g[k,kcompl, drop=FALSE]
        g21<-t(g12)
        g22<-g[kcompl,kcompl , drop=FALSE]

        ## this doesn't actually save us much time.
        #h11a <- try(am.inv(a = g11), silent = TRUE)  # This fails from R 2.15 because of change to LAPACK.
        h11a<-try(solve(g11),silent=TRUE)
        if (inherits(h11a,"try-error")) {
          h11a<-mpinv(g11)     # This is where significant time is spent!
                                        # About as much time as in the rest of the EM
                                        # step
        }
        h11<-as.matrix((-h11a))
        if (reverse) {sgn2<- -1} else {sgn2<- 1}
        h12<-as.matrix(sgn2 * (h11a %*% g12))
        h21<-as.matrix(t(h12))
        h22<-g22-(g21 %*% h11a %*% g12)

        hwo<-rbind(cbind(h11,h12),cbind(h21,h22))
        xordering<-c(k,kcompl)
        h<-matrix(0,p,p)
        h[xordering,xordering]<-hwo
      }
      return(h)
    }
}

## EM chain architecture calls
emarch<-function(x,p2s=TRUE,thetaold=NULL,startvals=0,tolerance=0.0001,priors=NULL,empri=NULL,frontend=FALSE,collect=FALSE,allthetas=FALSE,autopri=0.05,emburn=c(0,0)){
  if (p2s == 2) {
    cat("setting up EM chain indicies\n")
    flush.console()
  }

  iter.hist<-matrix(0,nrow=1,ncol=3)
  if (sum(complete.cases(x)) < nrow(x)){          # Check for fully observed data

    if (identical(thetaold,NULL)) thetaold<-startval(x,startvals=startvals,priors=priors)
    indx<-indxs(x)                      # This needs x.NA
    if (!identical(priors,NULL)){
      priors[,4]<-1/priors[,4]          # change sd to 1/var
      priors[,3]<-priors[,3]*priors[,4] # get the precision-weighted
                                        # mus
      priors <- priors[order(priors[,1],priors[,2]),,drop = FALSE]

    }

    x[is.na(x)]<-0                      # Change x.NA to x.0s
    AM1stln<-sum(indx$m[1,])==0 & nrow(indx$m) > 1
    count<-0
    diff<- 1+tolerance
    AMr1 <- 1 * indx$AMr1
    oo <- 1 * indx$o
    mm <- 1 * indx$m
    if (is.null(empri)) {
      empri <- 0
    }
    theta <- .Call("emcore", x, AMr1, oo, mm,
                   indx$ivector, thetaold, tolerance, emburn, p2s,
                   empri,autopri, allthetas, priors, PACKAGE="Amelia")
    if (!exists("theta")) {
      cat('hiiiiiiii')
    }
  } else {
    if (p2s) cat("\n","No missing data in bootstrapped sample:  EM chain unnecessary")
    pp1<-ncol(x)+1                       # p (the number of variables) plus one
    means<-colMeans(x)
    thetanew<-matrix(0,pp1,pp1)
    thetanew[1,1]<-(-1)
    thetanew[2:pp1,1]<-means
    thetanew[1,2:pp1]<-means
    thetanew[2:pp1,2:pp1]<-cov(x)              # Need to consider Priors in these cases,
    iter.hist<-NA                              # Although not
                                        # currently necessary.
    return(list(thetanew=thetanew,iter.hist=iter.hist))
  }
  return(list(thetanew=theta$theta,iter.hist=theta$iter.hist))
}

## Draw imputations for missing values from a given theta matrix
amelia.impute<-function(x,thetareal,priors=NULL,bounds=NULL,max.resample=NULL){

  indx<-indxs(x)                      # This needs x.NA
  if (!identical(priors,NULL)){
    priors[,4]<-1/priors[,4]
    priors[,3]<-priors[,3]*priors[,4]
  }



  x[is.na(x)]<-0                      # Change x.NA to x.0s
  AM1stln<-sum(indx$m[1,])==0 & nrow(indx$m) > 1  # Create sundry simple indicators
  i<-indx$ivector
  iii<-indx$icap
  AMp<-ncol(x)
  AMn<-nrow(x)
  AMr1 <- 1 * indx$AMr1
  oo <- 1 * indx$o
  mm <- 1 * indx$m
  if (is.null(bounds)) max.resample <- NULL
  imp <- .Call("ameliaImpute", x, AMr1, oo, mm, indx$ivector, thetareal,
                  priors, bounds, max.resample, PACKAGE="Amelia")
  return(imp)

  ## I <- diag(1,AMp)                    # A reference identity matrix for speed

  ## xplay<-matrix(0,nrow=AMn,ncol=AMp)
  ## if (!AM1stln){
  ##   st<-1
  ## } else {
  ##   xplay[i[1]:(i[2]-1),]<-x[i[1]:(i[2]-1),]
  ##   st<-2
  ## }

  ## if (identical(priors,NULL) | !identical(priors,NULL)){                     # No Observation Level Priors in Dataset

  ##   for (ss in st:(length(i)-1)){

  ##     theta<-amsweep(thetareal,c(FALSE,o[ss,]))

  ##     is<-i[ss]
  ##     isp<-i[ss+1]-1

  ##     Ci<-matrix(0,AMp,AMp)
  ##     hold<-chol(theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
  ##     Ci[m[ss,],m[ss,]]<-hold


  ##     imputations<-AMr1[is:isp, , drop=FALSE] * ((x[is:isp, , drop=FALSE] %*% theta[2:(AMp+1),2:(AMp+1) , drop=FALSE])
  ##                                   + (matrix(1,1+isp-is,1) %*% theta[1,2:(AMp+1) , drop=FALSE]) )

  ##     if (!identical(bounds,NULL)) {
  ##       xplay[is:isp,] <- am.resample(x.ss=x[is:isp,,drop=FALSE], ci=Ci, imps=imputations,
  ##                                     m.ss=m[ss,], bounds=bounds,
  ##                                     max.resample=max.resample)
  ##     } else {
  ##       junk<-matrix(rnorm((i[ss+1]-is) * AMp), i[ss+1]-is, AMp) %*% Ci
  ##       xplay[is:isp,]<-x[is:isp,,drop=FALSE] + imputations + junk
  ##     }
  ##   }

  ## } else {                                    # Observation Level Priors Used

  ##   for (ss in st:(length(i)-1)){

  ##     theta<-amsweep(thetareal,c(FALSE,o[ss,]))

  ##     is<-i[ss]
  ##     isp<-i[ss+1]-1


  ##     imputations<- AMr1[is:isp, , drop=FALSE] * ((x[is:isp, , drop=FALSE] %*%
  ##                                    theta[2:(AMp+1),2:(AMp+1) , drop=FALSE]) + (matrix(1,1+isp-is,1) %*%
  ##                                                                  theta[1,2:(AMp+1) , drop=FALSE]) )

  ##     ## get the prior rows and non-prior rows
  ##     priorsinpatt <- which(is:isp %in% priors[,1])

  ##     nopri <- !(is:isp %in% priors[,1])


  ##     hasPrior <- priors[,1] %in% is:isp
  ##     priorsForPatt <- priors[hasPrior, , drop = FALSE]
  ##     priorsForPatt <- priorsForPatt[order(priorsForPatt[,1],priorsForPatt[,2]), , drop = FALSE]

  ##     numRowsWithPrior <- length(unique(priorsForPatt[,1]))
  ##     mu.prior <- matrix(0, nrow = sum(m[ss,]), ncol = numRowsWithPrior)

  ##     ## create one large matrix to house all the lambdas

  ##     solve.Lambda <- array(0, c(sum(m[ss,]), sum(m[ss,]), numRowsWithPrior))


  ##     colsWithPriors <- tapply(priorsForPatt[,2], priorsForPatt[,1],
  ##                              function(x) (1:AMp %in% x)[m[ss,]])

  ##                                       #      colsWithPriors <- colsWithPriors[rep(m[ss,], times = length(priorsinpatt))]

  ##     inds <- cbind(rep(1:sum(m[ss,]),numRowsWithPrior),
  ##                   rep(1:sum(m[ss,]),numRowsWithPrior),
  ##                   sort(rep(1:numRowsWithPrior,sum(m[ss,]))))


  ##     diag.Lambda <- rep(0, sum(m[ss,])*numRowsWithPrior)
  ##     diag.Lambda[unlist(colsWithPriors)] <- priorsForPatt[,4]
  ##     mu.prior[unlist(colsWithPriors)] <- priorsForPatt[,3]
  ##     solve.Lambda[inds] <- diag.Lambda


  ##     solve.Sigma  <- am.inv(theta[c(FALSE,m[ss,]),
  ##                                  c(FALSE,m[ss,]),drop=FALSE])

  ##     junk <- matrix(0,nrow(imputations),AMp)
  ##     ## we should only try to fill non-prior cases when they actually
  ##     ## exist in the patter of missingness
  ##     if (sum(nopri) > 0) {
  ##       Ci<-matrix(0,AMp,AMp)
  ##       hold<-chol(theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
  ##       Ci[m[ss,],m[ss,]]<-hold


  ##       if (!identical(bounds,NULL)) {
  ##         xplay[(is:isp)[nopri],] <- am.resample(x.ss=x[(is:isp)[nopri],,drop=FALSE], ci=Ci,
  ##                                                imps=imputations[nopri,,drop=FALSE],
  ##                                                m.ss=m[ss,], bounds=bounds,
  ##                                                max.resample=max.resample)
  ##       } else {
  ##         junk[nopri,] <- matrix(rnorm(sum(nopri)*AMp), sum(nopri), AMp) %*% Ci
  ##         xplay[(is:isp)[nopri],]<-x[(is:isp)[nopri],,drop=FALSE] +
  ##           imputations[nopri,,drop=FALSE] + junk[nopri,,drop=FALSE]
  ##       }
  ##     }

  ##     if (length(priorsinpatt) == 0) next()

  ##     for (jj in 1:length(priorsinpatt)){
  ##       or <- (is:isp)[priorsinpatt[jj]]  ## original rows
  ##       rowInPatt <- priorsinpatt[jj]

  ##       ##priorsForThisRow <- priors[priors[,1] == or, , drop = FALSE]
  ##       ##priorsForThisRow <- priorsForThisRow[order(priorsForThisRow[,2]),,drop=FALSE]
  ##       ##columnsWithPriors <- c(1:AMp) %in% priorsForThisRow[, 2]

  ##       ##npr <- nrow(priorsForThisRow)

  ##                                       # Calculate sd2
  ##       ##solve.Sigma  <- am.inv(theta[c(FALSE,m[ss,]),
  ##       ##                             c(FALSE,m[ss,]),drop=FALSE])

  ##       ##solve.Lambda <- matrix(0,sum(m[ss,]),sum(m[ss,]))
  ##       ##diag.Lambda <- rep(0, sum(m[ss,]))
  ##       ##diag.Lambda[columnsWithPriors[m[ss,]]] <- priorsForThisRow[,4]
  ##       ##diag(solve.Lambda) <- diag.Lambda

  ##       wvar <- am.inv(solve.Lambda[,,jj] + solve.Sigma)

  ##                                       # Weight these together
  ##       ##mu.prior <- rep(0, sum(m[ss,]))
  ##       ##mu.prior[columnsWithPriors[m[ss,]]] <- priorsForThisRow[,3]
  ##       mu.miss <- (wvar) %*% (mu.prior[,jj] +
  ##                              solve.Sigma  %*% imputations[rowInPatt,m[ss,]])

  ##       imputations[rowInPatt,m[ss,]] <- mu.miss

  ##                                       # update **theta**
  ##       copy.theta <- theta
  ##       copy.theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])] <- wvar

  ##       ## Create "noise" term from updated theta
  ##       Ci<-matrix(0,AMp,AMp)
  ##       hold<-chol(copy.theta[c(FALSE,m[ss,]),c(FALSE,m[ss,])])
  ##       Ci[m[ss,],m[ss,]]<-hold

  ##                                       # fork for the bounds resampler
  ##       if (!identical(bounds,NULL)) {
  ##         xplay[or,] <- am.resample(x.ss=x[or,,drop=FALSE], ci=Ci, imps=imputations[rowInPatt,,drop=FALSE],
  ##                                   m.ss=m[ss,], bounds=bounds,
  ##                                   max.resample=max.resample)

  ##       } else {
  ##         junk[rowInPatt,]<-matrix(rnorm(AMp), 1, AMp) %*% Ci

  ##                                       # Piece together this observation
  ##         xplay[or,]<-x[or,] + (AMr1[or, , drop=FALSE] * (imputations[rowInPatt,] + junk[rowInPatt,]))
  ##       }
  ##     }
  ##   }
  ## }

  ## return(xplay)
}




## resampler - takes in data with a single miss. pattern and resamples;
##   returns an xplay for this pattern.
am.resample <- function(x.ss, ci, imps, m.ss, bounds, max.resample) {

                                        # create some holders for the bounds
  AMn.ss <- nrow(x.ss)
  AMp    <- ncol(x.ss)
  ub.mat <- matrix(NA, nrow=AMn.ss, ncol=AMp)
  lb.mat <- matrix(NA, nrow=AMn.ss, ncol=AMp)
  xp.ss  <- matrix(0,  nrow=AMn.ss, ncol=AMp)

  junk <- matrix(rnorm(AMn.ss*AMp), AMn.ss, AMp) %*% ci
                                        # b.cols selects the columns that have bounds
  b.cols <- c(1:AMp) %in% bounds[,1]

                                        # patt.bounds selects the rows of 'bounds' that are used in this pattern
  patt.bounds <- bounds[(bounds[,1] %in% c(1:AMp)[m.ss]),,drop=FALSE]

                                        # we only need to redraw if there are missing variables w/ bounds in
                                        # this pattern
  if (length(patt.bounds) > 0) {

                                        # put them in the right order so that we can
    patt.bounds <- patt.bounds[order(patt.bounds[,1]),,drop=FALSE]

                                        # fill the holders (only in the columns that are missing and have bounds)
    lb.mat[,(b.cols & m.ss)] <- t(matrix(patt.bounds[,2],
                                         nrow=nrow(patt.bounds),ncol=AMn.ss))

    ub.mat[,(b.cols & m.ss)] <- t(matrix(patt.bounds[,3],
                                         nrow=nrow(patt.bounds),ncol=AMn.ss))

                                        # create an index of those rows left
    left <- seq(1,AMn.ss)
    samp <- 1

    while ((length(left) > 0) & (samp < max.resample)) {

                                        #if (length(left) == 1)
                                        #browser()
                                        # these are matrices where T means it's in the bound, F means it's out
      utest <- (imps + junk) < ub.mat
      ltest <- (imps + junk) > lb.mat

                                        # this matrix combines the two
      btest <- utest & ltest

                                        # get the failing/passing cells
      fail.cells <- which(!btest, arr.ind=TRUE)
      fail.rows  <- rowSums(!btest, na.rm=TRUE) > 0
      pass.rows  <- rowSums(!btest, na.rm=TRUE) == 0

                                        # record the rows that we have left
      new.left <- left[fail.rows]

      if (sum(pass.rows) > 0) {
        xp.ss[left[pass.rows],] <- x.ss[pass.rows,,drop=FALSE] +
          imps[pass.rows,,drop=FALSE] +
            junk[pass.rows,,drop=FALSE]
      }

      left <- new.left

      junk <- matrix(rnorm(length(left)*AMp), length(left), AMp) %*% ci

      imps   <-  imps[fail.rows,,drop=FALSE]
      ub.mat <- ub.mat[fail.rows,,drop=FALSE]
      lb.mat <- lb.mat[fail.rows,,drop=FALSE]
      x.ss   <-   x.ss[fail.rows,,drop=FALSE]
      samp <- samp+1
    }


                                        # set failing cells to their bounds of the last failure
                                        # this is probably okay, as if the n
    if ((samp==max.resample) && (length(left) > 0)) {
      xp.ss[left,] <- x.ss + imps + junk
      utest <- (imps + junk) < ub.mat
      ltest <- (imps + junk) > lb.mat
      u.fails <- which(!utest, arr.ind=TRUE)
      l.fails <- which(!ltest, arr.ind=TRUE)
      xp.left <- xp.ss[left,,drop=FALSE]
      xp.left[u.fails] <- ub.mat[u.fails]
      xp.left[l.fails] <- lb.mat[l.fails]
      xp.ss[left,] <- xp.left
    }

  } else {
    xp.ss <- x.ss + imps + junk
  }
  return(xp.ss)
}




am.inv <- function(a,tol=.Machine$double.eps) {
  if (length(a)==1)
    return(1/a)
  storage.mode(a) <- "double"
  size <- ncol(a)
  chol2inv(chol.default(a), ncol(a))
}

##
## ameliabind - combines multiple Amelia outputs
##
## INPUTS: >1 amelia output
##
## OUTPUTS: a merged amelia output with all inputted lists
##

ameliabind <- function(...) {
  args <- list(...)

  if (any(!sapply(args, is, "amelia")))
    stop("All arguments must be amelia output.")

  if (length(args) > 1) {
    ## test that data is the same. we'll just compare the missMatrices.
    ## this will allow datasets with the same size and missingness
    ## matrix to be combined unintentionally, but this seems unlikely.
    datacheck <- lapply(args,
                        function(x) isTRUE(identical(x$missMatrix,args[[1]]$missMatrix)))
    if (any(!unlist(datacheck)))
      stop("Non-compatible datasets.")

    ## test that all the arguments are the same
    check <- lapply(args,
                    function(x) isTRUE(identical(x$arguments, args[[1]]$arguments)))
    if (any(!unlist(check)))
      stop("Non-compatible amelia arguments")

    check <- lapply(args,
                    function(x) isTRUE(identical(x$transform.calls,
                                                 args[[1]]$transform.calls)))
    if (any(!unlist(check)))
      stop("Non-compatible transformations on imputed datasets")

    imps <- unlist(lapply(args, function(x) return(x$m)))
    newm <- sum(imps)
    impindex <- c(0,cumsum(imps))

    k <- nrow(args[[1]]$mu)
    out  <- list(imputations = list(),
                 m           = integer(0),
                 missMatrix  = matrix(NA,0,0),
                 overvalues  = args[[1]]$overvalues,
                 theta       = array(NA, dim = c(k+1,k+1,newm) ),
                 mu          = matrix(NA, nrow = k, ncol = newm),
                 covMatrices = array(NA, dim = c(k,k,newm)),
                 code        = integer(0),
                 message     = character(0),
                 iterHist    = list(),
                 arguments   = list(),
                 orig.vars   = args[[1]]$orig.vars)

    out$m <- newm
    out$missMatrix <- args[[1]]$missMatrix
    out$arguments <- args[[1]]$arguments
    out$transform.calls <- args[[1]]$transform.calls
    out$transform.vars <- args[[1]]$trasnform.vars

    ## since code==1 is good and code==2 means we have an NA,
    ## then our new output should inherit a 2 if there are any
    out$code <- max(unlist(lapply(args,function(x) return(x$code))))

    if (out$code > 2)
      stop("Amelia output contains error.")
    if (out$code==2)
      out$message <- "One or more of the imputations resulted in a covariance matrix that was not invertible."
    else
      out$message <- "Normal EM convergence"

    for (i in 1:length(args)) {
      currimps <- (impindex[i]+1):impindex[i+1]
      out$mu[,currimps] <- args[[i]]$mu
      out$theta[,,currimps] <- args[[i]]$theta
      out$covMatrices[,,currimps] <- args[[i]]$covMatrices
      out$imputations <- c(out$imputations, args[[i]]$imputations)
      out$iterHist    <- c(out$iterHist, args[[i]]$iterHist)

    }
    class(out) <- "amelia"
    class(out$imputations) <- c("mi","list")
  } else {
    out <- args[[1]]
  }
  return(out)
}

getOriginalData <- function(obj) {
  data <- obj$imputations[[1]]
  is.na(data) <- obj$missMatrix
  data <- data[, obj$orig.vars]
  oi <- obj$arguments$overimp
  if (!is.null(oi)) {
    for (i in 1:nrow(oi)) {
      data[oi[i,1], oi[i,2]] <- obj$overvalues[i]
    }
  }
  return(data)
}

remove.imputations <- function(obj) {
  data <- obj$imputations[[1]]
  is.na(data) <- obj$missMatrix
  oi <- obj$arguments$overimp
  if (!is.null(oi)) {
    for (i in 1:nrow(oi)) {
      data[oi[i,1], oi[i,2]] <- obj$overvalues[i]
    }
  }
  return(data)
}

## amelia - multiple imputation. core function
##

amelia <- function(x, ...) {
  UseMethod("amelia", x)
}

amelia.amelia <- function(x, m = 5, p2s = 1, frontend = FALSE, ...) {

  ## The original data is the imputed data with the
  ## imputations marked to NA. These two lines do that
  data <- x$imputations[[1]]

  ## Only the variables in the missMatrix should be passed. This is
  ## because the others are
  data <- getOriginalData(x)

  out <- amelia.default(x = data, m = m, arglist=x$arguments, p2s=p2s,
                        frontend = frontend, incheck=FALSE)
  num.tcalls <- length(x$transform.calls)
  if (num.tcalls > 0) {
    for (i in 1:num.tcalls) {
      tcall <- x$transform.calls[[i]]
      tcall[[2]] <- as.name("out")
      out <- eval(tcall)
    }
    out$transform.calls <- x$transform.calls
  }
  ret <- ameliabind(x, out)
  return(ret)
}


amelia.molist <- function(x, ...) {
  m <- match.call(expand.dots=TRUE)
  m$x <- x$data
  m$priors <- x$priors
  m$overimp <- x$overimp
  m[[1]] <- as.name("amelia.default")
  ret <- eval(m)

  return(ret)
}

amelia.default <- function(x, m = 5, p2s = 1, frontend = FALSE, idvars=NULL,
                           ts=NULL,cs=NULL,polytime=NULL,splinetime=NULL,intercs=FALSE,
                           lags=NULL,leads=NULL,startvals=0,tolerance=0.0001,
                           logs=NULL,sqrts=NULL,lgstc=NULL,noms=NULL,ords=NULL,
                           incheck=TRUE,collect=FALSE,arglist=NULL,
                           empri=NULL,priors=NULL,autopri=0.05,
                           emburn=c(0,0),bounds=NULL,max.resample=100,
                           overimp = NULL,
                           parallel = c("no", "multicore", "snow"),
                           ncpus = getOption("amelia.ncpus", 1L), cl = NULL, ...) {

  ## parellel infrastructure modeled off of 'boot' package
  if (missing(parallel)) parallel <- getOption("amelia.parallel", "no")
  parallel <- match.arg(parallel)
  have_mc <- have_snow <- FALSE
  if (parallel != "no" && ncpus > 1L) {
    if (parallel == "multicore") have_mc <- .Platform$OS.type != "windows"
    else if (parallel == "snow") have_snow <- TRUE
    if (!have_mc && !have_snow) ncpus <- 1L
    if (p2s == 2) {
      cat("\nUsing '", parallel, "' parallel backend with", ncpus, "cores.")
    }
  }


  if (p2s==2) {
    cat("\namelia starting\n")
    flush.console()
  }
  am.call <- match.call(expand.dots = TRUE)
  archv <- am.call

  prepped<-amelia.prep(x=x,m=m,idvars=idvars,empri=empri,ts=ts,cs=cs,
                       tolerance=tolerance,polytime=polytime,splinetime=splinetime,
                       lags=lags,leads=leads,logs=logs,sqrts=sqrts,lgstc=lgstc,
                       p2s=p2s,frontend=frontend,intercs=intercs,
                       noms=noms,startvals=startvals,ords=ords,incheck=incheck,
                       collect=collect,
                       arglist=arglist,priors=priors,autopri=autopri,bounds=bounds,
                       max.resample=max.resample,overimp=overimp)

  if (prepped$code!=1) {
    cat("Amelia Error Code: ",prepped$code,"\n",prepped$message,"\n")
    return(invisible(list(code=prepped$code,message=prepped$message)))
  }


  do.amelia <- function(X,...) {

    if (p2s==2) {
      cat("running bootstrap\n")
    }
    k <- ncol(prepped$x)
    if (!is.null(colnames(x))) {
      ovars <- colnames(x)
    } else {
      ovars <- 1:k
    }

    code <- 1

    impdata <- list(imputations = list(),
                    m           = 1,
                    missMatrix  = prepped$missMatrix,
                    overvalues  = prepped$overvalues,
                    theta       = array(NA, dim = c(k+1,k+1,1) ),
                    mu          = matrix(NA, nrow = k, ncol = 1),
                    covMatrices = array(NA, dim = c(k,k,1)),
                    code        = integer(0),
                    message     = character(0),
                    iterHist    = list(),
                    arguments   = list(),
                    orig.vars   = ovars)
    class(impdata) <- "amelia"
    class(impdata$imputations) <- c("mi","list")

    x.boot<-bootx(prepped$x,prepped$priors)
    x.stacked<-amstack(x.boot$x,colorder=FALSE,x.boot$priors)   # Don't reorder columns thetanew will not align with d.stacked$x

    if (p2s) cat("-- Imputation", X, "--\n")

    thetanew <- emarch(x.stacked$x, p2s = p2s, thetaold = NULL,
                       tolerance = tolerance, startvals = startvals,
                       priors = x.stacked$priors, empri = empri,
                       frontend = frontend, collect = collect,
                       autopri = prepped$autopri, emburn = emburn)

    ##thetanew <- .Call("emarch", PACKAGE = "Amelia")
    ## update the amelia ouptut
    impdata$iterHist[[1]]    <- thetanew$iter.hist
    impdata$theta[,,1]       <- thetanew$thetanew
    impdata$mu[,1]           <- thetanew$thetanew[-1,1]
    impdata$covMatrices[,,1] <- thetanew$thetanew[-1,-1]
    dimnames(impdata$covMatrices)[[1]] <- prepped$theta.names
    dimnames(impdata$covMatrices)[[2]] <- prepped$theta.names
    dimnames(impdata$mu)[[1]] <- prepped$theta.names

    if
    (any(eigen(thetanew$thetanew[2:nrow(thetanew$thetanew),2:ncol(thetanew$thetanew)], only.values=TRUE, symmetric=TRUE)$values < .Machine$double.eps)) {
      impdata$imputations[[1]] <- NA
      impdata$code <- 2
      cat("\n\nThe resulting variance matrix was not invertible.",
          "  Please check your data for highly collinear variables.\n\n")
      return(impdata)
    }

    ximp <- amelia.impute(prepped$x, thetanew$thetanew, priors = prepped$priors,
                          bounds = prepped$bounds, max.resample)
    ximp <- amunstack(ximp, n.order = prepped$n.order,
                      p.order = prepped$p.order)
    ximp <- unscale(ximp, mu = prepped$scaled.mu, sd = prepped$scaled.sd)
    ximp <- unsubset(x.orig = prepped$trans.x, x.imp = ximp,
                     blanks = prepped$blanks, idvars = prepped$idvars,
                     ts = prepped$ts, cs = prepped$cs, polytime = polytime,
                     splinetime = splinetime, intercs = intercs,
                     noms = prepped$noms, index = prepped$index,
                     ords = prepped$ords)
    ximp <- untransform(ximp, logs = prepped$logs, xmin = prepped$xmin,
                        sqrts = prepped$sqrts, lgstc = prepped$lgstc)

    if (p2s==2) {
      cat("\n saving and cleaning\n")
    }

    ## here we deal with the imputed matrix.

    ## first, we put the data into the output list and name it
    impdata$imputations[[1]] <- impfill(x.orig = x, x.imp = ximp,
                                        noms = prepped$noms,
                                        ords = prepped$ords, priors = priors)

    if (p2s) cat("\n")
    if (frontend) {
      tcl(getAmelia("runAmeliaProgress"), "step",(100/m -1))
    }

    impdata$code <- code
    impdata$arguments <- prepped$archv
    names(impdata$imputations) <- paste("imp", X, sep = "")
    class(impdata$arguments) <- c("ameliaArgs", "list")

    return(impdata)
  }

  ## parallel infrastructure from the 'boot' package
  impdata <- if (ncpus > 1L && (have_mc || have_snow)) {
    if (have_mc) {
      parallel::mclapply(seq_len(m), do.amelia, mc.cores = ncpus)
    } else if (have_snow) {
      list(...) # evaluate any promises
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
        if(RNGkind()[1L] == "L'Ecuyer-CMRG")
          parallel::clusterSetRNGStream(cl)
        res <- parallel::parLapply(cl, seq_len(m), do.amelia)
        parallel::stopCluster(cl)
        res
      } else parallel::parLapply(cl, seq_len(m), do.amelia)
    }
  } else lapply(seq_len(m), do.amelia)

  if (all(sapply(impdata, is, class="amelia"))) {
    if (!all(sapply(impdata, function(x) is.na(x$imputations)))) {
      impdata <- do.call(ameliabind, impdata)
      if (impdata$code == 2) {
        impdata$message <- paste("One or more of the imputations resulted in a",
                                 "covariance matrix that was not invertible.")
      } else {
        impdata$message <- paste("Normal EM convergence.")
      }
    } else {
      impdata$code <- 2
      impdata$message <- paste("All of the imputations resulted in a covariance",
                               "matrix that is not invertible.")
    }
  }


  return(impdata)
}
