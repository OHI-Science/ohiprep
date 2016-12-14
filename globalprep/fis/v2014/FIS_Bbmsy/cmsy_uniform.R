#######################################
## CMSY script to obtain b/bmsy data

runCMSY<-function(cdat, stockNumber){ 
  #stockNumber <- 1 #MRF addition
  stock_id<-unique(cdat$stock_id)
  ##
  seed<-ceiling(runif(1,0,1e6))
  set.seed(seed)
  ##
  
  yr   <- cdat$yr[as.character(cdat$stock)==stock_id[stockNumber]]
  ct   <- as.numeric(cdat$ct[as.character(cdat$stock)==stock_id[stockNumber]])/1000  ## assumes that catch is given in tonnes, transforms to '000 tonnes
  res  <- unique(as.character(cdat$res[as.character(cdat$stock)==stock_id[stockNumber]])) ## resilience from FishBase, if needed, enable in PARAMETER SECTION
  ##
  method_id<-"CMSY"
  #res  <- unique(as.character(cdat$res[as.character(cdat$stock)==stock])) ## resilience from FishBase, if needed, enable in PARAMETER SECTION
  nyr  <- length(yr)    ## number of years in the time series
  
  ### MRF: mistake in here...forgot to include: ct[length(ct)-1], I added in, but might want to check
  meanct<-(ct[length(ct)-4]+ct[length(ct)-3]+ct[length(ct)-2]+ct[length(ct)-1]+ct[length(ct)])/5 #calculate mean over last 5 years  
  ## PARAMETER SECTION
  # R prior
  start_r     <- c(0.01,10)  ## disable this line if you use resilience
  start_k     <- c(max(ct),50*max(ct)) ## default for upper k e.g. 50 * max catch         ****THIS IS AN UPDATE FROM RAINER--Oct 12, 2012    
  startbio    <- if(ct[1]/max(ct) < 0.2) {c(0.5,0.9)} else {c(0.2,0.6)} ## use for batch processing   ****THIS IS AN UPDATE FROM RAINER--Oct 12, 2012  
  interyr     <- yr[2]   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available
  interbio     <- c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
  finalbio    <- c(0.01,0.7) ## THIS IS AN UPDATE FROM Kristin Jul 29 2014
  n           <- 1e5  ## number of iterations
  sigR        <- 0.0   ## process error; 0 if deterministic model; 0.05 reasonable value? 0.2 is too high
  startbt     <- seq(startbio[1], startbio[2], by = 0.05) ## apply range of start biomass in steps of 0.05  
  parbound    <- list(r = start_r, k = start_k, lambda = finalbio, sigR) 
  
  ## FUNCTIONS
  .schaefer  <- function(theta)
  {
    with(as.list(theta), {  ## for all combinations of ri & ki
      bt=0
      ell = 0  ## initialize ell
      J=0
      for (j in startbt)
      {
        if(ell == 0) 
        {
          bt[1]=j*k*exp(rnorm(1,0, sigR))  ## set biomass in first year
          for(i in 1:nyr) ## for all years in the time series
          {
            xt=rnorm(1,0, sigR)
            bt[i+1]=(bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
          }                
          ##Bernoulli likelihood, assign 0 or 1 to each combination of r and k
          ell = 0
          if(bt[nyr+1]/k>=lam1 && bt[nyr+1]/k <=lam2 && min(bt) > 0 && max(bt) <=k && bt[which(yr==interyr)]/k>=interbio[1] && bt[which(yr==interyr)]/k<=interbio[2]) 
            ell = 1
          J=j
        }              
      }
      return(list(ell=ell, J=J))
    })
  }
  ##
  sraMSY <- function(theta, N){
    ##This function conducts the stock reduction
    ##analysis for N trials
    ##args:
    ##	theta - a list object containing:
    ##		r (lower and upper bounds for r)
    ##		k (lower and upper bounds for k)
    ##		lambda (limits for current depletion)
    ##
    #theta <- parbound #mel addition, testing
    #N=n #mel addition, testing
    with(as.list(theta), 
{
  ri = exp(runif(N, log(r[1]), log(r[2])))  ## get N values between r[1] and r[2], assign to ri
  ki = exp(runif(N, log(k[1]), log(k[2])))  ## get N values between k[1] and k[2], assing to ki
  itheta=cbind(r=ri,k=ki, lam1=lambda[1],lam2=lambda[2], sigR=sigR) ## assign ri, ki, and final biomass range to itheta
  #test <- data.frame(r=4.51446027, k=13.818701, lam1=0.01, lam2=0.40, sigR=0) #mel addition
  #apply(test, 1, .schaefer) #mel addition
  M = apply(itheta,1,.schaefer) ## call Schaefer function with parameters in itheta
  i=1:N
  ## prototype objective function
  get.ell=function(i) M[[i]]$ell 
  ell = sapply(i, get.ell) 
  get.J=function(i) M[[i]]$J
  J=sapply(i, get.J)
  return(list(r=ri,k=ki, ell=ell, J=J)	)
})
  }
  
  
  ## MAIN
  Time = system.time(
    R1<-sraMSY(parbound, n) 
  )
  ## Get statistics on r, k, MSY and determine new bounds for r and k
  r1 	<- R1$r[R1$ell==1]
  k1 	<- R1$k[R1$ell==1]
  j1  <- R1$J[R1$ell==1]
  msy1  <- r1*k1/4
  mean_msy1 <- exp(mean(log(msy1))) 
  max_k1a  <- min(k1[r1<1.1*parbound$r[1]]) ## smallest k1 near initial lower bound of r
  max_k1b  <- max(k1[r1*k1/4<mean_msy1]) ## largest k1 that gives mean MSY
  max_k1 <- if(max_k1a < max_k1b) {max_k1a} else {max_k1b}
  ##
  if(length(r1)<10) {
    cat("Too few (", length(r1), ") possible r-k combinations, check input parameters","\n")
    flush.console()
  }
  ##
  if(length(r1)>=10) {
    ## set new upper bound of r to 1.2 max r1
    parbound$r[2] <- 1.2*max(r1)
    ## set new lower bound for k to 0.9 min k1 and upper bound to max_k1 
    parbound$k 	  <- c(0.9 * min(k1), max_k1)    
    #     
    #     cat("First MSY =", format(1000*mean_msy1, digits=3),"\n")
    #     cat("First r =", format(exp(mean(log(r1))), digits=3),"\n")
    #     cat("New upper bound for r =", format(parbound$r[2],digits=2),"\n")	
    #     cat("New range for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")
    #     
    #     
    ## Repeat analysis with new r-k bounds
    R1 = sraMSY(parbound, n)
    ## Get statistics on r, k and msy
    r = R1$r[R1$ell==1]
    k = R1$k[R1$ell==1]
    j = R1$J[R1$ell==1]
    msy = r * k / 4
    mean_ln_msy = mean(log(msy))
    ##
    
    BT=0
    getBiomass  <- function(r, k, j)
    {
      bt=vector()
      for (v in 1:length(r))
      {
        #v <- 1 #mel addition, testing
        bt[1]=j[v]*k[v]*exp(rnorm(1,0, sigR))  ## set biomass in first year. For: exp(rnorm(1,0,sigR)) equals 1 if sigR is 0
        for(i in 1:nyr) ## for all years in the time series
        {
          #i <- 1 #mel addition, testing
          xt=rnorm(1,0, sigR)
          bt[i+1]=(bt[i]+r[v]*bt[i]*(1-bt[i]/k[v])-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
        }
        BT=rbind(BT, t(t(bt)))        
      }
      return(BT)
    }
    R2<-getBiomass(r, k, j) 
    R2<-R2[-1,]
    runs<-rep(1:length(r), each=nyr+1)
    count<-rep(1:(nyr+1), length=length(r)*(nyr+1))
    runs<-t(runs)
    count<-t(count)
    R3<-cbind(as.numeric(runs), as.numeric(count), stock_id[stockNumber], as.numeric(R2) )
    ##R4<-data.frame(R3)
    ## CM: changed this, as otherwise biomass is the
    ## level of the factor below
    R4<-data.frame(R3, stringsAsFactors=FALSE)
    ##
    names(R4)<-c("Run", "Count", "Stock","Biomass")
    #B0x<-R4$Biomass[R4$Count==1] # / j [for each sample]
    #B0_x<-as.numeric(paste(B0x))
    B0_x <- k
    Bmsy_x<-B0_x*0.5
    Run<-c(1:length(r)) 
    BMSY<-cbind(Run, Bmsy_x)
    R5<-merge(R4, BMSY, by="Run", all.x=T, all.y=F)
    R5$B_Bmsy<-as.numeric(paste(R5$Biomass))/R5$Bmsy_x
    ## CM, changed to get quantiles
    ##R6<-aggregate(log(B_Bmsy)~as.numeric(Count)+Stock, data=R5, mean) # WHY NOT GETTING THE QUANTILES DIRECTLY FROM R5?
    R6<-aggregate(log(B_Bmsy)~as.numeric(Count)+Stock, data=R5, FUN=function(z){c(mean=mean(z),sd=sd(z),upr=exp(quantile(z, p=0.975)), lwr=exp(quantile(z, p=0.025)), lwrQ=exp(quantile(z, p=0.25)), uprQ=exp(quantile(z, p=0.75)))})
    ##R6<-aggregate(B_Bmsy~as.numeric(Count)+Stock, data=R5, FUN=function(z){c(mean=mean(z),sd=sd(z),upr=exp(quantile(z, p=0.975)), lwr=exp(quantile(z, p=0.025)))})
    ## CM: sort out columns
    R6<-data.frame(cbind(R6[,1:2],R6[,3][,1],R6[,3][,2],R6[,3][,3],R6[,3][,4],R6[,3][,5], R6[,3][,6]))
    names(R6)<-c("Count", "Stock", "BoverBmsy", "BoverBmsySD","BoverBmsyUpper","BoverBmsyLower","BoverBmsylwrQ","BoverBmsyuprQ")      ## CM: remove  last entry as it is 1 greater than number of years
    ## CHECK WITH KRISTIN!---Kristin: commented out line 209 and I'm getting plots that look reasonable
    ## CM: 19th June 2013 removed final year here for ease of dataframe output below
    R6<-R6[-length(R6),]
    ## CM: geometric mean
    R6$GM_B_Bmsy<-exp(R6$BoverBmsy)
    ## CM: arithmetic mean
    R6$M_B_Bmsy<-exp(R6$BoverBmsy+R6$BoverBmsySD^2/2)
    ##
    R6<-R6[order(R6[,1]), ]
    ##BoverBmsy<-R6$GM_B_Bmsy
    ## CM: note using arithmetic mean
    BoverBmsy<-R6$M_B_Bmsy
    b_bmsyUpper<-R6$BoverBmsyUpper
    b_bmsyLower<-R6$BoverBmsyLower
    BoBtest<-data.frame(BoverBmsy)
    b_bmsylwrQ<-R6$BoverBmsylwrQ
    b_bmsyuprQ<-R6$BoverBmsyuprQ
    #Calculate alternative upper and lower bounds on B/Bmsy for comparison (based on
    #the fact that a Schaefer production model will suggest the equilibrium biomass B/Bmsy 
    #that will eventually result from a constant relative catch Y/MSY as: B/Bmsy = 1+/-sqrt(1-catch/MSY)).
    #Additional evidence is needed to decide whether the biomass is above or below Bmsy.
    #BoverBmsy2upper<-1+sqrt(1-ct/exp(mean(log(msy))))# WHY NOT GETTING THE QUANTILES DIRECTLY FROM R5?
    #BoverBmsy2lower<-1-sqrt(1-ct/exp(mean(log(msy))))
    ##
    effective_sample_size<-length(r)
    convergence<-ifelse(effective_sample_size<30, "NC", 
                        ifelse(effective_sample_size>200, "SC", "WC"))
    ##outputSIM = list("stock_id"=stock, "b_bmsy"=BoverBmsy, "b_bmsyUpper"=apply(BoBtest,MARGIN=2,FUN=quantile,prob=0.975), "b_bmsyLower"=apply(BoBtest,MARGIN=2,FUN=quantile,prob=0.025), "b_bmsy_iq25"=apply(BoBtest,MARGIN=2,FUN=quantile,prob=0.25), "b_bmsy_iq75"=apply(BoBtest,MARGIN=2,FUN=quantile,prob=0.75), "b_bmsy2U"=BoverBmsy2upper, "b_bmsy2L"=BoverBmsy2lower, "year"=yr, "seed"=seed, "convergence"=convergence, "n_iterations"=n, "effective_sample_size"=effective_sample_size, "run_time"=Time['elapsed'], "method_id"=method_id, r_params=r1, k_params=k1)
    ## CM: changed the output to reflect changes above, please check all
    outputSIM = list("stock_id"=stock_id[stockNumber], "b_bmsy"=BoverBmsy, "b_bmsyUpper"=b_bmsyUpper, "b_bmsyLower"=b_bmsyLower, "b_bmsy_iq25"=b_bmsylwrQ, "b_bmsy_iq75"=b_bmsyuprQ, "year"=yr, "seed"=seed, "convergence"=convergence, "n_iterations"=n, "effective_sample_size"=effective_sample_size,  "method_id"=method_id, r_params=r1, k_params=k1)
  }else{
    outputSIM = list("stock_id"=stock_id[stockNumber], "b_bmsy"=NA, "b_bmsyUpper"=NA, "b_bmsyLower"=NA, "b_bmsy_iq25"=NA, "b_bmsy_iq75"=NA, "year"=yr, "seed"=seed, "convergence"="NC", "n_iterations"=n, "effective_sample_size"=effective_sample_size,  "method_id"=method_id, r_params=r1, k_params=k1)
  }
  
  
  ##
  ##res.list<-list(outputSIM)
  ## not elegant naming of results list below
  ## but need to have name somewhere in list names 
  ##names(res.list)<-stock_id[stockNumber]
  return(outputSIM)
}
