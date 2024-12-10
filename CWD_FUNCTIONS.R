
### Creating the functions for disease free populations and when disease is present. There are switches to turn on and off the hunting and predation portions.

calcInfection<- function(n.years = 30, n0 = 10000, n.age.cats = 18 , 
                         juvs=c(1:2),adults=c(3:13),old=c(14:18), fawn.an.sur = 0.7,
                         juv.an.sur  = 0.9, ad.an.f.sur = 0.9, ad.an.m.sur = 0.8,
                         fawn.repro = 0, juv.repro = 0.4, ad.repro = 0.9,
                         hunt.mort.fawn = 0.02,hunt.mort.juv.f = 0.03, hunt.mort.juv.m = 0.03,
                         hunt.mort.ad.f = 0.05, hunt.mort.ad.m = 0.1,
                         n.predators = 70, min.predators = 20, max.predators = 150, 
                         n_inflect = 2000, k_max = 1, k_inflect = 3000, r = 0.25,
                         numeric.form ="Type2", functional.form ="Type3", base.juv = 3, 
                         base.adult = 1, base.old = 2, selection.form ='exponential', 
                         ini.fawn.prev = 0.01, ini.juv.prev.f  =0.03, ini.juv.prev.m  = 0.03,
                         ini.ad.prev.f = 0.04, ini.ad.prev.m = 0.04, stages  =c(0:10), p = 0.28,
                         env.foi = 0.0, beta.low.f = 0.026, beta.low.m = 0.026, S = 5, 
                         beta.high.f = 0.13, beta.high.m = 0.13, theta =1, rel.risk =1.0,
                         H=1,P=0){
  
  ## Initial Conditions ##
  
  months <- seq(1, n.years * 12)   # monthly timestep
  hunt.mo <- rep(0, n.years * 12)  # months in where the hunt occurs
  hunt.mo[months%%12 == 7] <- 1    # hunt.mo==1 on Nov
  
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)
  
  
  # Create the survival and birth vectors vector of survival rates for all age classes
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur,n.age.cats-2))
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur,n.age.cats-2))
  
  # group into a vector initial female prevalence
  ini.prev.f <- c(ini.fawn.prev, ini.juv.prev.f, rep(ini.ad.prev.f,n.age.cats-2))
  # initial male prevalence
  ini.prev.m <- c(ini.fawn.prev, ini.juv.prev.m, rep(ini.ad.prev.m,n.age.cats-2))
  
  # Create the Leslie Matrix to start the population at stable age dist
  
  if (H==0){
    
    M <- matrix(rep(0, n.age.cats*2*n.age.cats*2), nrow = n.age.cats*2)
    
    # replace the -1 off-diagonal with the survival rates
    M[row(M) == (col(M) + 1)] <- c(juv.an.sur,
                                   rep(ad.an.f.sur,n.age.cats-2),0,
                                   c(juv.an.sur,rep(ad.an.m.sur,n.age.cats-2)))
    
    # if you want the top age category to continue to survive (yes), put adult
    # survival in top age cat
    M[n.age.cats, n.age.cats] <- ad.an.f.sur
    M[n.age.cats*2, n.age.cats*2] <- ad.an.m.sur
    
    # insert the fecundity vector for prebirth census - assuming 50/50 sex ratio
    M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats-2)) * 0.5 * fawn.an.sur
    M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
    
  }
  
  if (H==1){
    
    M <- matrix(rep(0, n.age.cats * 2 * n.age.cats * 2), nrow = n.age.cats * 2)
    # replace the -1 off-diagonal with the survival rates
    M[row(M) == (col(M) + 1)] <- c(juv.an.sur * (1 - hunt.mort.juv.f),
                                   rep(ad.an.f.sur * (1 - hunt.mort.ad.f),
                                       n.age.cats - 2), 0,
                                   c(juv.an.sur *
                                       (1 - hunt.mort.juv.m),
                                     rep(ad.an.m.sur * (1 - hunt.mort.ad.m),
                                         n.age.cats - 2)))
    # if you want the top age category to continue to survive (yes), put adult female survival in top age cat
    M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
    # adult male survival in top age cat
    M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)
    
    # insert the fecundity vector for prebirth census - assuming 50/50 sex ratio
    M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) *
      0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
    M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  }
  
  # pre-allocate the output matrices
  tmp      <- matrix(0, nrow = n.age.cats, ncol = n.years * 12)
  
  #susceptible prey vector
  num.prey.s4.f <- tmp 
  num.prey.s4.m <- tmp 
  
  #infected prey vector (0:3 stages)
  num.prey.i4.f<- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10)) 
  num.prey.i4.m<- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10)) 
  
  # natural deaths vector
  Dt.prey8.f <- tmp
  Dt.prey8.m <- tmp
  
  if (H==1){
    
    # hunting deaths vector
    Ht.prey4.f <- tmp
    Ht.prey4.m <- tmp
    hunted.i.f <- tmp
    hunted.i.m <- tmp
    
  }
  
  # disease deaths vector
  CWDd.prey4.f <- tmp
  CWDd.prey4.m <- tmp
  
  
  if (P==1){
    #predators
    Predators <- c(rep(n.predators,12), rep(0,(n.years-1)*12))
    
    # diet vectors
    Proportion_Diet_S.f <- tmp   
    Proportion_Diet_S.m <- tmp   
    
    Proportion_Diet_I.f <- num.prey.i4.f
    Proportion_Diet_I.m <- num.prey.i4.m
    
    #predation vectors
    Predated_S2.f <- tmp   # healthy 
    Predated_S2.m <- tmp   # healthy 
    
    Predated_I2.f <- num.prey.i4.f  # infected 
    Predated_I2.m <- num.prey.i4.m  # infected 
  }
  
  # Initializing with the stable age distribution
  num.prey.s4.f[, 1] <- popbio::stable.stage(M)[1:n.age.cats] * n0* (1 - ini.prev.f)
  num.prey.s4.m[, 1] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats * 2)] * n0* (1 - ini.prev.m)
  
  if(sum(num.prey.s4.f[,1]) <= 0) {
    warning("These parameters result in a stable age structure with no surviving 
            females.")
  }
  
  # equally allocating prevalence across ages
  num.prey.i4.f[, 1, 1:10] <- popbio::stable.stage(M)[1:n.age.cats] * n0/10 * ini.prev.f
  num.prey.i4.m[, 1, 1:10] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats * 2)] * n0/10 * ini.prev.m
  
  ## POPULATION MODEL ###
  for (t in 2:(n.years * 12)) {
    
    # on birthdays, add in recruits and age everyone by one year births
    # reproduction is in June, model starts in May
    if (t%%12 == 2) {
      
      # susceptible aging the last age category remains in place and doesn't die
      num.prey.s4.f[2:(n.age.cats - 1), t] <- num.prey.s4.f[1:(n.age.cats - 2), t - 1]
      num.prey.s4.f[n.age.cats, t] <- num.prey.s4.f[n.age.cats, t - 1] + num.prey.s4.f[(n.age.cats - 1), t - 1]
      
      num.prey.s4.m[2:(n.age.cats - 1), t] <- num.prey.s4.m[1:(n.age.cats - 2), t - 1]
      num.prey.s4.m[n.age.cats, t] <- num.prey.s4.m[n.age.cats, t - 1] + num.prey.s4.m[(n.age.cats - 1), t - 1]
      
      # infected aging the last age category remains in place and doesn't die
      num.prey.i4.f[2:(n.age.cats - 1), t, ] <- num.prey.i4.f[1:(n.age.cats - 2), t - 1, ]
      num.prey.i4.f[n.age.cats, t, ] <- num.prey.i4.f[n.age.cats, t - 1, ] + num.prey.i4.f[(n.age.cats - 1), t - 1, ]
      
      num.prey.i4.m[2:(n.age.cats - 1), t, ] <- num.prey.i4.m[1:(n.age.cats - 2), t - 1, ]
      num.prey.i4.m[n.age.cats, t, ] <- num.prey.i4.m[n.age.cats, t - 1, ] + num.prey.i4.m[(n.age.cats - 1), t - 1, ]
      
      # reproduction (female only)
      I_juv <- sum(num.prey.i4.f[2, t - 1, ])
      I_adults <- sum(num.prey.i4.f[3:n.age.cats, t - 1, ])
      
      num.prey.s4.f[1, t] <- ((sum(num.prey.s4.f[2, t - 1]) + I_juv) * juv.repro +
                                (sum(num.prey.s4.f[3:n.age.cats, t - 1]) + I_adults) * ad.repro) * 0.5
      num.prey.s4.m[1, t] <- ((sum(num.prey.s4.f[2, t - 1]) + I_juv) * juv.repro +
                                (sum(num.prey.s4.f[3:n.age.cats, t - 1]) + I_adults) * ad.repro) * 0.5
      
    }
    
    if (t%%12 != 2) {
      
      # updating the next month
      num.prey.s4.f[, t] <-num.prey.s4.f[, t - 1]#+num.prey.s4[, t - 1]*((C-sum(num.prey.s4[,t-1]+num.prey.i4[,t-1,]))/C)
      num.prey.s4.m[, t] <-num.prey.s4.m[, t - 1]#+num.prey.s4[, t - 1]*((C-sum(num.prey.s4[,t-1]+num.prey.i4[,t-1,]))/C)
      
      num.prey.i4.f[, t, ] <- num.prey.i4.f[, t - 1,]#+num.prey.i4[, t - 1]*((sum(num.prey.s4)-sum(num.prey.i4[,t-1,]))/sum(num.prey.s4[,t-1]))
      num.prey.i4.m[, t, ] <- num.prey.i4.m[, t - 1,]#+num.prey.i4[, t - 1]*((sum(num.prey.s4)-sum(num.prey.i4[,t-1,]))/sum(num.prey.s4[,t-1]))
    }
    
    # Disease mortality: stochastic movement of individuals from one stage to next
    # disease induced mortality occurs by advancing all I's and only a proportion of the final (10) category remains
    move.f <- num.prey.i4.f[, t, ] * p
    move.m <- num.prey.i4.m[, t, ] * p
    
    num.prey.i4.f[, t, 1] <- num.prey.i4.f[, t, 1] - move.f[, 1]
    num.prey.i4.f[, t, 2:10] <- num.prey.i4.f[, t, 2:10] - move.f[, 2:10] + move.f[, 1:9]
    
    num.prey.i4.m[, t, 1] <- num.prey.i4.m[, t, 1] - move.m[, 1]
    num.prey.i4.m[, t, 2:10] <- num.prey.i4.m[, t, 2:10] - move.m[, 2:10] + move.m[, 1:9]
    
    # store info on those that die directly from disease
    CWDd.prey4.f[, t] <- move.f[, 10]
    CWDd.prey4.m[, t] <- move.m[, 10]
    
    # Infection 
    
    Ilow <- sum(num.prey.i4.f[, t, 1:7]+num.prey.i4.m[, t, 1:7])
    Ihigh <- sum(num.prey.i4.f[, t, 8:10]+num.prey.i4.m[, t, 8:10])
    Nall <- sum(num.prey.s4.f[, t]+num.prey.s4.m[, t]) + Ilow + Ihigh
 
    
    ## CALCULATE NEW CASES
    cases.f <- num.prey.s4.f[, t] * (1 - exp(- (beta.low.f*(Ilow/Nall^theta) + beta.high.f*(Ihigh/Nall^theta)) ))
    cases.m <- num.prey.s4.m[, t] * (1 - exp(- (beta.low.m*(Ilow/Nall^theta) + beta.high.m*(Ihigh/Nall^theta)) ))
    
    num.prey.s4.f[, t] <- num.prey.s4.f[, t] - cases.f
    num.prey.s4.m[, t] <- num.prey.s4.m[, t] - cases.m
    
    num.prey.i4.f[, t, 1] <- num.prey.i4.f[, t, 1] + cases.f
    num.prey.i4.m[, t, 1] <- num.prey.i4.m[, t, 1] + cases.m  
    
    # Environmental transmission happens last
    envcases.f <- num.prey.s4.f[, t] * env.foi
    envcases.m <- num.prey.s4.m[, t] * env.foi
    
    num.prey.s4.f[, t] <- num.prey.s4.f[, t] - envcases.f
    num.prey.s4.m[, t] <- num.prey.s4.m[, t] - envcases.m
    
    num.prey.i4.f[, t, 1] <- num.prey.i4.f[, t, 1] + envcases.f
    num.prey.i4.m[, t, 1] <- num.prey.i4.m[, t, 1] + envcases.m
    
    # Predation calculations
    if (P==1){
      ## SELECTION
      # selection.coeff
      
      
      if(selection.form=="exponential"){
        s.juv <- base.juv*(1 + r)^stages
        s.adult <- base.adult*(1 + r)^stages
        s.old <- base.old*(1 + r)^stages
      }
      selection.coeffs <- data.frame(cbind(s.juv,s.adult,s.old))
      
      # make new variables to work with temporarily so we are not working in the main data frame
      total.prey <- sum(num.prey.s4.f[,t],num.prey.s4.m[,t], num.prey.i4.f[,t, ],num.prey.i4.m[,t, ])
      A.f <- num.prey.s4.f[,t]
      A.m <- num.prey.s4.m[,t]
      
      AI.f <- num.prey.i4.f[,t, ]
      AI.m <- num.prey.i4.m[,t, ]
      
      # use .s to denote 'selected'
      A.s.f <- A.f
      A.s.m <- A.m
      
      AI.s.f <- AI.f
      AI.s.m <- AI.m
      
      #### PART ONE
      ## CALCULATE abundance*selection for all HEALTHY individuals (j=0)
      for(i in 1:length(A.f)){  
        if(i %in% juvs){
          temp.s <- selection.coeffs$s.juv[1]
        } 
        if(i %in% adults){
          temp.s <- selection.coeffs$s.adult[1]
        } 
        if(i %in% old){
          temp.s <- selection.coeffs$s.old[1]
        }
        
        A.s.f[i] <-  (A.f[i]*temp.s)
        A.s.m[i] <-  (A.m[i]*temp.s)
        
      }
      
      ## CALCULATE abundance*selection for all INFECTED individuals (j=1-10)
      for(i in 1:nrow(AI.f)){
        if(i %in% juvs){ 
          temp.s <- selection.coeffs[2:nrow(selection.coeffs), 1]
        }
        if(i %in% adults){
          temp.s <- selection.coeffs[2:nrow(selection.coeffs), 2]
        }
        if(i %in% old){
          temp.s <- selection.coeffs[2:nrow(selection.coeffs), 3]
        }
        # temp.s
        
        AI.s.f[i,] <-  (AI.f[i,]*temp.s)
        AI.s.m[i,] <-  (AI.m[i,]*temp.s)
        
      }
      
      ## CALCULATE proportion of each age*infection class in the predator's diet
      # first we need the total sum
      total.sum <- (sum(A.s.f)+sum(A.s.m)+sum(AI.s.f)+sum(AI.s.m))
      
      # equation = sA[i] / (sA[i]+sum(all other sA[i]'s)
      A.p.f <- (A.s.f / total.sum)
      A.p.m <- (A.s.m / total.sum)
      
      AI.p.f <- (AI.s.f / total.sum)
      AI.p.m <- (AI.s.m / total.sum)
      
      ## PUT INTO STORAGE ARRAYS
      Proportion_Diet_S.f[,t] <- A.p.f
      Proportion_Diet_S.m[,t] <- A.p.m
      
      Proportion_Diet_I.f[,t,1:10] <- AI.p.f
      Proportion_Diet_I.m[,t,1:10] <- AI.p.m
      
      #### PART TWO - calculate prey removal
      
      n.predators <- Predators[t]
      
      if(functional.form=="Type2"){
        k <- (n.predators*k_max*total.prey) / (k_inflect + total.prey)
      }
      if(functional.form=="Type3"){
        k <- (n.predators*k_max*(total.prey^2)) / ((k_inflect^2)+(total.prey^2))
      }
      k
      
      ## calculate how many individuals to REMOVE in their age*infection class + PUT INTO STORAGE ARRAYS
      
      Predated_S2.f[,t] <- (A.p.f * k)
      Predated_S2.m[,t] <- (A.p.m * k)
      
      Predated_I2.f[,t,1:10] <- (AI.p.f * k)
      Predated_I2.m[,t,1:10] <- (AI.p.m * k)
      
      ## ACTUALLY REMOVE THOSE INDIVIDUALS
      num.prey.s4.f[, t] <- (num.prey.s4.f[, t] - Predated_S2.f[, t])
      num.prey.s4.m[, t] <- (num.prey.s4.m[, t] - Predated_S2.m[, t])
      
      num.prey.i4.f[, t, ] <- (num.prey.i4.f[, t, ] - Predated_I2.f[, t, ])
      num.prey.i4.m[, t, ] <- (num.prey.i4.m[, t, ] - Predated_I2.m[, t, ])
    }
    
    
    # Hunitng mortality 
    if (H==1){  
      # Hunt mortality
      if (hunt.mo[t] == 1) {
        Iall.f<- rowSums(num.prey.i4.f[, t, ])
        Iall.m<- rowSums(num.prey.i4.m[, t, ])
        
        Nt.f <-  num.prey.s4.f[, t] + Iall.f
        Nt.m <-  num.prey.s4.m[, t] + Iall.m
        
        # total hunted
        hunted.f <- Nt.f * c(hunt.mort.fawn, hunt.mort.juv.f, rep(hunt.mort.ad.f,n.age.cats-2))
        hunted.m <- Nt.m * c(hunt.mort.fawn, hunt.mort.juv.m, rep(hunt.mort.ad.m,n.age.cats-2))
        
        # tracking the # hunted
        Ht.prey4.f[, t] <- hunted.f
        Ht.prey4.m[, t] <- hunted.m
        
        # those hunted in the I class overall are based on: the total hunted, the total that are susceptible/infected, and the relative hunting risk of S v. I - warning: this can result in a dividing by 0 and NA. 
        # This can also result in more hunting of a category than are available.
        hunted.i.f[,t] <- (rel.risk * Iall.f * hunted.f)/( num.prey.s4.f[, t] + rel.risk *Iall.f)
        hunted.i.m[,t] <- (rel.risk * Iall.m * hunted.m)/( num.prey.s4.m[, t] + rel.risk *Iall.m)
        
        hunted.i.f[,t][which(is.na(hunted.i.f[,t]))] <- 0
        hunted.i.m[,t][which(is.na(hunted.i.m[,t]))] <- 0
        
        # those hunted in the S class
        num.prey.s4.f[, t] <-  num.prey.s4.f[, t] - (hunted.f - hunted.i.f[,t])
        num.prey.s4.m[, t] <-  num.prey.s4.m[, t] - (hunted.m - hunted.i.m[,t])
        
        num.prey.i4.f[, t, ] <-  num.prey.i4.f[, t, ] * (1 - hunted.i.f[,t]/Iall.f)
        num.prey.i4.m[, t, ] <-  num.prey.i4.m[, t, ] * (1 - hunted.i.m[,t]/Iall.m)
        
      }
    }
    
    # Natural mortality
    num.prey.s4.f[, t] <- num.prey.s4.f[, t] * Sur.f
    num.prey.s4.m[, t] <- num.prey.s4.m[, t] * Sur.m
    
    num.prey.i4.f[, t, ] <- num.prey.i4.f[, t, ] * Sur.f
    num.prey.i4.m[, t, ] <- num.prey.i4.m[, t, ] * Sur.m
    
    Dt.prey8.f[, t] <- (num.prey.s4.f[, t] + rowSums(num.prey.i4.f[, t, ])) * (1 - Sur.f)
    Dt.prey8.m[, t] <- (num.prey.s4.m[, t] + rowSums(num.prey.i4.m[, t, ])) * (1 - Sur.m)
    
    if (P==1){
    ####################### PREDATOR NUMERICAL RESPONSE ########################
    if (t%%12 == 0){  ## perform on the last month of the year (April)
      total.prey <- sum(num.prey.s4.f[, t],num.prey.s4.m[, t],num.prey.i4.f[,t,],num.prey.i4.m[,t,])
      
      if(numeric.form=="Type2"){
        predators_t1 <- (max.predators*total.prey) / (n_inflect + total.prey)
      }
      if(numeric.form=="Type3"){
        predators_t1 <- (max.predators*(total.prey^2)) / ((n_inflect^2)+(total.prey^2))
      }
      
      predators_t1 <- ifelse(predators_t1<min.predators, min.predators, predators_t1)
      Predators[(t+1) : (t+12)] <- round(predators_t1, 2)
    }
    }
  }
  
if (P==1){
  Predators1 <- Predators[1:(length(Predators)-12)]
  p <- Predators1
  pt <- matrix(p, nrow=18, ncol=length(p), byrow=TRUE)
}
  
  if (H==0&P==0){
    return(list(SFpreyA = num.prey.s4.f,
                SMpreyA = num.prey.s4.m,
                TotalSprey =  num.prey.s4.f+num.prey.s4.m,
                IFpreyA = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+
                  num.prey.i4.f[,,3]+num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+
                  num.prey.i4.f[,,8]+num.prey.i4.f[,,9]+num.prey.i4.f[,,10],
                IMpreyA = num.prey.i4.m[,,1]+num.prey.i4.m[,,2]+
                  num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+num.prey.i4.m[,,5]+num.prey.i4.m[,,6]+num.prey.i4.m[,,7]+
                  num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+num.prey.i4.m[,,10],
                TotalIpreyL = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5],
                TotalIpreyH = num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                TotalIprey = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                Totalprey = num.prey.s4.f+ num.prey.s4.m + num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                FCWDdeath = CWDd.prey4.f, 
                MCWDdeath = CWDd.prey4.m, 
                TotalCWDdeath = CWDd.prey4.f+CWDd.prey4.m,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
    
    
  }
  
  if (H==1&P==0){
    return(list(SFpreyA = num.prey.s4.f,
                SMpreyA = num.prey.s4.m,
                TotalSprey = num.prey.s4.f+num.prey.s4.m,
                IFpreyA = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+
                  num.prey.i4.f[,,3]+num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+
                  num.prey.i4.f[,,8]+num.prey.i4.f[,,9]+num.prey.i4.f[,,10],
                IMpreyA = num.prey.i4.m[,,1]+num.prey.i4.m[,,2]+
                  num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+num.prey.i4.m[,,5]+num.prey.i4.m[,,6]+num.prey.i4.m[,,7]+
                  num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+num.prey.i4.m[,,10],
                TotalIpreyL = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5],
                TotalIpreyH = num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                TotalIprey = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                Totalprey = num.prey.s4.f+ num.prey.s4.m + num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                FpreyH = Ht.prey4.f,
                MpreyH = Ht.prey4.m,
                TotalH = Ht.prey4.f+Ht.prey4.m,
                TotalHI = hunted.i.f+hunted.i.m,
                FCWDdeath = CWDd.prey4.f, 
                MCWDdeath = CWDd.prey4.m, 
                TotalCWDdeath = CWDd.prey4.f+CWDd.prey4.m,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
    
    
  }
  
  if (H==0&P==1){
    return(list(SFpreyA = num.prey.s4.f,
                SMpreyA = num.prey.s4.m,
                TotalSprey = num.prey.s4.f+num.prey.s4.m,
                IFpreyA = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+
                  num.prey.i4.f[,,3]+num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+
                  num.prey.i4.f[,,8]+num.prey.i4.f[,,9]+num.prey.i4.f[,,10],
                IMpreyA = num.prey.i4.m[,,1]+num.prey.i4.m[,,2]+
                  num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+num.prey.i4.m[,,5]+num.prey.i4.m[,,6]+num.prey.i4.m[,,7]+
                  num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+num.prey.i4.m[,,10],
                TotalIpreyL = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5],
                TotalIpreyH = num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                TotalIprey = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                Totalprey = num.prey.s4.f+ num.prey.s4.m + num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                FCWDdeath = CWDd.prey4.f, 
                MCWDdeath = CWDd.prey4.m, 
                TotalCWDdeath = CWDd.prey4.f+CWDd.prey4.m,
                SFPropDiet = Proportion_Diet_S.f,    
                SMPropDiet = Proportion_Diet_S.m,    
                IFPropDiet = colSums(Proportion_Diet_I.f[,,1:10]), 
                IMPropDiet = colSums(Proportion_Diet_I.m[,,1:10]),
                SFPreddeath = Predated_S2.f,   
                SMPreddeath = Predated_S2.m,             
                IFPreddeath = Predated_I2.f[,,1]+Predated_I2.f[,,2]+Predated_I2.f[,,3]+
                  Predated_I2.f[,,4]+Predated_I2.f[,,5]+Predated_I2.f[,,6]+
                  Predated_I2.f[,,7]+Predated_I2.f[,,8]+
                  Predated_I2.f[,,9]+Predated_I2.f[,,10],
                IMPreddeath = Predated_I2.m[,,1]+Predated_I2.m[,,2]+Predated_I2.m[,,3]+
                  Predated_I2.m[,,4]+Predated_I2.m[,,5]+Predated_I2.m[,,6]+
                  Predated_I2.m[,,7]+Predated_I2.m[,,8]+
                  Predated_I2.m[,,9]+Predated_I2.m[,,10],
                TotalPreddeath = Predated_S2.f+Predated_S2.m+Predated_I2.f[,,1]+
                  Predated_I2.f[,,2]+Predated_I2.f[,,3]+Predated_I2.f[,,4]+
                  Predated_I2.f[,,5]+Predated_I2.f[,,6]+
                  Predated_I2.f[,,7]+Predated_I2.f[,,8]+Predated_I2.f[,,9]+
                  Predated_I2.f[,,10]+Predated_I2.m[,,1]+Predated_I2.m[,,2]+
                  Predated_I2.m[,,3]+ Predated_I2.m[,,4]+Predated_I2.m[,,5]+
                  Predated_I2.m[,,6]+Predated_I2.m[,,7]+
                  Predated_I2.m[,,8]+ Predated_I2.m[,,9]+Predated_I2.m[,,10],
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
    
  }
  
  if (H==1&P==1){
    return(list(SFpreyA = num.prey.s4.f,
                SMpreyA = num.prey.s4.m,
                TotalSprey = num.prey.s4.f+num.prey.s4.m,
                IFpreyA = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+
                  num.prey.i4.f[,,3]+num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+
                  num.prey.i4.f[,,8]+num.prey.i4.f[,,9]+num.prey.i4.f[,,10],
                IMpreyA = num.prey.i4.m[,,1]+num.prey.i4.m[,,2]+
                  num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+num.prey.i4.m[,,5]+num.prey.i4.m[,,6]+num.prey.i4.m[,,7]+
                  num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+num.prey.i4.m[,,10],
                TotalIpreyL = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5],
                TotalIpreyH = num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                TotalIprey = num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                Totalprey = num.prey.s4.f+ num.prey.s4.m + num.prey.i4.f[,,1]+num.prey.i4.f[,,2]+num.prey.i4.f[,,3]+
                  num.prey.i4.f[,,4]+num.prey.i4.f[,,5]+num.prey.i4.m[,,1]+
                  num.prey.i4.m[,,2]+num.prey.i4.m[,,3]+num.prey.i4.m[,,4]+
                  num.prey.i4.m[,,5]+num.prey.i4.f[,,6]+num.prey.i4.f[,,7]+num.prey.i4.f[,,8]+
                  num.prey.i4.f[,,9]+num.prey.i4.f[,,10]+num.prey.i4.m[,,6]+
                  num.prey.i4.m[,,7]+num.prey.i4.m[,,8]+num.prey.i4.m[,,9]+
                  num.prey.i4.m[,,10],
                FpreyH = Ht.prey4.f,
                MpreyH = Ht.prey4.m,
                TotalH = Ht.prey4.f+Ht.prey4.m,
                TotalHI = hunted.i.f+hunted.i.m,
                FCWDdeath = CWDd.prey4.f, 
                MCWDdeath = CWDd.prey4.m, 
                TotalCWDdeath = CWDd.prey4.f+CWDd.prey4.m,
                SFPropDiet = Proportion_Diet_S.f,    
                SMPropDiet = Proportion_Diet_S.m,    
                IFPropDiet = colSums(Proportion_Diet_I.f[,,1:10]), 
                IMPropDiet = colSums(Proportion_Diet_I.m[,,1:10]),
                SFPreddeath = Predated_S2.f,   
                SMPreddeath = Predated_S2.m,  
                IFPreddeath = Predated_I2.f[,,1]+Predated_I2.f[,,2]+Predated_I2.f[,,3]+
                  Predated_I2.f[,,4]+Predated_I2.f[,,5]+Predated_I2.f[,,6]+
                  Predated_I2.f[,,7]+Predated_I2.f[,,8]+
                  Predated_I2.f[,,9]+Predated_I2.f[,,10],
                IMPreddeath = Predated_I2.m[,,1]+Predated_I2.m[,,2]+Predated_I2.m[,,3]+
                  Predated_I2.m[,,4]+Predated_I2.m[,,5]+Predated_I2.m[,,6]+
                  Predated_I2.m[,,7]+Predated_I2.m[,,8]+
                  Predated_I2.m[,,9]+Predated_I2.m[,,10],
                TotalPreddeath = Predated_S2.f+Predated_S2.m+Predated_I2.f[,,1]+
                  Predated_I2.f[,,2]+Predated_I2.f[,,3]+Predated_I2.f[,,4]+
                  Predated_I2.f[,,5]+Predated_I2.f[,,6]+
                  Predated_I2.f[,,7]+Predated_I2.f[,,8]+Predated_I2.f[,,9]+
                  Predated_I2.f[,,10]+Predated_I2.m[,,1]+Predated_I2.m[,,2]+
                  Predated_I2.m[,,3]+ Predated_I2.m[,,4]+Predated_I2.m[,,5]+
                  Predated_I2.m[,,6]+Predated_I2.m[,,7]+
                  Predated_I2.m[,,8]+ Predated_I2.m[,,9]+Predated_I2.m[,,10],
                Predators=pt ,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
    
  }
  
}

### This is the function for the population without disease.

calcHealthy<- function(n.years = 30, n0 = 10000, n.age.cats = 18 , 
                       juvs=c(1:2),adults=c(3:13),old=c(14:18), fawn.an.sur = 0.7,
                       juv.an.sur  = 0.9, ad.an.f.sur = 0.9, ad.an.m.sur = 0.8,
                       fawn.repro = 0, juv.repro = 0.4, ad.repro = 0.9,
                       hunt.mort.fawn = 0.02,hunt.mort.juv.f = 0.03, hunt.mort.juv.m = 0.03,
                       hunt.mort.ad.f = 0.05, hunt.mort.ad.m = 0.1,
                       n.predators = 70, min.predators = 20, max.predators = 150, 
                       n_inflect = 2000, k_max = 1, k_inflect = 3000, r = 0.25,
                       numeric.form ="Type2", functional.form ="Type3", base.juv = 3, 
                       base.adult = 1, base.old = 2, selection.form ='exponential', H=0,P=0){
  ## Initial Conditions ##
  
  months <- seq(1, n.years * 12)   # monthly timestep
  hunt.mo <- rep(0, n.years * 12)  # months in where the hunt occurs
  hunt.mo[months%%12 == 7] <- 1    # hunt.mo==1 on Nov
  
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)
  
  
  # Create the survival and birth vectors vector of survival rates for all age classes
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur,n.age.cats-2))
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur,n.age.cats-2))
  
  # Create the Leslie Matrix to start the population at stable age dist
  
  if (H==0){
    
    M <- matrix(rep(0, n.age.cats*2*n.age.cats*2), nrow = n.age.cats*2)
    
    # replace the -1 off-diagonal with the survival rates
    M[row(M) == (col(M) + 1)] <- c(juv.an.sur,
                                   rep(ad.an.f.sur,n.age.cats-2),0,
                                   c(juv.an.sur,rep(ad.an.m.sur,n.age.cats-2)))
    
    # if you want the top age category to continue to survive (yes), put adult
    # survival in top age cat
    M[n.age.cats, n.age.cats] <- ad.an.f.sur
    M[n.age.cats*2, n.age.cats*2] <- ad.an.m.sur
    
    # insert the fecundity vector for prebirth census - assuming 50/50 sex ratio
    M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats-2)) * 0.5 * fawn.an.sur
    M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
    
  }
  
  if (H==1){
    
    M <- matrix(rep(0, n.age.cats * 2 * n.age.cats * 2), nrow = n.age.cats * 2)
    # replace the -1 off-diagonal with the survival rates
    M[row(M) == (col(M) + 1)] <- c(juv.an.sur * (1 - hunt.mort.juv.f),
                                   rep(ad.an.f.sur * (1 - hunt.mort.ad.f),
                                       n.age.cats - 2), 0,
                                   c(juv.an.sur *
                                       (1 - hunt.mort.juv.m),
                                     rep(ad.an.m.sur * (1 - hunt.mort.ad.m),
                                         n.age.cats - 2)))
    # if you want the top age category to continue to survive (yes), put adult female survival in top age cat
    M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
    # adult male survival in top age cat
    M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)
    
    # insert the fecundity vector for prebirth census - assuming 50/50 sex ratio
    M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) *
      0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
    M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  }
  
  # pre-allocate the output matrices
  tmp      <- matrix(0, nrow = n.age.cats, ncol = n.years * 12)
  
  #susceptible prey vector
  num.prey4.f <- tmp 
  num.prey4.m <- tmp 
  
  # natural deaths vector
  Dt.prey8.f <- tmp
  Dt.prey8.m <- tmp
  
  if (H==1){
    
    # hunting deaths vector
    Ht.prey3.f <- tmp
    Ht.prey3.m <- tmp
    
  }
  
  if (P==1){
    #predators
    Predators <- c(rep(n.predators,12), rep(0,(n.years-1)*12))
    
    # diet vectors
    Proportion_Diet.f <- tmp   
    Proportion_Diet.m <- tmp   
    
    #predation vectors
    Predated2.f <- tmp   # healthy 
    Predated2.m <- tmp   # healthy 
  }
  
  # Initializing with the stable age distribution
  num.prey4.f[, 1] <- popbio::stable.stage(M)[1:n.age.cats] * n0
  num.prey4.m[, 1] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats * 2)] * n0
  
  if(sum(num.prey4.f[,1]) <= 0) {
    warning("These parameters result in a stable age structure with no surviving 
            females.")
  }
  
  ## POPULATION MODEL ###
  for (t in 2:(n.years * 12)) {
    
    # on birthdays, add in recruits and age everyone by one year births
    # reproduction is in June, model starts in May
    if (t%%12 == 2) {
      
      # susceptible aging the last age category remains in place and doesn't die
      num.prey4.f[2:(n.age.cats - 1), t] <- num.prey4.f[1:(n.age.cats - 2), t - 1]
      num.prey4.f[n.age.cats, t] <- num.prey4.f[n.age.cats, t - 1] + num.prey4.f[(n.age.cats - 1), t - 1]
      
      num.prey4.m[2:(n.age.cats - 1), t] <- num.prey4.m[1:(n.age.cats - 2), t - 1]
      num.prey4.m[n.age.cats, t] <- num.prey4.m[n.age.cats, t - 1] + num.prey4.m[(n.age.cats - 1), t - 1]
      
      # reproduction (female only)
      
      num.prey4.f[1, t] <- (sum(num.prey4.f[2, t - 1]) * juv.repro +
                              sum(num.prey4.f[3:n.age.cats, t - 1])* ad.repro) * 0.5
      num.prey4.m[1, t] <- (sum(num.prey4.f[2, t - 1]) * juv.repro +
                              sum(num.prey4.f[3:n.age.cats, t - 1]) * ad.repro) * 0.5
      
    }
    
    if (t%%12 != 2) {
      
      # updating the next month
      num.prey4.f[, t] <-num.prey4.f[, t - 1]#+num.prey.s4[, t - 1]*((C-sum(num.prey.s4[,t-1]+num.prey.i4[,t-1,]))/C)
      num.prey4.m[, t] <-num.prey4.m[, t - 1]#+num.prey.s4[, t - 1]*((C-sum(num.prey.s4[,t-1]+num.prey.i4[,t-1,]))/C)
      
      # Predation calculations
      if (P==1){
        ## SELECTION
        # selection.coeff
        # 
        
          s.juv <- base.juv
          s.adult <- base.adult
          s.old <- base.old
        
        selection.coeffs <- data.frame(cbind(s.juv,s.adult,s.old))
        
        # make new variables to work with temporarily so we are not working in the main data frame
        total.prey <- sum(num.prey4.f[,t],num.prey4.m[,t])
        A.f <- num.prey4.f[,t]
        A.m <- num.prey4.m[,t]
        
        # use .s to denote 'selected'
        A.s.f <- A.f
        A.s.m <- A.m
        
        #### PART ONE
        ## CALCULATE abundance*selection for all HEALTHY individuals (j=0)
        for(i in 1:length(A.f)){  
          if(i %in% juvs){
            temp.s <- selection.coeffs$s.juv[1]
          } 
          if(i %in% adults){
            temp.s <- selection.coeffs$s.adult[1]
          } 
          if(i %in% old){
            temp.s <- selection.coeffs$s.old[1]
          }
          
          A.s.f[i] <-  (A.f[i]*temp.s)
          A.s.m[i] <-  (A.m[i]*temp.s)
          
        }
        
        ## CALCULATE proportion of each age*infection class in the predator's diet
        # first we need the total sum
        total.sum <- (sum(A.s.f)+sum(A.s.m))
        
        # equation = sA[i] / (sA[i]+sum(all other sA[i]'s)
        A.p.f <- (A.s.f / total.sum)
        A.p.m <- (A.s.m / total.sum)
        
        ## PUT INTO STORAGE ARRAYS
        Proportion_Diet.f[,t] <- A.p.f
        Proportion_Diet.m[,t] <- A.p.m
        
        #### PART TWO - calculate prey removal
        
        n.predators <- Predators[t]
        
        if(functional.form=="Type2"){
          k <- (n.predators*k_max*total.prey) / (k_inflect + total.prey)
        }
        if(functional.form=="Type3"){
          k <- (n.predators*k_max*(total.prey^2)) / ((k_inflect^2)+(total.prey^2))
        }
        k
        
        ## calculate how many individuals to REMOVE in their age*infection class + PUT INTO STORAGE ARRAYS
        
        Predated2.f[,t] <- (A.p.f * k)
        Predated2.m[,t] <- (A.p.m * k)
        
        ## ACTUALLY REMOVE THOSE INDIVIDUALS
        num.prey4.f[, t] <- (num.prey4.f[, t] - Predated2.f[, t])
        num.prey4.m[, t] <- (num.prey4.m[, t] - Predated2.m[, t])
        
        ### PREDATOR NUMERICAL RESPONSE ##
        if (t%%12 == 0){  ## perform on the last month of the year (April)
          total.prey <- sum(num.prey4.f[, t],num.prey4.m[, t])
          
          if(numeric.form=="Type2"){
            predators_t1 <- (max.predators*total.prey) / (n_inflect + total.prey)
          }
          if(numeric.form=="Type3"){
            predators_t1 <- (max.predators*(total.prey^2)) / ((n_inflect^2)+(total.prey^2))
          }
          
          predators_t1 <- ifelse(predators_t1<min.predators, min.predators, predators_t1)
          Predators[(t+1) : (t+12)] <- round(predators_t1, 2)
        }
        
      }
      
      # Hunitng mortality 
      if (H==1){  
        # Hunt mortality
        if (hunt.mo[t] == 1) {
          
          Nt.f <-  num.prey4.f[, t]
          Nt.m <-  num.prey4.m[, t]
          
          # total hunted
          hunted.f <- Nt.f * c(hunt.mort.fawn, hunt.mort.juv.f, rep(hunt.mort.ad.f,n.age.cats-2))
          hunted.m <- Nt.m * c(hunt.mort.fawn, hunt.mort.juv.m, rep(hunt.mort.ad.m,n.age.cats-2))
          
          # tracking the # hunted
          Ht.prey3.f[, t] <- hunted.f
          Ht.prey3.m[, t] <- hunted.m
          
          # those hunted in the S class
          num.prey4.f[, t] <-  num.prey4.f[, t] - hunted.f
          num.prey4.m[, t] <-  num.prey4.m[, t] - hunted.m
          
        }
      }
      
      # Natural mortality
      num.prey4.f[, t] <- num.prey4.f[, t] * Sur.f
      num.prey4.m[, t] <- num.prey4.m[, t] * Sur.m
      
      Dt.prey8.f[, t] <- num.prey4.f[, t] * (1 - Sur.f)
      Dt.prey8.m[, t] <- num.prey4.m[, t] * (1 - Sur.m)
      
    }
  }
  
  if (H==0&P==0){
    return(list(FpreyA = num.prey4.f,
                MpreyA = num.prey4.m,
                Totalprey = num.prey4.f + num.prey4.m,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
    
  }
  
  if (H==1&P==0){
    return(list(FpreyA = num.prey4.f,
                MpreyA = num.prey4.m,
                Totalprey = num.prey4.f + num.prey4.m,
                FpreyH = Ht.prey3.f,
                MpreyH = Ht.prey3.m,
                TotalH =  Ht.prey3.f+ Ht.prey3.m,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
    
  }
  
  if (H==0&P==1){
    return(list(FpreyA = num.prey4.f,
                MpreyA = num.prey4.m,
                Totalprey = num.prey4.f + num.prey4.m,
                FPropDiet = Proportion_Diet.f,    
                MPropDiet = Proportion_Diet.m,
                FPreddeath = Predated2.f,   
                MPreddeath = Predated2.m,
                TotalPreddeath = Predated2.f+Predated2.m,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
  }
  
  if (H==1&P==1){
    return(list(FpreyA = num.prey4.f,
                MpreyA = num.prey4.m,
                Totalprey = num.prey4.f + num.prey4.m,
                FpreyH = Ht.prey3.f,
                MpreyH = Ht.prey3.m,
                TotalH =  Ht.prey3.f+ Ht.prey3.m,
                FPropDiet = Proportion_Diet.f,    
                MPropDiet = Proportion_Diet.m,
                FPreddeath = Predated2.f,   
                MPreddeath = Predated2.m, 
                TotalPreddeath = Predated2.f+Predated2.m,
                FpreyD = Dt.prey8.f,
                MpreyD = Dt.prey8.m,
                TotalD = Dt.prey8.f+Dt.prey8.m))
  }
  
}



