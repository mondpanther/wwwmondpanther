# routines to maintain the mulama roulette



# Draw dataset ####
drawdata=function(FIXFLAG=c("K","L")){
  
  

  
  
  #print(FIXFLAG)
  alphK=gam-alphL-alphM
  
  
  
  ids=data.frame(id=1:firms) %>% mutate(alphmu=gam+sigalphmu*runif(n()))
  
  #id=1
  ts=data.frame(t=1:periods)
  #philam=1
  
  df=ids |> merge(ts)   |> arrange(id,t) |>
    group_by(id) |>
    mutate(nua  =rnorm(n(),0,signua),     # shocks to 
           
           nulam=signumu* runif(n()),     # shocks to lambda
           
           mu   =1+alphmu+nulam,          # make mu  
           
           eta  =mu/(mu-1) ,              # make eta
           
           lam  = nulam,                  # initialise lambda
           a    = nua,                    # initialise a
           
           lam  = purrr::accumulate(lam, ~ .x *philam+.y),  # build AR(1) for lambda
           
           
           a    = purrr::accumulate(a,~ .x *phia+ .y) ,        # build AR(1) for a
           omega=(a+lam)/mu,                        # make omega
           
           
           A=exp(a),
           Lam=exp(lam),
           sumalphFLEX=alphL+alphM+alphK,
           
           QFLEX=(A * alphL^alphL * alphM^alphM *alphK^alphK *  # build all factors flexible Q
                    Lam^(sumalphFLEX/mu)/mu^sumalphFLEX )^(mu/(mu-sumalphFLEX)) ,
           
           
           nuk=rnorm(n(),0,signuk),                         # make shocks to capital
           NUK=exp(nuk),
           KFLEX=alphK * QFLEX^(1/mu) * Lam^(1/mu)/mu,
           K=lag(KFLEX) * exp(nuk),  # build capital 
           k=log(K),
           
           
           sumalph=alphL+alphM,
           
           QKstar=(A * alphL^alphL * alphM^alphM * K^alphK *      # build Q conditional on capital
                     Lam^(sumalph/mu)/mu^sumalph )^(mu/(mu-sumalph)) ,
           Qstar=QKstar)   #%>% filter(t>1)
  
  if("L" %in% FIXFLAG){                              # If L also fixed factor
    #print("Hello")
    df=df %>% mutate(nul=rnorm(n(),0,signul),        # make quasi fixed labor too in case we assume fixed
                     LFLEX=alphL * QFLEX^(1/mu) * Lam^(1/mu)/mu,
                     L=lag(LFLEX) *exp(nul),  
                     
                     QKLstar=(A *  alphM^alphM *L^alphL* K^alphK *      # build Q conditional on capital and labor
                                Lam^(alphM/mu)/mu^alphM )^(mu/(mu-alphM)), 
                     
                     Qstar=QKLstar)
    
    
  } 
  else {
    
    df= df %>% mutate(  L=alphL  * Qstar^(1/mu)*Lam^(1/mu)/mu)
    
  }
  df=df %>% mutate(
    P=Lam^(1/mu)*Qstar^(-1/eta),                           # prices
    
    R=Qstar*P,                                            # revenues
    r=log(R),
    
    
    M=alphM  * Qstar^(1/mu)*Lam^(1/mu)/mu,
    m=log(M),
    l=log(L),
    alphMOmu=alphM/mu,
    sM=M/R
    
  )
  return(df)
  
}



# Estimation ####
library(fixest)
library(purrr)

esti=function(rlabel,df){
  #rlabel=1 
  # build objects for estimation
  #df=df %>% filter(t>1)
  df=df %>% group_by(id) %>% mutate(sM = M/R,
                                    Q=R/P,
                                    q=log(Q),
                                    
                                    LHS=(r - sM * (m-k) ) / sM,
                                    z1=k,
                                    z2=l-k,
                                    z3=lag(LHS),
                                    z4=lag(k),
                                    z5=lag(l-k),
                                    z6=lag(r/sM),
                                    z7=lag(q)
                                    
  )
  
  reg1=feols(LHS~z1+z2+z3+z4+z5+z6+z7| t , df, warn=F,note=F)
  #reg1=lm(LHS~z1+z2+z3+z4+z5+z6+z7+factor(t),df)
  
  phia=reg1[["coefficients"]][["z3"]]
  b1  =reg1[["coefficients"]][["z1"]]
  b2  =reg1[["coefficients"]][["z2"]]
  b4  =reg1[["coefficients"]][["z4"]]
  b6  =reg1[["coefficients"]][["z6"]]
  
  df=df %>% mutate(    LHSbar=q-phia*lag(q),
                       z8=1/b1 * (m-k) + b2/b1 * (l-k) + k+phia/b1*lag(LHS)- b2 * phia/b1*lag(l-k)-phia*lag(k)-phia*lag(r)/(b1*lag(sM))  )
  
  #reg2=lm(LHSbar ~ z8,df)
  
  
  
  #c("1", "2", "ABC") %>% {quietly(as.numeric)}() 
  reg2= feols(LHSbar~1| t | z8~k+l
              +lag(k)+lag(l)
              , df, warn=F,note=F)
  #reg2={quietly( feols)}(LHSbar~1| t | z8~k, df, warn=F)
  #reg2=reg2$result
  # recover parameters
  
  esti=data.frame(rlabel=rlabel,
                  gam=reg2[["coefficients"]][["fit_z8"]],
                  alphM=gam/b1,
                  alphL=b2*alphM,
                  phia=phia,
                  b1=b1,
                  b2=b2,
                  b4=b4,
                  b6=b6,
                  philam=b6+phia) 
  
  
}



# Run montecarlo ####
monte=function(params){
  print("Hello")
  
  #message(paste('ok made it this far with x='))
  # recover parameters
  for(pp in names(params)){
    expr=paste0(pp,"=",params[[pp]])
    eval(parse(text=expr))
    #print(expr)
  }

#}

  b1=gam/alphM
  b4=-b1*phia
  
  
  
#xxx=function(){  
  library(foreach)
  library(doParallel)
  
  cores=detectCores()
  #cl <- makeCluster(cores[1]-2) #not to overload your computer
  cl <- makeCluster(7) #not to overload your computer
  
  combiner=function(final,temp){
    library(dplyr)
    final=bind_rows(final,temp)
    return(final)
  }
  
  registerDoParallel(cl)
  
  
  #repli=10
  rr_df <- foreach(cou=1:repli, 
                   .combine=combiner, 
                   .export=c(names(params),"drawdata","esti"),
                   #.export=c("philam","phia","alphL","alphM","periods","firms","gam"),
                   .packages=c("stringr","fixest","dplyr","purrr"),
                   .inorder=F) %dopar% {
                     cdf=drawdata()
                     esti(cou,cdf)
                     #print(summary(cdf))
                     
                   }
  #stop cluster
  stopCluster(cl)
  
  
  
  summary=rr_df %>% summary() 
  
  library(ggplot2)
  rr_df %>% ggplot(aes(x=gam))+geom_density()+
    geom_vline(xintercept = gam,         color = "blue", size=1.5)+
    theme_minimal()
  
  rr_df %>% ggplot(aes(x=alphM))+geom_density()+
    geom_vline(xintercept = alphM,       color = "blue", size=1.5)+
    theme_minimal()
  
  rr_df %>% ggplot(aes(x=alphL))+geom_density()+
    geom_vline(xintercept = alphL,       color = "blue", size=1.5)+
    theme_minimal()
  
  rr_df %>% ggplot(aes(x=phia))+geom_density()+
    geom_vline(xintercept = phia,        color = "blue", size=1.5)+
    theme_minimal()
  
  rr_df %>% ggplot(aes(x=philam))+geom_density()+
    geom_vline(xintercept = philam,      color = "blue", size=1.5)+
    theme_minimal()
  
  rr_df %>% ggplot(aes(x=b1))+geom_density()+
    geom_vline(xintercept = b1,        color = "blue", size=1.5)+
    theme_minimal()
  
  rr_df %>% ggplot(aes(x=b4))+geom_density()+
    geom_vline(xintercept = b4,        color = "blue", size=1.5)+
    theme_minimal()
  
  return(rr_df)
  
  
}

