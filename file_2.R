################################################################################
################################################################################
################################################################################

# RESTART SESSION, CLEAR ENVIRONMENT

################################################################################
################################################################################
################################################################################


library(magrittr)
library(dplyr)

actives <- readRDS("initial_data/actives.rds")
Global_paramlist <- readRDS("initial_data/Global_paramlist.rds")
paramlist <- readRDS("initial_data/paramlist.rds")
retirees <- readRDS("initial_data/retirees.rds")
salgrowth <- readRDS("initial_data/salgrowth.rds")
SS.all <- readRDS("generated_data/SS.all.rds") %>% as_tibble()
tailored_demoData <- readRDS("generated_data/tailored_demoData.rds")



## Functions for model control
assign_parmsList <- function(paramlist, excludes = NULL, ...){
  varNames   <- setdiff(names(paramlist), excludes)
  assign_var <- function(x) assign(x, paramlist[[x]], ...)
  sapply(varNames, assign_var)
}

################################################################################
################################################################################
################################################################################




#*************************************************************************************************************
#                     Supplement the inital salary table with all starting salary                        #####                  
#*************************************************************************************************************
## spline smoothing 
splong<-function(df,fillvar,fitrange=NULL, method = "natural"){
  # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
  # or just 2 columns, with no nonfillvar
  # last column ALWAYS must be the value var
  valvar<-names(df)[length(names(df))]
  nonfillvar<-setdiff(names(df),c(fillvar,valvar))
  f<-function(x) {
    if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
    spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
    dfout<-data.frame(x=spl$x, y=spl$y)
    names(dfout)<-c(fillvar,valvar)
    return(dfout)
  }
  if(length(nonfillvar)>0) dfl2 <- plyr::ddply(df,c(nonfillvar),f) else dfl2<-f(df)
  return(dfl2)
}



fill_startSal <- function(.actives          = tailored_demoData$actives,
                          .paramlist        = paramlist,
                          .Global_paramlist = Global_paramlist){
  # This function generate a table of initial salary (year 1) which include all starting salary levels (age = ea)
  # If the starting salary is missing from the actives data frame, spline function is used to interpolate and/or 
  # extraploate the missing values. 
  
  
  # Run the section below when developing new features.
  #   .actives          = actives
  #   .paramlist        = paramlist
  #   .Global_paramlist = Global_paramlist
  
  assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
  assign_parmsList(.paramlist,        envir = environment())  
  
  sal <- .actives %>% select(age, ea, salary)
  #x <- sal %>% spread(age, salary)
  
  sal.start <- splong(sal, "ea", range_ea) %>% filter(age == ea) %>% select(-age) %>% splong("ea", range_ea) %>% mutate(age = ea)
  
  sal <- rbind(sal, sal.start) 
  
  sal <- sal[!duplicated(sal[c("age","ea")]),]
  # sal %>% spread(age, salary)
  
  
  # DIRTY TRICK to correct negative salary in "youngplan"
  if(planname_actives == "youngplan") sal %<>%  mutate(salary =  ifelse(salary <= 0, salary[age == 62], salary ))
  
  if(any(sign(sal$salary) != 1)) stop("Negative value(s) in imputed starting salary.")
  
  return(sal)
  
}

init_sal <- fill_startSal()
# init_sal <- init_sal  %>% filter(age == ea)






#*************************************************************************************************************
#                                        Create complete salary history                                  #####                  
#*************************************************************************************************************

get_salary <- function(.SS.all = SS.all,
                       .init_sal =  init_sal,
                       .paramlist = paramlist,
                       .Global_paramlist  = Global_paramlist){
  
  # Run the section below when developing new features.
  #   .SS.all = SS.all
  #   .init_sal =  init_sal
  #   .paramlist = paramlist
  #   .Global_paramlist  = Global_paramlist
  
  
  assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
  assign_parmsList(.paramlist,        envir = environment())  
  
  #avgpay <- actives %>% filter(planname == .planname_actives) %>% select(age, ea, salary)
  
  salary <- .SS.all %>% left_join(.init_sal) %>% 
    group_by(start.year, ea) %>% 
    mutate(sx = ifelse(start.year <= 1, salary[year == 1] * scale,
                       salary[age == ea] * scale)) %>%
    select(start.year, ea, age, year, sx)
  
  
  return(salary)
}

salary <- get_salary() 
salary





#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************
get_benefit <- function(
  .retirees = tailored_demoData$retirees,
  .paramlist = paramlist,
  .Global_paramlist  = Global_paramlist){
  
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())  
  
  avgben <- .retirees %>% select(age, benefit)  
  
  benefit <- avgben %>% 
    # filter(age>=r.max) %>% 
    mutate(year = 1,
           ea = r.min - 1)
  # benefit %>% select(-year) %>% spread(age, benefit)
  
  return(benefit)
}


benefit <- get_benefit()






#*************************************************************************************************************
#                               Generating inital population                                             #####                  
#*************************************************************************************************************

get_initPop <- function (.actives          = tailored_demoData$actives,
                         .retirees         = tailored_demoData$retirees,
                         .paramlist        = paramlist,
                         .Global_paramlist = Global_paramlist){
  # Import and standardize the total number of actives and retirees.  
  
  # Run the section below when developing new features.
  #   .actives          = actives
  #   .retirees         = retirees
  #   .paramlist        = paramlist
  #   .Global_paramlist = Global_paramlist
  
  
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment()) 
  
  
  init_actives <- .actives %>% select(ea, age, nactives)
  init_actives <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_actives) %>% 
    mutate(nactives = n_init_actives * nactives/sum(nactives, na.rm = TRUE)) %>% 
    tidyr::spread(age, nactives, fill = 0) %>% select(-ea) %>% as.matrix 
  
  
  init_retirees <- .retirees %>% select(age, nretirees) %>% mutate(ea = r.min - 1) 
  init_retirees <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_retirees) %>% 
    mutate(nretirees = n_init_retirees * nretirees/sum(nretirees, na.rm = TRUE)) %>% 
    tidyr::spread(age, nretirees, fill = 0) %>% select(-ea) %>% as.matrix
  
  return(list(actives = init_actives, retirees = init_retirees))
}


# if(!dev_mode) 
init_pop <- get_initPop()






get_entrantsDist <- function(.actives          = tailored_demoData$actives,
                             .planname         = paramlist$planname_actives,
                             .range_ea         = paramlist$range_ea,
                             #.paramlist        = paramlist,
                             .Global_paramlist = Global_paramlist,
                             simple = FALSE){
  # Simple imputation rule is applied under the following two circumstances:
  # 1. parameter "simple" is set to TRUE
  # 2. negative weights are generated by the regular rule. 
  
  #   .actives          = actives
  #   .paramlist        = paramlist
  #   .Global_paramlist = Global_paramlist  
  #   .planname = "youngplan"
  #   simpe = TRUE
  #   .range_ea = paramlist$range_ea
  
  assign_parmsList(.Global_paramlist, envir = environment())
  #assign_parmsList(.paramlist,        envir = environment())   
  
  nact <- .actives %>% select(age, ea, nactives)
  #nact %>% spread(age, nactives)
  
  ## Distributon by simple rule
  nact1 <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives)) %>% right_join(data.frame(ea = .range_ea))
  while(any(is.na(nact1$avg_ent))) nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lag(avg_ent) , avg_ent))
  # nact1
  
  nact <- splong(nact, "ea", .range_ea) %>% splong("age", .range_ea) %>% filter(age >= ea)
  #nact <- splong(nact, "ea", range_ea) %>% filter(age >= ea)
  nact %>% tidyr::spread(age, nactives)
  
  
  ent <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives))
  
  neg_ea <- ent[which(ent$avg_ent < 0), "ea"]
  
  if(any(ent$avg_ent < 0)){warning("Negative inferred value(s) in the following entry age(s): " , as.character(neg_ea), "\n",
                                   "  Simple imputation rule is applied")
    ent <- nact1                          
  }
  
  # ent %<>% mutate(avg_ent = ifelse(avg_ent < 0, 0, avg_ent))
  
  if(simple) ent <- nact1
  
  dist <- lowess(ent$avg_ent, f= 0.1)$y
  dist <- dist/sum(dist)
  
  return(dist)
}

entrants_dist <- get_entrantsDist()




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
mortality <- readRDS("initial_data/mortality.rds")
termrates <- readRDS("initial_data/termrates.rds")
retrates <- readRDS("initial_data/retrates.rds")

mortality %<>% mutate(qxm = 0.6 * qxm) %>% 
  mutate(qxm.r = qxm)

termrates %<>% dplyr::rename(qxt = termrate)
termrates %<>% mutate(qxt = 1.2 * qxt)

retrates %<>% dplyr::rename(qxr = retrate)
retrates %<>% mutate(qxr = qxr * 0.7) 

library(readxl)

disb <- read_excel("initial_data/Winklevoss.xlsx", sheet = "Tab2-7Disb", skip = 1)
dbl <- read_excel("initial_data/Winklevoss.xlsx", sheet = "Tab2-5DisbLife", skip = 1)
names(dbl)[2] <- "qxmd" 

na2zero <- function(x){replace(x, is.na(x), 0)}

#*************************************************************************************************************
#                               Constructing Decrement Table  
#*************************************************************************************************************

get_decrements <- function(.paramlist = paramlist,
                           .Global_paramlist  = Global_paramlist){
  # Inputs
  # Data frames:(can be selected from the RunControl file.)   
  # - Mortality table:    by age (later we may want mortality rates change over time also). Var name qxm
  # - Termination table:  by age and ea. Var name qxt
  # - Disability table:   by age. Var name qxd
  # - Retirement table:   by age. Var name qxr. Will be coerced to single retirement age for now.
  # - Disability mortality table: by age.
  # Parameters:
  # - r.min, r.max
  # - r.yos
  # - range_age, range_ea 
  # Outputs
  # decrement   
  
  
  # Notes
  # 1) For now, we assume all decrement rates do not change over time.  
  # 2) Now assume the decrement tables contain multiple decrement rates(probabilities) rather than single decrement rates.
  #    If the decrement tables provide single decrement rates, we need to convert them to multiple decrement rates in a consistent way.   
  #    At least for TPAF, the multiple decrement rates (probabilities) are provided in AV.  
  
  ## Timing of decrements
  # Time period t is defined as the time interval [t, t+1), closed at the beginning and open at the end. 
  # Assume retirement is independent of all other risks and occurs at the beginning of time period t, with the probability qxr(t).
  # Individual's status at t becomes "retired" immediately after the risk of retirement is realized at the beginning of t.    
  # The occurence of death, disability and termination follow UUD over period t. 
  # payment of retirement benefit occurs at the beginning of t. Hence all retirees will recieve benefit at least once, at the very moment when
  # they become retirees. 
  # Given the assumptions above, it follows that (' indicates single decrement rates)
  # qe = qe'
  # qt = qt'(1 - 0.5qm')(1 - 0.5 qd')(1 - qe'), (qd, qm are similar), note that qd=qm=qt=0 at max retirement age, when qe' = 1
  # p  = 1 - qe - qt - qm - qd
  # We assume qe, qt, qd, qm are directly available from data.     
  
  
  ## Notes for new multiple retirement rates and new prototypes containing decrement tables
  # 
  
  
  
  # Run the section below when developing new features.  
  #     .paramlist = paramlist
  #     .Global_paramlist = Global_paramlist
  
  # Assign parameters to the local function call.
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())
  
  
  #*************************************************************************************************************
  #                                   1. Importing decrement tables  ####
  #*************************************************************************************************************
  
  ## Mortality (from decrement package) 
  mort <- mortality %>% filter(tablename == tablename_mortality) %>% #mutate(qxm.r = qxm * 1) %>% 
    select(age, qxm, qxm.r)
  
  
  ## Termination (Separation) rates (from plan data)
  # Consistency check: max yos <= r.full - min.ea
  # Problem: auto detecting term table by only yos and by yos and age. 
  
  term <- termrates %>% filter(planname == tablename_termination) %>% select(-planname)
  # term
  # term <- termination %>% filter(tablename == tablename_termination) %>% select(age, ea, qxt) 
  
  
  ## Service Retirement rates (from plan data)
  ret <- retrates %>% filter(planname == tablename_retirement) %>% select(-planname)
  ret
  
  
  ## Disability rates and mortality rates for disabled. (From Winklevoss data)
  disb <- disb # disability
  dbl  <- dbl  # mortality for disabled
  
  
  
  #*************************************************************************************************************
  #                      2. Putting together decrements and calculate surviving rates  ####
  #*************************************************************************************************************
  
  # Create decrement table and calculate probability of survival
  decrement <- expand.grid(age = range_age, ea = range_ea) %>% 
    mutate(yos = age - ea) %>% 
    filter(age >= ea) %>% 
    left_join(filter(mort, age >= min.age)) %>%    # mortality 
    left_join(term)  %>%                           # termination
    left_join(disb)  %>%                           # disability
    left_join(dbl)   %>%                           # mortality for disabled
    left_join(ret)    %>%                           # early retirement
    select(ea, age, everything()) %>%          
    arrange(ea, age)  #%>%
  # replace_na
  # plyr::colwise(na2zero)(.) %>% 
  
  decrement[is.na(decrement)] <- 0
  
  decrement <- decrement %>%
    group_by(ea) 
  
  # decrement$qxr <- na2zero(decrement$qxr)
  
  
  
  ## Imposing restrictions 
  decrement %<>% mutate(
    # 1. Coerce termination rates to 0 when eligible for early retirement or reaching than r.full(when we assume terms start to receive benefits). 
    qxt = ifelse((age >= r.min & (age - ea) >= r.yos) | age >= r.full, 0, qxt),
    #qxt = ifelse(age >= r.min | age >= r.full, 0, qxt),
    
    # qxt = ifelse( age >= r.full, 0, qxt),
    # 2. Coerce retirement rates to 0 when age greater than r.max                     
    #   qxr = ifelse(age == r.max, 1, 
    #                ifelse(age %in% r.min:(r.max - 1), qxr, 0))
    #   
    qxr = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
                 ifelse(age - ea < r.yos, 0, 
                        ifelse(age %in% r.min:(r.max - 1), qxr, 0)
                 )
    )
  ) 
  
  
  
  #*************************************************************************************************************
  # Motifying decrement talbes, mainly for development purposes.
  
  #  decrement$qxr <- ifelse(decrement$age == 65, 1, ifelse(decrement$age == 64, 0.3, 
  #                                                        ifelse(decrement$age == 63, 0.3, 0)
  #                                                        )) # Single retirement age.
  
  # decrement$qxr <- ifelse(decrement$age == 65, 1, ifelse(decrement$age == 64, 0.3, 0))
  
  # decrement$qxr <- ifelse(decrement$age == 65, 1, 0)
  
  # decrement$qxt <- 0 # no terminations 
  #*************************************************************************************************************
  
  
  # Adjustment to the decrement table:
  # Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
  # For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
  # which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
  # whether they will die at r.max)      
  
  decrement %<>% group_by(ea) %>%  
    mutate(qxr = ifelse(age == r.max - 1,
                        1 - qxt - qxm - qxd, 
                        lead(qxr)*(1 - qxt - qxm - qxd)))
  
  
  ## define decrements for status and calculte survival probabilities. 
  decrement %<>% 
    # For active(".a"). 
    mutate(qxt.a   = ifelse(age >= r.max, 0, qxt),   # qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2),
           qxd.a   = ifelse(age >= r.max, 0, qxd),   # (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
           qxm.a   = ifelse(age >= r.max, 0, qxm),   # (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
           qxr.a   = qxr                             # ifelse(age == 64, (1 - qxt.p)*(1 - qxd.p)*(1 - qxm.p), 0)
    ) %>%
    
    # For terminated(".t"), target status are dead and retired.
    # Terminated workers will never enter the status of "retired". Rather, they will begin to receive pension benefits 
    # when reaching age r.max, but still with the status "terminated". So now we do not need qxr.t
    mutate(qxm.t   = qxm) %>%
    
    # For disabled(".d"), target status are dead. Note that we need to use the mortality for disabled 
    # Note the difference from the flows 3Darray.R. Disabled can not become retired here. 
    mutate(qxm.d = qxmd ) %>%
    
    # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
    mutate(qxm.r   = qxm.r) 
  
  
  
  # Calculate various survival probabilities
  decrement %<>% 
    mutate( pxm = 1 - qxm,
            pxm.r = 1 - qxm.r,
            pxT = 1 - qxt - qxd - qxm - qxr, #(1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
            pxRm = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm))), # prob of surviving up to r.max, mortality only
            px_r.full_m = order_by(-age, cumprod(ifelse(age >= r.full, 1, pxm)))
            # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
            # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
    )
  
  
  
  return(decrement)
}

decrement <- get_decrements()



saveRDS(decrement, "generated_data/decrement.rds")
saveRDS(salary, "generated_data/salary.rds")
saveRDS(init_pop, "generated_data/init_pop.rds")
saveRDS(init_sal, "generated_data/init_sal.rds")
saveRDS(benefit, "generated_data/benefit.rds")
saveRDS(entrants_dist, "generated_data/entrants.rds")

