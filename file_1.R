################################################################################
######################### BOYD & YIN - WORKFORCE PROJECTIONS ###################
################################################################################

### INSTALL OLD PACKAGES INSTRUCTIONS
# https://hansjoerg.me/2018/03/06/multiple-package-versions/

# detach("package:dplyr")
# detach("package:tibble")
# detach("package:magrittr")

library("dplyr", lib.loc = "/Library/Frameworks/R.framework/Versions/4.1/Resources/old_library")
library("tibble", lib.loc = "/Library/Frameworks/R.framework/Versions/4.1/Resources/old_library")
library("magrittr", lib.loc = "/Library/Frameworks/R.framework/Versions/4.1/Resources/old_library")


### CHECK PACKAGE VERSIONS
packageVersion("dplyr")
packageVersion("tibble")
packageVersion("magrittr")


actives <- readRDS("initial_data/actives.rds")
Global_paramlist <- readRDS("initial_data/Global_paramlist.rds")
paramlist <- readRDS("initial_data/paramlist.rds")
retirees <- readRDS("initial_data/retirees.rds")
salgrowth <- readRDS("initial_data/salgrowth.rds")

## Functions for model control
assign_parmsList <- function(paramlist, excludes = NULL, ...){
  varNames   <- setdiff(names(paramlist), excludes)
  assign_var <- function(x) assign(x, paramlist[[x]], ...)
  sapply(varNames, assign_var)
}


#*************************************************************************************************************
#                                       Tailoring Demographic Data                                       #####                  
#*************************************************************************************************************

tailor_demoData <- function(.paramlist = paramlist,
                            .Global_paramlist  = Global_paramlist,
                            .actives = actives,
                            .retirees = retirees){
  
  assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
  assign_parmsList(.paramlist,        envir = environment())
  
  .actives  %<>% filter(planname == paramlist$planname_actives,
                        ea  %in% range_ea,
                        age %in% range_ea)
  .retirees %<>% filter(planname == paramlist$planname_retirees,
                        age >= r.min)
  
  return(list(actives = .actives, retirees = .retirees))
}

tailored_demoData <-  tailor_demoData()




#*************************************************************************************************************
#                                        Create complete salary scale                                    #####                  
#*************************************************************************************************************

get_scale <- function(
  #.salgrowth.hist   = salgrowth.hist,
  #.salgrowth.assume = salgrowth.assume, 
  .salgrowth = salgrowth,
  .paramlist = paramlist,
  .Global_paramlist  = Global_paramlist){
  
  # This function generates a complete salary scale for all combos of starting year, entry ages and ages relevant to
  # to model. 
  # 
  # Salary levels at year 1 are set to 1. For future workers (entry year greater than 1) whose span of career years
  # do not include year 1, assumption about their starting salary levels is needed. Curretnly we assume starting salary
  # grows at inflation rate. 
  
  
  # Run the section below when developing new features. 
  #   .salgrowth        = salgrowth
  #   .paramlist = paramlist
  #   .Global_paramlist  = Global_paramlist
  
  
  assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
  assign_parmsList(.paramlist,        envir = environment())
  
  
  # sscale_hist   <- .salgrowth.hist   %>% filter(planname == .planname_sscale.hist) %>% select(-planname)
  # sscale_assume <- .salgrowth.assume %>% filter(planname == .planname_sscale.assume) %>% select(-planname)
  # Do not distinguish between sscale_hist and sscale_assume.
  
  sscale <- .salgrowth %>% filter(planname == planname_sscale) %>% select(-planname)
  
  SS.all <- expand.grid(start.year = (1 - (max.age - min.age)):nyear, ea = range_ea, age = min.age:(r.max - 1)) %>%
    filter(age >= ea, start.year + r.max - 1 - ea >= 1 ) %>% # workers must stay in workforce at least up to year 1.
    # arrange(start.year, ea, age) %>%
    mutate(yos = age - ea) %>%
    left_join(sscale) %>%
    group_by(start.year, ea) %>%
    mutate(year = start.year + (age - ea),
           growth.start = (1 + startingSal_growth)^(start.year - 1), # assume starting salary grows at the rate of inflation for all entry ages
           scale = cumprod(ifelse(age == ea, 1, lag(1 + salgrowth))), # salgrowth is from data salgrowth
           scale = ifelse(start.year <= 1, scale/scale[year == 1],
                          scale * growth.start)
    ) %>%
    select(start.year, ea, age, year, scale)
  
  return(SS.all)
  
  # return(sscale)
}

SS.all <- get_scale() %>% as_tibble()



################################################################################
################################################################################
################################################################################

saveRDS(SS.all, "generated_data/SS.all.rds")
saveRDS(tailored_demoData, "generated_data/tailored_demoData.rds")
