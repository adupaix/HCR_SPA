####################################"
### HCR JP SPA functions
####################################"

# HCR from SESSF harvest strategy seee Little et al. 2011 ---------------------

rbc.fun <- function(rbc, cpue, catch,
                    ref.yrs, cur.yr, m.period,
                    var.limit.up, var.limit.lo, var.limit, ratio.cpue.lim,
                    buffer){
  
  
  first.yrs = (cur.yr - m.period + 1) :cur.yr# recent years
  
  # Reco last year 
  rbc.last = rbc %>% filter(AN==cur.yr) %>% dplyr::select(contains('RBC'))
  tac.last = rbc %>% filter(AN==cur.yr) %>% dplyr::select(contains('TAC'))

  # Cpue recent mean (4 years window)
  cpue.recent = cpue %>% filter(AN %in% first.yrs) %>% ungroup()%>% dplyr::summarise(CPUE = mean(CPUE))

  # CPUE limit, target
  cpue.limTar = cpue %>% filter(AN %in% ref.yrs) %>% ungroup()%>%
    dplyr::summarise(cpue.tar = mean(CPUE)) %>%
    mutate(cpue.lim = ratio.cpue.lim * cpue.tar)

  # Catch target
  catch.tar =  catch %>% filter(AN %in% ref.yrs) %>%
    ungroup() %>%  dplyr::summarise(catch.tar = mean(catch)*10^(-3))

  catch.max = max(catch$catch)*10^(-3)

  # df
  df_ref = data.frame(cpue.recent = cpue.recent$CPUE, rbc.last = as.numeric(rbc.last),
                      tac.last=as.numeric(tac.last),
                     cpue.lim = cpue.limTar$cpue.lim, cpue.tar = cpue.limTar$cpue.tar,
                     catch.tar=catch.tar$catch.tar,
                     catch.max=catch.max)

  # Scaling factor SF and Recommended Biological Catch (RBC)
  RBC.SF = hcr.cpue(cpue.recent= cpue.recent, cpue.lim=df_ref$cpue.lim,
           cpue.tar=df_ref$cpue.tar,
           catch.tar= catch.tar,
           buffer)

  df_ref = df_ref %>%
    mutate(catch.tar= (1-buffer) * catch.tar,
           cpue.tar= (1+buffer) * cpue.tar,
           RBC = RBC.SF$RBC ,
           SF =  RBC.SF$SF)

  # Limit variation
  df_ref = df_ref %>% rowwise() %>%
    mutate(RBC.rec= ifelse(RBC >= catch.max  & (1+var.limit.up) *
                             tac.last >=catch.max,
                           catch.max,
                           ifelse(  RBC >= (1+var.limit.up) *
                                      tac.last,
                                    (1+var.limit.up[1]) * tac.last,
                                    ifelse( RBC <= (1-var.limit.lo) * tac.last & RBC > (1-var.limit)*tac.last,
                                            (1-var.limit.lo) *  tac.last,
                                            RBC  )))
    )

  return(df_ref)
  
}




# HCR main function ----------------------------------------------------------

  hcr.cpue <- function(cpue.recent, cpue.lim , cpue.tar, catch.tar, buffer){

  
  SF= (cpue.recent - cpue.lim) / (  ((1+buffer)*cpue.tar) - cpue.lim)
  
  RBC = SF * ((1-buffer)* catch.tar)
  
  return(data.frame( RBC=as.numeric(RBC) , SF=as.numeric(SF) ))
  }
  
  
  
  hcr.cpue.lim <- function(cpue.recent, cpue.lim , cpue.tar, catch.tar, buffer, 
                           RBClag, var.limit.up, var.limit.lo, var.limit ){
    
    SF=numeric(length(cpue.recent))
    RBC=numeric(length(cpue.recent))
    
    for(i in 1:length(cpue.recent)){
     SF[i]= (cpue.recent[i] - cpue.lim) / (  ((1+buffer)* cpue.tar) - cpue.lim)
    
     RBC[i] = SF[i] * ((1-buffer)* catch.tar)
    
     if(RBC[i] > RBClag[i]*(1+var.limit.up) & !is.na(RBClag[i])) {
       RBC[i] = RBClag[i]*(1+var.limit.up)
     } else if(RBC[i] < RBClag[i]*(1-var.limit.lo) & RBC[i] > RBClag[i]*(1-var.limit) & !is.na(RBClag[i])){
       RBC[i] = RBClag[i]*(1-var.limit.lo)
     }
    }
    
    return(data.frame( RBC=as.numeric(RBC) , SF=as.numeric(SF) ))
  }

