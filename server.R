#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2025-01-30
#'@email : amael.dupaix@mnhn.fr
#'#***************************************************************************
#'@description : Script gerant le serveur
#'               
#'#***************************************************************************



# Fichier pour gérer les interactions de l'application Shiny

# Paramètres d'entrée :
# input : liste de boutons, menus déroulants, curseurs... définis dans UI
# output : liste des affichages (tableaux, graphes, cartes...) définis dans UI


server <- function(input, output) {
    
    updateData <- reactive({
      if (input$rbIle == 1){
        df_cpue <- df_CPUE_A_pred_yr
        catch = df_SPA_cap_an %>% dplyr::filter(ILE=='Amsterdam') %>% 
          dplyr::group_by(AN) %>% dplyr::summarise(catch = sum(POIDS_LANDED)) %>%
          dplyr::mutate(AN = as.numeric(as.character(AN))) %>%
          as.data.frame()
        rbc <- TAC_SPA %>% filter(AN<=max(df_cpue$AN_num)+1) %>%
          as.data.frame() %>%
          dplyr::select (AN, TAC.JP.A, RBC.JP.A)
      } else if (input$rbIle == 2){
        df_cpue <- df_CPUE_SP_pred_yr
        catch = df_SPA_cap_an %>% dplyr::filter(ILE=='Saint Paul',
                                                   QUALITE!='bancdes16milles') %>% 
          dplyr::group_by(AN) %>% dplyr::summarise(catch = sum(POIDS_LANDED)) %>%
          dplyr::mutate(AN = as.numeric(as.character(AN))) %>%
          as.data.frame()
        rbc <- TAC_SPA %>% filter(AN<=max(df_cpue$AN_num)+1) %>%
          as.data.frame() %>%
          dplyr::select (AN, TAC.JP.SP, RBC.JP.SP)
      }
      
      catch %>%
        dplyr::bind_rows(data.frame(AN = max(catch$AN)+1,
                                    catch = 10**3*rbc$TAC[rbc$AN == max(catch$AN)+1])) -> catch
      
      m.period.calculation <- ifelse(input$rbAvePeriodCalc == 1, 'mean', 'worst_mean')
      m.period <- as.numeric(input$rbAvePeriod)
      var.limit.lo <- as.numeric(input$rbLimLow)
      var.limit.up <- as.numeric(input$rbLimHigh)
      
      colnames(rbc) = c('AN', 'TAC', 'RBC')

      cpue = df_cpue %>% dplyr::select(AN, pred.mean.CPUE) %>%
        dplyr::mutate(AN = as.numeric(as.character(AN))) %>%
        as.data.frame()
      colnames(cpue) = c('AN', 'CPUE')

      if (input$rbScenar == 1){
        n_years_sim <- 8
        cpue <- bind_rows(cpue, data.frame(AN = (max(cpue$AN)+1):(max(cpue$AN)+n_years_sim),
                                                 CPUE = (cpue %>% dplyr::filter(AN >= 1983, AN < 1983+n_years_sim))$CPUE))
      } else if (input$rbScenar == 2){
        n_years_sim <- 30
        cpue <- bind_rows(cpue, data.frame(AN = (max(cpue$AN)+1):(max(cpue$AN)+n_years_sim),
                                                 CPUE = cpue$CPUE[cpue$AN == max(cpue$AN)] - 2))
      } else if (input$rbScenar == 3){
        n_years_sim <- 30
        cpue <- bind_rows(cpue, data.frame(AN = (max(cpue$AN)+1):(max(cpue$AN)+n_years_sim),
                                           CPUE = cpue$CPUE[cpue$AN == max(cpue$AN)]-4 +
                                             2*sin((max(cpue$AN)+1):(max(cpue$AN)+n_years_sim)/2 +1.2)))
      }

      rbc_loop <- rbc
      catch_loop <- catch
      
      for (j in 1:(n_years_sim-1)){
        rbc_loop %>%
          add_row(AN=max(rbc_loop$AN)+1) -> rbc_loop
        
        k <- (max(rbc_loop$AN) - as.numeric(input$slideAppStart)) %% as.numeric(input$slideAppPeriod)
        
        # RBC ----------------------------------------------------------------
        df_HCR = rbc.fun(rbc=rbc_loop, cpue=cpue, catch=catch_loop,
                         ref.yrs, cur.yr, m.period, m.period.calculation,
                         var.limit.up, var.limit.lo, var.limit, ratio.cpue.lim,
                         buffer)
        
        #fill recommended biological catch and consider its adopted as TAC
        if (k != 0){
          rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN)] <- rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN) - 1]
          rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN)] <- rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN) - 1]
        } else {
          rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN)] <- round(df_HCR$RBC.rec)
          rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN)] <- round(df_HCR$RBC.rec)
        }
        

        # consider that fishermen take the whole TAC (no impact)
        catch_loop <- bind_rows(catch_loop,
                                data.frame(AN = max(catch_loop$AN)+1,
                                           catch = rbc_loop$TAC[rbc_loop$AN == max(catch_loop$AN)+1] * 10**3))
        cur.yr = cur.yr + 1
      }

      list(catch = catch_loop,
           rbc = rbc_loop,
           cpue = cpue,
           df_HCR = df_HCR)
      
    })
  
    
    ### Plots langoustes ----
    output$Captures <- renderPlot({
      
      tempList <- updateData()

      ggplot()+
        geom_histogram(data = tempList$catch,
                       aes(x = AN, y = catch/10**3),
                       stat = 'identity')+
      geom_line(data = tempList$rbc, aes(x=AN, y=TAC),
                color = "darkblue", linewidth = 1.5)+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.max, color = 'Limite'))+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.tar, color = 'Cible'))+
        scale_colour_manual(values = c("green","red"), name="")+
      scale_x_continuous("Années",
                         breaks = seq(1980, max(tempList$catch$AN), 5),
                         labels = function(y) paste0(y,"/",substring(y+1, 3)))+
      geom_vline(aes(xintercept = 2026))+
      ylab("Captures (t)")+
      theme(panel.background = element_rect(color = 'black', fill = 'white'),
            panel.grid = element_line(linetype = "dotted", colour = "grey"),
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 20))


    })
    
    output$Indicateur <- renderPlot({
      
      tempList <- updateData()
      
      ggplot() +
        geom_line(data = tempList$cpue %>% dplyr::filter(AN <= 2023),
                  aes(x=AN, y = CPUE)) +
        geom_line(data = tempList$cpue %>% dplyr::filter(AN >= 2023),
                  aes(x=AN, y = CPUE),
                  linetype = 'dashed') +
        geom_hline(data= tempList$df_HCR, aes(yintercept = cpue.lim, color='Limite'))+
        geom_hline(data= tempList$df_HCR, aes(yintercept = cpue.tar, color='Cible'))+
        scale_x_continuous("Années",
                           breaks = seq(1980, max(tempList$catch$AN), 5),
                           labels = function(y) paste0(y,"/",substring(y+1, 3)))+
        scale_colour_manual(values = c("green","red"), name="")+
        annotate("rect", xmin=c(as.numeric(as.character(ref.yrs[1]))),
                 xmax=c(as.numeric(as.character(ref.yrs[length(ref.yrs)]))),
                 ymin=0, ymax=max(tempList$cpue$CPUE), alpha=0.2, fill="blue")+
        ylab('Indice de biomasse (kg/casier)')+
        theme(panel.background = element_rect(fill = "white", colour = "black"),
              panel.grid = element_line(linetype = "dotted", colour = "grey"),
              legend.position = 'bottom',
              text = element_text(size = 20))
      
      
    })
    
    output$mainPlot <- renderPlot({
      
      tempList <- updateData()
      
      p_cap <- ggplot()+
        geom_histogram(data = tempList$catch,
                       aes(x = AN, y = catch/10**3),
                       stat = 'identity')+
        geom_line(data = tempList$rbc, aes(x=AN, y=TAC),
                  color = "darkblue", linewidth = 1.5)+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.max, color = 'Limite'))+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.tar, color = 'Cible'))+
        scale_colour_manual(values = c("green","red"), name="")+
        scale_x_continuous("Années",
                           breaks = seq(1980, max(tempList$catch$AN), 5),
                           limits = c(2015, NA),
                           labels = function(y) paste0(y,"/",substring(y+1, 3)))+
        geom_vline(aes(xintercept = 2026))+
        ylab("Captures (t)")+
        theme(panel.background = element_rect(color = 'black', fill = 'white'),
              panel.grid = element_line(linetype = "dotted", colour = "grey"),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 20))
      
      p_indic <- ggplot() +
        geom_line(data = tempList$cpue %>% dplyr::filter(AN <= 2023),
                  aes(x=AN, y = CPUE)) +
        geom_line(data = tempList$cpue %>% dplyr::filter(AN >= 2023),
                  aes(x=AN, y = CPUE),
                  linetype = 'dashed') +
        geom_hline(data= tempList$df_HCR, aes(yintercept = cpue.lim, color='Limite'))+
        geom_hline(data= tempList$df_HCR, aes(yintercept = cpue.tar, color='Cible'))+
        geom_vline(aes(xintercept = 2025))+
        scale_x_continuous("Années",
                           breaks = seq(1980, max(tempList$catch$AN), 5),
                           limits = c(2015, NA),
                           labels = function(y) paste0(y,"/",substring(y+1, 3)))+
        scale_colour_manual(values = c("green","red"), name="")+
        annotate("rect", xmin=c(as.numeric(as.character(ref.yrs[1]))),
                 xmax=c(as.numeric(as.character(ref.yrs[length(ref.yrs)]))),
                 ymin=0, ymax=max(tempList$cpue$CPUE), alpha=0.2, fill="blue")+
        ylab('Indice de biomasse (kg/casier)')+
        theme(panel.background = element_rect(fill = "white", colour = "black"),
              panel.grid = element_line(linetype = "dotted", colour = "grey"),
              legend.position = 'bottom',
              text = element_text(size = 20))
      
      
      ggarrange(p_indic, p_cap,
                nrow = 2, labels = "AUTO",
                common.legend = T)
      
    })

}
