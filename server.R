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
        df_cpue <- cpue_A
        catch = captures %>% dplyr::filter(ILE=='Amsterdam') 
        
        rbc <- TAC_SPA %>% filter(AN<=max(df_cpue$AN)+1) %>%
          as.data.frame() 
        if (input$rbZone == 'both'){
          rbc %>%
            dplyr::select (AN, TAC.JP.A, RBC.JP.A) -> rbc
        } else if (input$rbZone == 'coastal'){
          rbc %>%
            dplyr::select (AN, TAC.JP.A.cotier, RBC.JP.A.cotier) -> rbc
        } else if (input$rbZone == 'deep'){
          rbc %>%
            dplyr::select (AN, TAC.JP.A.profond, RBC.JP.A.profond) -> rbc
        }
      } else if (input$rbIle == 2){
        df_cpue <- cpue_SP
        catch = captures %>% dplyr::filter(ILE=='Saint Paul',
                                                   QUALITE!='bancdes16milles')
        rbc <- TAC_SPA %>% filter(AN<=max(df_cpue$AN)+1) %>%
          as.data.frame()
        if (input$rbZone == 'both'){
          rbc %>%
            dplyr::select (AN, TAC.JP.SP, RBC.JP.SP) -> rbc
        } else if (input$rbZone == 'coastal'){
          rbc %>%
            dplyr::select (AN, TAC.JP.SP.cotier, RBC.JP.SP.cotier) -> rbc
        } else if (input$rbZone == 'deep'){
          rbc %>%
            dplyr::select (AN, TAC.JP.SP.profond, RBC.JP.SP.profond) -> rbc
        }
      }
      if (input$rbZone != 'both'){
        catch %>%
          dplyr::filter(QUALITE == ifelse(input$rbZone == 'coastal',
                                          'cotiere',
                                          'profonde')) -> catch
      }
      
      colnames(rbc) = c('AN', 'TAC', 'RBC')
      
      catch %>%
        dplyr::group_by(AN) %>%
        dplyr::summarise(catch = sum(POIDS_LANDED)) %>%
        dplyr::mutate(AN = as.numeric(as.character(AN))) %>%
        as.data.frame() %>%
        dplyr::bind_rows(data.frame(AN = max(.$AN)+1,
                                    catch = 10**3*rbc$TAC[rbc$AN == max(.$AN)+1])) -> catch
      
      m.period.calculation <- ifelse(input$rbAvePeriodCalc == 1, 'mean', 'worst_mean')
      m.period <- as.numeric(input$rbAvePeriod)
      var.limit.lo <- -as.numeric(input$sliderLimLow)/100
      var.limit.up <- as.numeric(input$sliderLimHigh)/100

      cpue = df_cpue %>%
        dplyr::filter(ZONE == input$rbZone) %>%
        dplyr::select(AN, pred.mean.CPUE) %>%
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
        n_years_sim <- 8
        # cpue %>%
        #   filter(AN %in% ref.yrs) -> cpue_tar
        # cpue_tar <- mean(cpue_tar$CPUE)
        cpue <- bind_rows(data.frame(AN = (max(cpue$AN)-30):max(cpue$AN),
                                     CPUE = 10),
                          data.frame(AN = (max(cpue$AN)+1):(max(cpue$AN)+n_years_sim),
                                     CPUE = 10 * 0.85 ** seq(1,n_years_sim,1)))
        catch %>%
          dplyr::filter(AN %in% ref.yrs) -> catch_tar
        catch$catch <- mean(catch_tar$catch)
      }
      # } else if (input$rbScenar == 3){
      #   n_years_sim <- 30
      #   cpue <- bind_rows(cpue, data.frame(AN = (max(cpue$AN)+1):(max(cpue$AN)+n_years_sim),
      #                                      CPUE = cpue$CPUE[cpue$AN == max(cpue$AN)]-4 +
      #                                        2*sin((max(cpue$AN)+1):(max(cpue$AN)+n_years_sim)/2 +1.2)))
      # }

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
        
        #fill recommended biological catch and consider it's adopted as TAC
        if (k != 0){ # the HCR is not applied this year
          if (input$rbSortie){ # if there is a mecanism to potentially apply the HCR earlier
            # get the last cpue year when it was applied
            last_application_year <- cur.yr - k
            # compare the current biomass indicator with the biomass indicator on that year
            cur.cpue <- df_HCR$cpue.recent
            last_app.cpue <- rbc.fun(rbc=rbc_loop, cpue=cpue, catch=catch_loop,
                                     ref.yrs, last_application_year,
                                     m.period, m.period.calculation,
                                     var.limit.up, var.limit.lo, var.limit, ratio.cpue.lim,
                                     buffer)$cpue.recent
            if (cur.cpue / last_app.cpue < (1 - input$sliderPourcentSortie/100)){
              rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN)] <- round(df_HCR$RBC.rec)
              rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN)] <- round(df_HCR$RBC.rec)
            } else {
              rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN)] <- rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN) - 1]
              rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN)] <- rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN) - 1]
            }
          } else {
            rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN)] <- rbc_loop$TAC[rbc_loop$AN == max(rbc_loop$AN) - 1]
            rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN)] <- rbc_loop$RBC[rbc_loop$AN == max(rbc_loop$AN) - 1]
          }
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
           df_HCR = df_HCR,
           scenario = input$rbScenar)
      
    })
  
    
    ### Plots langoustes ----
    output$Captures <- renderPlot({
      
      tempList <- updateData()

      ggplot()+
        geom_histogram(data = tempList$catch,
                       aes(x = AN, y = catch/10**3),
                       stat = 'identity')+
      # geom_line(data = tempList$rbc, aes(x=AN, y=TAC),
      #           color = "darkblue", linewidth = 1.5)+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.max, color = 'Limite'))+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.tar, color = 'Cible'))+
        scale_colour_manual(values = c("green","red"), name="")+
      scale_x_continuous("Années",
                         breaks = seq(1980, max(tempList$catch$AN), 10),
                         labels = function(y) paste0(y,"/",substring(y+1, 3)))+
      geom_vline(aes(xintercept = 2026))+
      ylab("Captures (t)")+
      theme(panel.background = element_rect(color = 'black', fill = 'white'),
            panel.grid = element_line(linetype = "dotted", colour = "grey"),
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 18))


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
                           breaks = seq(1980, max(tempList$catch$AN), 10),
                           labels = function(y) paste0(y,"/",substring(y+1, 3)))+
        scale_colour_manual(values = c("green","red"), name="")+
        annotate("rect", xmin=c(as.numeric(as.character(ref.yrs[1]))),
                 xmax=c(as.numeric(as.character(ref.yrs[length(ref.yrs)]))),
                 ymin=0, ymax=max(tempList$cpue$CPUE), alpha=0.2, fill="blue")+
        ylab('Indice de biomasse (kg/casier)')+
        theme(panel.background = element_rect(fill = "white", colour = "black"),
              panel.grid = element_line(linetype = "dotted", colour = "grey"),
              legend.position = 'bottom',
              text = element_text(size = 18))
      
      
    })
    
    output$mainPlot <- renderPlot({
      
      tempList <- updateData()
      
      if (tempList$scenario == "1" & tempList$catch[tempList$catch$AN == 2026,'catch']/10**3 > 1.05 * tempList$df_HCR$catch.tar){
        panel_color <- "#950606"
        panel_width <- 2
      } else if (tempList$scenario == "1" & tempList$catch[tempList$catch$AN == 2026,'catch']/10**3 <= 1.05 * tempList$df_HCR$catch.tar) {
        panel_color <- "darkgreen"
        panel_width <- 2
      } else {
        panel_color <- 'grey40'
        panel_width <- .5
      }
      
      p_cap <- ggplot()+
        geom_histogram(data = tempList$catch %>%
                         dplyr::mutate(col = AN == 2026),
                       aes(x = AN, y = catch/10**3,
                           fill = col),
                       stat = 'identity')+
        scale_fill_manual(values = c('grey40', panel_color))+
        # geom_line(data = tempList$rbc, aes(x=AN, y=TAC),
        #           color = "darkblue", linewidth = 1.5)+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.max, color = 'Limite'))+
        geom_hline(aes(yintercept = tempList$df_HCR$catch.tar, color = 'Cible'))+
        scale_colour_manual(values = c("green","red"), name="")+
        scale_x_continuous("Années",
                           breaks = seq(1980, max(tempList$catch$AN), 5),
                           limits = c(2015, NA),
                           labels = function(y) paste0(y,"/",substring(y+1, 3)))+
        ylab("Captures (t)")+
        theme(panel.background = element_rect(color = panel_color, fill = 'white',
                                              linewidth = panel_width),
              panel.grid = element_line(linetype = "dotted", colour = "grey"),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15))+
        guides(fill = 'none')
      
      p_indic <- ggplot() +
        geom_line(data = tempList$cpue %>% dplyr::filter(AN <= 2023),
                  aes(x=AN, y = CPUE)) +
        geom_line(data = tempList$cpue %>% dplyr::filter(AN >= 2023),
                  aes(x=AN, y = CPUE),
                  linetype = 'dashed') +
        geom_hline(data= tempList$df_HCR, aes(yintercept = cpue.lim, color='Limite'))+
        geom_hline(data= tempList$df_HCR, aes(yintercept = cpue.tar, color='Cible'))+
        scale_x_continuous("Années",
                           breaks = seq(1980, max(tempList$catch$AN), 5),
                           limits = c(2015, NA),
                           labels = function(y) paste0(y,"/",substring(y+1, 3)))+
        scale_colour_manual(values = c("green","red"), name="")+
        annotate("rect", xmin=c(as.numeric(as.character(ref.yrs[1]))),
                 xmax=c(as.numeric(as.character(ref.yrs[length(ref.yrs)]))),
                 ymin=0, ymax=max(tempList$cpue$CPUE), alpha=0.2, fill="blue")+
        ylab('Indice de biomasse (kg/casier)')+
        theme(panel.background = element_rect(fill = "white", colour = 'black'),
              panel.grid = element_line(linetype = "dotted", colour = "grey"),
              legend.position = 'bottom',
              text = element_text(size = 15))
      
      if (tempList$scenario == 1){
        p_indic <- p_indic + geom_vline(aes(xintercept = 2025))
        p_cap <- p_cap + geom_vline(aes(xintercept = 2026))
      } else if (tempList$scenario == 3){
        p_indic <- p_indic + geom_vline(aes(xintercept = 2028))
        p_cap <- p_cap + geom_vline(aes(xintercept = 2029))
      }
      
      ggarrange(p_indic, p_cap,
                nrow = 2,
                common.legend = T)
      
    })

}
