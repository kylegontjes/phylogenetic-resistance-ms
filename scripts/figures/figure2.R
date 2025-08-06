# Functions used for figure 2
generate_descriptive_stats <- function(drug,datasets,variables,factorVars){
  non_s_df <- datasets[['present']] %>% as.data.frame
  emerge_df <- datasets[['singleton']] %>% as.data.frame
  spread_df <- datasets[['cluster']]  %>% as.data.frame
  pheno  <- paste0(drug,"_dich_num")

  descriptive_non_s <- convert_tableone_into_df(dataset = non_s_df ,vars = variables,strata=pheno,factorVars = factorVars,outcome_names = c("Susceptible","Non-Susceptible")) %>% mutate(variable = variables) %>% select(-Variable) %>% select(variable,everything()) %>% `rownames<-`(NULL) %>% `colnames<-`(c("variable","overall","susceptible","non_susceptible","p-value")) %>% select(-`p-value`)
  descriptive_emerge <- convert_tableone_into_df(dataset =  emerge_df,vars=variables,strata=pheno,factorVars = factorVars,outcome_names = c("Susceptible","Non-Susceptible Emergence"))  %>% mutate(variable = variables) %>% select(-Variable) %>% select(variable,everything())  %>% `rownames<-`(NULL) %>% `colnames<-`(c("variable","overall","susceptible","non_susceptible","p-value")) %>% select(-`p-value`)
  descriptive_spread <- convert_tableone_into_df(dataset = spread_df,vars = variables,strata=paste0(drug,"_dich_num"),factorVars = factorVars,outcome_names = c("Susceptible","Non-Susceptible Spread"))  %>% mutate(variable = variables) %>% select(-Variable) %>% select(variable,everything()) %>% `rownames<-`(NULL) %>% `colnames<-`(c("variable","overall","susceptible","non_susceptible","p-value")) %>% select(-`p-value`)

  descriptives_list <- list(descriptive_non_s,descriptive_emerge,descriptive_spread) %>% `names<-`(c("present","singleton","cluster"))
  return(descriptives_list)
}

get_mv_table_adjusted <- function(descriptives,lr_results){
  # Nons
  nons_modeling <- left_join(descriptives[[1]] ,lr_results[[1]]) %>%  as.data.frame %>% mutate(outcome = "present") %>% subset(!rownames(.) %in% c("age","sex")) %>% `rownames<-`(NULL)
  # Emergence
  emergence_modeling <- left_join(descriptives[[2]] ,lr_results[[2]])%>%  as.data.frame %>% mutate(outcome = "singleton")  %>% subset(!rownames(.) %in% c("age","sex"))   %>% `rownames<-`(NULL)
  # Spread
  spread_modeling <- left_join(descriptives[[3]] ,lr_results[[3]]) %>%  as.data.frame %>% mutate(outcome = "cluster") %>% subset(!rownames(.) %in% c("age","sex"))  %>% `rownames<-`(NULL)

  # Get final multivariable table
  final_list <- rbind.data.frame(nons_modeling,emergence_modeling,spread_modeling)

  # Format the tables
  final_list$OR <- formatC(final_list$OR, format = "f", digits = 2)
  final_list$`2.5%` <- formatC(final_list$`2.5%`, format = "f", digits = 2)
  final_list$`97.5%` <-   formatC(final_list$`97.5%`, format = "f", digits = 2)

  # Create OR (95% CI) variable
  final_list$`OR (95% CI)` <- paste0(final_list$OR," (",final_list$`2.5%`,"-",final_list$`97.5%`,")")
  return(final_list)
}

create_mv_table <- function(descriptives,lr_results){
  # Nons
  nons_modeling <- suppressMessages(left_join(descriptives[[1]] ,lr_results[[1]]) %>%  as.data.frame %>% mutate(outcome = "present") %>% `rownames<-`(NULL))
  # Emergence
  emergence_modeling <- suppressMessages(left_join(descriptives[[2]] ,lr_results[[2]])%>%  as.data.frame %>% mutate(outcome = "singleton")   %>% `rownames<-`(NULL))
  # Spread
  spread_modeling <- suppressMessages(left_join(descriptives[[3]] ,lr_results[[3]]) %>%  as.data.frame %>% mutate(outcome = "cluster") %>% `rownames<-`(NULL))
  # Create final multivariable table
  final_list <- rbind.data.frame(nons_modeling,emergence_modeling,spread_modeling)

  # Format the tables
  final_list$OR <- formatC(final_list$OR, format = "f", digits = 2)
  final_list$`2.5%` <- formatC(final_list$`2.5%`, format = "f", digits = 2)
  final_list$`97.5%` <-   formatC(final_list$`97.5%`, format = "f", digits = 2)

  # Create OR (95% CI) variable
  final_list$`OR (95% CI)` <- paste0(final_list$OR," (",final_list$`2.5%`,"-",final_list$`97.5%`,")")

  return(final_list)
}

get_table_text <- function(TMP_SMX_results,gentamicin_results,AMK_results,CST_results,blbli_results){
  agent_string <- c("TMP-SMX","","","","Gentamicin","","","","Amikacin","","","","Colistin","","","","BL/BLI","","","")
  susceptible_string <- c("",subset(TMP_SMX_results,outcome=="present") %>% .$susceptible,"","","",subset(gentamicin_results,outcome=="present") %>% .$susceptible,"","","",subset(AMK_results,outcome=="present") %>% .$susceptible,"","","",subset(CST_results,outcome=="present") %>% .$susceptible,"","","",subset(blbli_results,outcome=="present") %>% .$susceptible,"","")
  feature_string <- c("","Presence","Emergence","Spread","","Presence","Emergence","Spread","","Presence","Emergence","Spread","","Presence","Emergence","Spread","","Presence","Emergence","Spread")
  resistance_string <- c("",TMP_SMX_results$non_susceptible,"",gentamicin_results$non_susceptible,"",AMK_results$non_susceptible,"",CST_results$non_susceptible,"",blbli_results$non_susceptible)
  OR_string <- c('',TMP_SMX_results$`OR (95% CI)`,"",gentamicin_results$`OR (95% CI)`,"",AMK_results$`OR (95% CI)`,"",CST_results$`OR (95% CI)`,"",blbli_results$`OR (95% CI)`) %>% str_pad(.,width=19,side='right', pad = "\u00A0")

  tabletext <- cbind.data.frame(agent_string,susceptible_string,feature_string,resistance_string,OR_string) %>% `colnames<-`(c("agent","susceptible","feature","resistance","OR"))
  return(tabletext)

}

get_fp_rmetadata <- function(TMP_SMX_results,gentamicin_results,AMK_results,CST_results,blbli_results){
  OR_stat <- c("",TMP_SMX_results$`OR (95% CI)`,"",gentamicin_results$`OR (95% CI)`,"",AMK_results$`OR (95% CI)`,"",CST_results$`OR (95% CI)`,"",blbli_results$`OR (95% CI)`)
  OR <- c("",TMP_SMX_results$OR,"",gentamicin_results$OR,"",AMK_results$OR,"",CST_results$OR,"",blbli_results$OR)
  lower <- c("",TMP_SMX_results$`2.5%`,"",gentamicin_results$`2.5%`,"",AMK_results$`2.5%`,"",CST_results$`2.5%`,"",blbli_results$`2.5%`)
  upper <- c("",TMP_SMX_results$`97.5%`,"",gentamicin_results$`97.5%`,"",AMK_results$`97.5%`,"",CST_results$`97.5%`,"",blbli_results$`97.5%`)

  fp_rmetadata <- cbind.data.frame(OR,lower,upper)  %>% mutate_all(as.numeric)
  return(fp_rmetadata)
}


res_forest_plot <- function(tabletext,fp_rmetadata,outcome_name,type_of_regression){
  forestplot( tabletext,
              mean = fp_rmetadata$OR,
              lower = fp_rmetadata$lower,
              upper = fp_rmetadata$upper,
              new_page = TRUE,
              nameorg=gpar(fontface=10),
              xlim = c(0.1,5),
              boxsize = .3,
              xlog=TRUE,
              col=fpColors(box="black",line="black"),
              graphwidth = unit(100,'mm'),
              lwd.zero = (2),
              align = c("l", "l", "l","l","l"),
              colgap = unit(2.75,"mm"),
              xlab = ifelse(type_of_regression =="Adjusted","Adjusted Odds Ratio (aOR)","Unadjusted Odds Ratio (OR)"),
              lwd.xaxis =2,
              vertices= TRUE,
              mar = unit(rep(0.5, times = 4), "mm"),
              txt_gp = fpTxtGp(ticks=gpar(cex=1),
                               xlab=gpar(cex=1))) %>% fp_add_header(agent = c("","Phenotype"),
                                                                    susceptible = c("","Susceptible"),
                                                                    feature = c("","Resistance Category"),
                                                                    resistance = c("","Outcome"),
                                                                    OR = c("",ifelse(type_of_regression =="Adjusted","aOR (95% CI)","OR (95% CI)")) %>% str_pad(.,width=19,side='right', pad = "\u00A0"))%>% fp_add_lines(h_3 = gpar(lwd=2),
                                                                                                              h_4 = gpar(columns = 1:5),
                                                                                                              h_8 = gpar(columns = 1:5),
                                                                                                              h_12 = gpar(columns = 1:5),
                                                                                                              h_16 = gpar(columns = 1:5),
                                                                                                              h_20 = gpar(columns = 1:5),
                                                                                                              h_23 = gpar(lwd=2)) %>%
    fp_set_style(txt_gp = fpTxtGp(xlab = gpar(cex = 1, fontface = "bold"),
                                  ticks = gpar(fontfamily = "", cex = 1),
                                  label = list(gpar(fontface = "bold"),
                                               gpar(),
                                               gpar(),
                                               gpar(),
                                               gpar())))
}
