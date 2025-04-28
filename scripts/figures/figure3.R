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

create_mv_table <- function(datasets,lr_results){
  # Nons
  nons_modeling <- cbind.data.frame(datasets[[1]] ,lr_results[[1]]) %>%  as.data.frame %>% mutate(outcome = "present") %>% `rownames<-`(NULL)
  # Emergence
  emergence_modeling <- cbind.data.frame(datasets[[2]] ,lr_results[[2]])%>%  as.data.frame %>% mutate(outcome = "singleton")   %>% `rownames<-`(NULL)
  # Spread
  spread_modeling <- cbind.data.frame(datasets[[3]] ,lr_results[[3]]) %>%  as.data.frame %>% mutate(outcome = "cluster") %>% `rownames<-`(NULL)
  final_list <- rbind(nons_modeling,emergence_modeling,spread_modeling)
  return(final_list)
}

res_forest_plot <- function(tabletext,fp_rmetadata,outcome_name){
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
              xlab = "Adjusted Odds Ratio (aOR)",
              lwd.xaxis =2,
              vertices= TRUE,
              mar = unit(rep(0.5, times = 4), "mm"),
              txt_gp = fpTxtGp(ticks=gpar(cex=1),
                               xlab=gpar(cex=1))) %>% fp_add_header(agent = c("","Phenotype"),
                                                                    susceptible = c("","Susceptible"),
                                                                    feature = c("","Resistance category"),
                                                                    resistance = c("","Outcome"),
                                                                    OR = c("","aOR (95% CI)") %>% str_pad(.,width=19,side='right', pad = "\u00A0"))%>% fp_add_lines(h_3 = gpar(lwd=2),
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
