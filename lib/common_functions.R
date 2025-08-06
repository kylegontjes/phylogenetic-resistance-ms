# Common scripts
## Convert table one into df
convert_tableone_into_df <- function (dataset, vars, strata = NULL, argsNormal = NULL, factorVars = NULL,
          outcome_names = NULL)
{
  if (is.null(strata) == T) {
    overall <- CreateTableOne(vars = vars, data = dataset,
                              argsNormal = argsNormal, factorVars = factorVars)
    bound_table <- capture.output(x <- print(overall, quote = FALSE,
                                             noSpaces = TRUE))
    names <- x %>% as.matrix() %>% rownames
    values <- x %>% as.data.frame %>% `rownames<-`(NULL)
    table <- cbind.data.frame(names, x %>% as.data.frame()) %>%
      `rownames<-`(NULL)
    colnames(table) <- c("Variable", paste0("Overall (n=",
                                            table[1, 2], ")"))
    rownames(table) <- NULL
    table <- table[-1, ]
    return(table)
  }
  if (is.null(strata) == F) {
    overall <- CreateTableOne(vars = vars, data = dataset,
                              strata = strata, addOverall = T, argsNormal = argsNormal,
                              factorVars = factorVars)
    bound_table <- capture.output(x <- print(overall, quote = FALSE,
                                             noSpaces = TRUE))
    table <- x[, -ncol(x)] %>% as.data.frame()
    names <- x %>% as.matrix() %>% rownames
    table <- cbind.data.frame(names, table %>% as.data.frame()) %>%
      `rownames<-`(NULL)
    colnames(table) <- c("Variable", paste0(c("Overall",
                                              outcome_names), " (n=", table[1, 2:c(ncol(table) -
                                                                                     1)], ")"), "p-value")
    rownames(table) <- NULL
    table <- table[-1, ]
    return(table)
  }
}

get_legend <- function(plot){
  cowplot::get_plot_component(plot,"guide-box",return_all=T) %>% .[[3]]
}

# Create table 1, s table 8, and figure 3
print_final_model <- function(x,threshold=NULL){
  tables <- lapply(x,FUN=function(list){
    mv_table <- list[['final_model_table']] %>% arrange(p_value)

    if(is.null(threshold)==F){
      mv_table <- mv_table %>% subset(p_value < threshold)
    }

    # Format the tables
    mv_table$OR <- formatC(mv_table$OR, format = "f", digits = 2)
    mv_table$`2.5%` <- formatC(mv_table$`2.5%`, format = "f", digits = 2)
    mv_table$`97.5%` <-   formatC(mv_table$`97.5%`, format = "f", digits = 2)

    # Create OR (95% CI) variable
    mv_table$`OR (95% CI)` <- paste0(mv_table$OR," (",mv_table$`2.5%`,"-",mv_table$`97.5%`,")")
    mv_table <- mv_table %>% select(variable, `OR (95% CI)`, p_value)
    return(mv_table)
  })  %>% `names<-`(c("present","singleton","spread"))

  lapply(tables,function(y){
    if(y %>% as.data.frame %>% nrow()==0){
      paste0(names(y))
      cbind.data.frame(variable ='No model',`OR (95% CI)` = 'NA',p_value ='NA')
    }
    if(y %>% as.data.frame %>% nrow()>0){
      paste0(names(y))
      y %>% arrange(p_value)
    }
  })
}

cbind_na <- function(df_list) {
  max_rows <- max(lapply(df_list, nrow) %>% unlist)
  lapply(df_list,function(x){
    rbind(x,as.data.frame(matrix(NA,nrow = (max_rows - nrow(x)),ncol=ncol(x))) %>% mutate_all(as.character) %>% `colnames<-`(c("variable","OR (95% CI)","p_value")))
  }) %>% do.call(cbind,.)
}

get_list_of_components <- function(tbl){
  Present =  tbl$present.variable  %>% subset(.!='NA')
  Emergence = tbl$singleton.variable   %>% subset(.!='NA')
  Spread = tbl$spread.variable  %>% subset(.!='NA')
  results <- list(A_Present=Present,B_Emergence=Emergence,C_Spread=Spread)
  return(results)
}
