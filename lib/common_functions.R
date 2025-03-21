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
