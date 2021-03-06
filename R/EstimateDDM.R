#' Estimate death registration between two Census years with a wrapper of the ddm() function from the DDM
#'
#' asdf
#' 
#' @param data data frame that contains at least seven columns representing: (1) five-year age groups, 
#' (2) sex,
#' (3, 4) population counts collected at two different time points (typically adjacent Census years)
#' (5, 6) dates of two different time points
#' (7) the level of subnational disaggregation in additino to sex (e.g. a geographic unit such as a province/state, 
#' a sociodemographic category such as education level, or combinations thereof). 
#' @param name.disaggregations Character string providing the name of the variable in `data` that represents the levels of subnational disaggregation
#' @param name.age Character string providing the name of the variable in `data` that represents age
#' @param name.sex Character string providing the name of the variable in `data` that represents sex
#' @param name.males Character string providing the name of the value of `name.sex` variable that represents males
#' @param name.females Character string providing the name of the value of `name.sex` variable that represents females
#' @param name.population.year1 Character string providing the name of the variable in `data` that represents the population count in the earlier time point
#' @param name.population.year2 Character string providing the name of the variable in `data` that represents the population count in the later time point
#' @param name.year1 Character string providing the name of the variable in `data` that represents the year of the earlier of the two time periods (e.g. year of the earlier Census)
#' @param name.month1 Character string providing the name of the variable in `data` that represents the month of the earlier of the two time periods (e.g. month of the earlier Census)
#' @param name.day1 Character string providing the name of the variable in `data` that represents the day of the earlier of the two time periods (e.g. day of the earlier Census)
#' @param name.year2 Character string providing the name of the variable in `data` that represents the year of the later of the two time periods (e.g. year of the later Census)
#' @param name.month2 Character string providing the name of the variable in `data` that represents the month of the later of the two time periods (e.g. month of the later Census)
#' @param name.day2 Character string providing the name of the variable in `data` that represents the day of the later of the two time periods (e.g. day of the later Census)
#' @param name.national A character string providing the value of `name.disaggregations` variable that indicates national-level results (e.g. "Overall" or "National"). Defaults to NULL, implying `name.disaggregations` variable in `data` only includes values for subnational levels. Defaults to NULL#' @param name.deaths Character string providing the name of the variable in `data` that represents the total count or annual average count of deaths between the earlier and later time points
#' @param deaths.summed A logical equivalent to the `deaths.summed` argument of `DDM::ddm()`, which indicates whether `name.deaths` provides the total count (TRUE) or annual average count (FALSE) of deaths between the two time points
#' @param show.age.range.sensitivity A logical equal to TRUE if the DDM estimates are provided for every possible age range (obeying the `min.age.in.search`, `max.age.in.search`, and `min.number.of.ages` arguments) and equal to FALSE are only provided for the optimal age range based on the search performed by ddm(). Defaults to TRUE
#' @param min.age.in.search A numeric equivalent to the `minA` argument of `DDM:ddm()`. Defaults to 15
#' @param max.age.in.search A numeric equivalent to the `maxA` argument of `DDM:ddm()`. Defaults to 75
#' @param min.number.of.ages A numeric equivalent to the `minAges` argument of `DDM:ddm()`. Defaults to 8
#' @param exact.ages.to.use A numeric vector equivalent to `exact.ages`. Defaults to NULL, which feeds the default value of NULL to `DDM:ddm()`
#' @param largest.lower.limit.sensitivity A numeric the indicates the largest value of `min.age.in.search` that should be considered in the sensitivity analysis (the smallest one considered will be the one specified originally with `min.age.in.search`). Defaults to 45
#' @param smallest.upper.limit.sensitivity A numeric the indicates the smallest value of `max.age.in.search` that should be considered in the sensitivity analysis (the largest one considered will be the one specified originally with `max.age.in.search`). Defaults to 50
#' @param life.expectancy.in.open.group A numeric equivalent to the `eOpen` argument of `DDM:ddm()`. Defaults to NULL
#' @examples
#' ddm_results <- EstimateDDM(data=ecuador_five_year_ages, 
#'                            name.disaggregations="province_name",
#'                            name.age="age",
#'                            name.sex="sex",
#'                            name.males="m",
#'                            name.females="f",
#'                            name.population.year1="pop1",
#'                            name.population.year2="pop2",
#'                            name.year1="year1"
#'                            name.month1="month1",
#'                            name.day1="day1",
#'                            name.year2="year2",
#'                            name.month2="month2",
#'                            name.day2="day2"
#'                            name.deaths="deaths",
#'                            deaths.summed=TRUE,
#'                            min.age.in.search=15,
#'                            max.age.in.search=75,
#'                            min.number.of.ages=8,
#'                            life.expectancy.in.open.group=NULL,
#'                            exact.ages.to.use=NULL,
#'                            show.age.range.sensitivity=FALSE)
#' ddm_results$ddm_estimates                            
#' @import dplyr
#' @import DDM
#' @export

EstimateDDM <- function(data, 
                        name.disaggregations,
                        name.age,
                        name.sex,
                        name.males,
                        name.females,
                        name.population.year1,
                        name.population.year2,
                        name.year1,
                        name.month1,
                        name.day1,
                        name.year2,
                        name.month2,
                        name.day2,
                        name.national=NULL,
                        name.deaths,
                        deaths.summed, # should not have a default
                        show.age.range.sensitivity=TRUE,
                        min.age.in.search=15,
                        max.age.in.search=75,
                        min.number.of.ages=8,
                        exact.ages.to.use=NULL,
                        largest.lower.limit.sensitivity=35,
                        smallest.upper.limit.sensitivity=40,
                        life.expectancy.in.open.group=NULL) {
  if (!is.data.frame(data)) {
    stop("the dataset provided in the 'data' argument needs to be a data frame")
  }
  if (is.null(name.national) == FALSE) {
    if (name.national %in% unique(data[, name.disaggregations]) == FALSE) {
      stop(paste("The value",
                 name.national,
                 "was not found in the variable",
                 name.disaggregations))
    }
  }
  data <- CreateDateVariable(data=data,
                             name.disaggregations=name.disaggregations,
                             name.year1=name.year1,
                             name.month1=name.month1,
                             name.day1=name.day1,
                             name.year2=name.year2,
                             name.month2=name.month2,
                             name.day2=name.day2)
  # re-formatting variables (this is the key purpose because the ddm() function is very picky)
  data_formatted <- FormatVariablesDDM(data=data, 
                     name.disaggregations=name.disaggregations,
                     name.age=name.age,
                     name.sex=name.sex,
                     name.males=name.males,
                     name.females=name.females,
                     name.population.year1=name.population.year1,
                     name.population.year2=name.population.year2,
                     name.year1=name.year1,
                     name.month1=name.month1,
                     name.day1=name.day1,
                     name.year2=name.year2,
                     name.month2=name.month2,
                     name.day2=name.day2,
                     name.deaths=name.deaths)
  data_for_ddm_females <- data_formatted$data_for_ddm_females
  data_for_ddm_males <- data_formatted$data_for_ddm_males
  # estimate completeness with ddm() function from DDM package
  ## males
  result_ddm_males <- ddm(X=data_for_ddm_males,
                    deaths.summed=deaths.summed,
                    minA=min.age.in.search,
                    maxA=max.age.in.search,
                    minAges=min.number.of.ages,
                    exact.ages=exact.ages.to.use,
                    eOpen=life.expectancy.in.open.group)
  
  ## females
  result_ddm_females <- ddm(X=data_for_ddm_females,
                          deaths.summed=deaths.summed,
                          minA=min.age.in.search,
                          maxA=max.age.in.search,
                          minAges=min.number.of.ages,
                          exact.ages=exact.ages.to.use,
                          eOpen=life.expectancy.in.open.group)
  # summarize and refomat results of call to ddm()
  ddm_estimates <- FormatOutputDDM(result_ddm_females=result_ddm_females,
                                      result_ddm_males=result_ddm_males)
  # also provide total population counts
  data_with_total_pop <- data %>%
                         group_by(get(name.disaggregations)) %>%
                         summarise("total_pop1"=sum(get(name.population.year1), na.rm=TRUE),
                                   "total_pop2"=sum(get(name.population.year2), na.rm=TRUE)) %>%
                         as.data.frame()
  names(data_with_total_pop)[names(data_with_total_pop) == "get(name.disaggregations)"] <- "cod"
  data_with_total_pop$cod <- as.factor(data_with_total_pop$cod)
  ddm_estimates <- left_join(ddm_estimates,
                             data_with_total_pop,
                             by="cod")
                             
  if (show.age.range.sensitivity == TRUE) {
    # perform DDM estimation for a sequence of age-range parameters to give a sense of estimation sensitivity
    # setting up before loop
    lower.limits.age.sensitivity <- seq(from=min.age.in.search,
                                        to=largest.lower.limit.sensitivity,
                                        by=5)
    upper.limits.age.sensitivity <- seq(from=smallest.upper.limit.sensitivity,
                                        to=max.age.in.search,
                                        by=5)
    possible_age_range_endpoints <- expand.grid(lower.limits.age.sensitivity,
                                       upper.limits.age.sensitivity)
    possible_age_range_sequences <- apply(possible_age_range_endpoints, 
                                          1, 
                                          function(x) seq(from=x[1], 
                                                           to=x[2], 
                                                           by=5))
    idx_acceptable_age_range_sequences <- sapply(possible_age_range_sequences,
                                                 function(x) length(x) >= min.number.of.ages)
                                                 #function(x) length(x) >= 8)
    acceptable_age_range_sequences <- possible_age_range_sequences[idx_acceptable_age_range_sequences]
    n_age_combinations <- length(acceptable_age_range_sequences)
    
    # storing summaries of sensitivity (point estinates and RMSE by possible age range)
    sensitivity_ddm_estimates <- matrix(NA, 
                                        nrow=1, 
                                        ncol=(ncol(ddm_estimates) + 1))
    colnames(sensitivity_ddm_estimates) <- c(colnames(ddm_estimates), "RMSE_ggb")
    
    # performing GGB-SEG estimation across all combinations of exact.ages sequences
    n_cod <- length(unique(ddm_estimates$cod))
    print(paste("estimating completeness of adult death registration completeness",
                "with GGB, SEG, and GGB-SEG methods within each of",
                n_age_combinations, 
                "possible age ranges within each of",
                n_cod,
                "levels of subnational disaggregations..."))
    for (seq in 1:length(acceptable_age_range_sequences)) {
      one_exact_ages <- acceptable_age_range_sequences[[seq]]
      ## point estimates for males and females
      one_ddm_females <- ddm(X=data_for_ddm_females,
                             deaths.summed=deaths.summed,
                             exact.ages=one_exact_ages,
                             eOpen=life.expectancy.in.open.group)
      one_ddm_males <- ddm(X=data_for_ddm_males,
                           deaths.summed=deaths.summed,
                           exact.ages=one_exact_ages,
                           eOpen=life.expectancy.in.open.group)
      one_ddm_estimates <- FormatOutputDDM(result_ddm_females=one_ddm_females,
                                           result_ddm_males=one_ddm_males)
      
      ## RMSEs for females and males
      one_RMSE_females <- CallggbgetRMS(my.ddm.data=data_for_ddm_females,
                                        age.range=one_exact_ages,
                                        min.age.in.search=min.age.in.search,
                                        max.age.in.searc=max.age.in.search,
                                        deaths.summed=deaths.summed)
      one_RMSE_males <- CallggbgetRMS(my.ddm.data=data_for_ddm_males,
                                      age.range=one_exact_ages,
                                      min.age.in.search=min.age.in.search,
                                      max.age.in.search=max.age.in.search,
                                      deaths.summed=deaths.summed)
      one_RMSE_females$sex <- "Females" ## to match coding from FormatOutputDDM
      one_RMSE_males$sex <- "Males" ## to match coding from FormatOutputDDM
      one_RMSE <- rbind(one_RMSE_females,
                        one_RMSE_males)
      
      ## merge together into final table
      ### merge in populations
      one_combined_estimates <- left_join(x=one_ddm_estimates,
                                        y=data_with_total_pop,
                                        by="cod")
      ### merge in RMSEs
      one_combined_estimates <- left_join(x=one_combined_estimates,
                                          y=one_RMSE,
                                          by=c("cod", "sex"))
      
      # stack up results for all acceptable age ranges
      sensitivity_ddm_estimates <- rbind(sensitivity_ddm_estimates,
                                          one_combined_estimates)
    }
    sensitivity_ddm_estimates <- as.data.frame(sensitivity_ddm_estimates[-1, ])

    # merge in indicator of "optimal" age range selected by GGB
    ddm_estimates$selected_age_range_ggb <- TRUE
    sensitivity_ddm_estimates <- left_join(x=sensitivity_ddm_estimates,
                                           y=ddm_estimates[, c("cod", "sex", 
                                                               "lower_age_range", 
                                                               "upper_age_range", 
                                                               "selected_age_range_ggb")],
                                           by=c("cod", "sex", 
                                                "lower_age_range", "upper_age_range"))
    sensitivity_ddm_estimates[is.na(sensitivity_ddm_estimates$selected_age_range_ggb), 
                              "selected_age_range_ggb"] <- FALSE
    ddm_estimates$selected_age_range_ggb <- NULL
    
    # sort/order columns    
    sensitivity_ddm_estimates <- arrange(sensitivity_ddm_estimates,
                                         sex, cod)
    sensitivity_ddm_estimates <- sensitivity_ddm_estimates %>%
                                 select(cod, sex, 
                                        ggbseg, ggb, seg,
                                        total_pop1, total_pop2,
                                        everything())
    
    ## cleaning up date variables
    date.1 <- unique(data[, "date1"])
    date.2 <- unique(data[, "date2"])

    return(list("show.age.range.sensitivity"=show.age.range.sensitivity,
                "name_disaggregations"=name.disaggregations,
                "name.national"=name.national,
                "date1"=date.1,
                "date2"=date.2,
                "sensitivity_ddm_estimates"=sensitivity_ddm_estimates,
                "ddm_estimates"=ddm_estimates))
  } else {
    return(list("show.age.range.sensitivity"=show.age.range.sensitivity,
                "name_disaggregations"=name.disaggregations,
                "name.national"=name.national,
                "date1"=date.1,
                "date2"=date.2,
                "ddm_estimates"=ddm_estimates))
  }
}
