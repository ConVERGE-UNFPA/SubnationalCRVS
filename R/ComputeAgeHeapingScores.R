#' Compute summaries of age heaping (wrapper for DemoTools::check_heaping_*)
#'
#' This function serves as a wrapper for five DemoTools functions -- 
#' check_heaping_roughness(), check_heaping_sawtooth(), check_heaping_whipple(), check_heaping_myers(), check_heaping_noumbissi() -- 
#' and returns the five summary statistics within specified levels of disaggregation in a given dataset 
#' (which must contain single-year age counts) separately for males and females.
#' @param data data frame that contains at least seven columns representing: (1) single-year age,
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
#' @param name.population.year1 Character string providing the name of the variable in `data` that represents the population count in the earlier time period
#' @param name.population.year2 Character string providing the name of the variable in `data` that represents the population count in the later time period
#' @param name.year1 Character string providing the name of the variable in `data` that represents the year of the earlier of the two time periods (e.g. year of the earlier Census)
#' @param name.month1 Character string providing the name of the variable in `data` that represents the month of the earlier of the two time periods (e.g. month of the earlier Census)
#' @param name.day1 Character string providing the name of the variable in `data` that represents the day of the earlier of the two time periods (e.g. day of the earlier Census)
#' @param name.year2 Character string providing the name of the variable in `data` that represents the year of the later of the two time periods (e.g. year of the later Census)
#' @param name.month2 Character string providing the name of the variable in `data` that represents the month of the later of the two time periods (e.g. month of the later Census)
#' @param name.day2 Character string providing the name of the variable in `data` that represents the day of the later of the two time periods (e.g. day of the later Census)
#' @param confirm_single_year_ages Logical indicating whether (in contrast to result of variable checks) the `name.age` does in fact represent single-year ages and the error thrown by the variable checks should be overwritten. Default is FALSE
#' @param roughness.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param roughness.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of the highest age that is a multiple of 10
#' @param Whipple.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 25
#' @param Whipple.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 65
#' @param Whipple.digit=NULL Equivalent to the `digit` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of c(0, 5)
#' @param Myers.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 10
#' @param Myers.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 89
#' @param Noumbissi.age.min=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param Noumbissi.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 64
#' @param 
#' @examples
#' ecuador_age_heaping_scores <- ComputeAgeHeapingScores(data=ecuador_single_year_ages,
#'                                               name.disaggregations="province_name",
#'                                               name.males="m",
#'                                               name.females="f",
#'                                               name.age="age",
#'                                               name.sex="sex",
#'                                               name.population.year1="pop1",
#'                                               name.population.year2="pop2",
#'                                               name.year1="year1"
#'                                               name.month1="month1",
#'                                               name.day1="day1",
#'                                               name.year2="year2",
#'                                               name.month2="month2",
#'                                               name.day2="day2")
#' head(ecuador_age_heaping_scores)
#' tail(ecuador_age_heaping_scores)
#' @import dplyr
#' @import DemoTools
#' @export

ComputeAgeHeapingScores <- function(data, 
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
                          confirm_single_year_ages=FALSE,
                          roughness.age.min=NULL,
                          roughness.age.max=NULL,
                          Whipple.age.min=NULL,
                          Whipple.age.max=NULL,
                          Whipple.digit=NULL,
                          Myers.age.min=NULL,
                          Myers.age.max=NULL,
                          Noumbissi.age.min=NULL,
                          Noumbissi.age.max=NULL) {
  if (!is.data.frame(data)) {
    stop("the dataset provided in the 'data' argument needs to be a data frame")
  }
  data[, name.disaggregations] <- as.factor(data[, name.disaggregations]) # should we requrie that the disaggregations are a factor variable with informative labels?
  data <- CreateDateVariable(data=data,
                           name.disaggregations=name.disaggregations,
                           name.year1=name.year1,
                           name.month1=name.month1,
                           name.day1=name.day1,
                           name.year2=name.year2,
                           name.month2=name.month2,
                           name.day2=name.day2)
  # verify that the age variable is single-year ages and not groups of multiple ages (e.g. 5-year age groups)
  # and also emphasize that only the "deaths" column is actually not required
  CheckSingleYearAges(data,
                      name.disaggregations=name.disaggregations,
                      name.sex=name.sex,
                      confirm_single_year_ages=confirm_single_year_ages) ## creates date1, date2 as date classes
  
  # convert data into long format (more convenient for ggplot2)
  long_year1 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year1,
           date1) %>%
    rename(#pop=pop1,
           pop=name.population.year1,
           date=date1)
  
  long_year2 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year2,
           date2) %>%
    rename(#pop=pop2,
           pop=name.population.year2,
           date=date2)
  data_long <- rbind(long_year1, long_year2)
  
  # compute age heaping statistics based on data in long format
  data_with_age_heaping_long <- data_long %>%
      group_by(date, get(name.sex), get(name.disaggregations)) %>% 
      summarise("total_pop"=sum(pop, na.rm=TRUE),
                "roughness"= 
                  myRoughness(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=roughness.age.min,
                              ageMax=roughness.age.max),
                "Whipple"=
                  myWhipple(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                            Age=age,
                            ageMin=Whipple.age.min,
                            ageMax=Whipple.age.max,
                            digit=Whipple.digit),
                "Myers"=
                  myMyers(Value=pop,
                          Age=age,
                          ageMin=Myers.age.min,
                          ageMax=Myers.age.max),
                "Noumbissi_0"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=0),
                "Noumbissi_1"= 
                   myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                               Age=age,
                               ageMin=Noumbissi.age.min,
                               ageMax=Noumbissi.age.max,
                               digit=1),
                "Noumbissi_2"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=2),
                "Noumbissi_3"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=3),
                "Noumbissi_4"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=4),
                "Noumbissi_5"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=5),
                "Noumbissi_6"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=6),
                "Noumbissi_7"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=7),
                "Noumbissi_8"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=8),
                "Noumbissi_9"= 
                  myNoumbissi(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                              Age=age,
                              ageMin=Noumbissi.age.min,
                              ageMax=Noumbissi.age.max,
                              digit=9)) %>%
      as.data.frame()
  names(data_with_age_heaping_long)[names(data_with_age_heaping_long) == "get(name.sex)"] = name.sex
  names(data_with_age_heaping_long)[names(data_with_age_heaping_long) == "get(name.disaggregations)"] = name.disaggregations

  return(data_with_age_heaping_long)
}
