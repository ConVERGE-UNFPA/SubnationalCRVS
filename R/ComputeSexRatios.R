#' Compute sex ratios for adjacent five-year age groups
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
#' @param name.population.year1 Character string providing the name of the variable in `data` that represents the population count in the earlier time period
#' @param name.population.year2 Character string providing the name of the variable in `data` that represents the population count in the later time period
#' @param name.year1 Character string providing the name of the variable in `data` that represents the year of the earlier of the two time periods (e.g. year of the earlier Census)
#' @param name.month1 Character string providing the name of the variable in `data` that represents the month of the earlier of the two time periods (e.g. month of the earlier Census)
#' @param name.day1 Character string providing the name of the variable in `data` that represents the day of the earlier of the two time periods (e.g. day of the earlier Census)
#' @param name.year2 Character string providing the name of the variable in `data` that represents the year of the later of the two time periods (e.g. year of the later Census)
#' @param name.month2 Character string providing the name of the variable in `data` that represents the month of the later of the two time periods (e.g. month of the later Census)
#' @param name.day2 Character string providing the name of the variable in `data` that represents the day of the later of the two time periods (e.g. day of the later Census)
#' 
#' @examples
#' ecuador_sex_ratios <- ComputeSexRatios(data=ecuador_ecuador_single_year_ages,
#'                                        name.disaggregations="province_name",
#'                                        name.males="m",
#'                                        name.females="f",
#'                                        name.age="age",
#'                                        name.sex="sex",
#'                                        name.population.year1="pop1",
#'                                        name.population.year2="pop2",
#'                                        name.year1="year1"
#'                                        name.month1="month1",
#'                                        name.day1="day1",
#'                                        name.year2="year2",
#'                                        name.month2="month2",
#'                                        name.day2="day2")
#' head(ecuador_sex_ratios)
#' tail(ecuador_sex_ratios)
#' @import dplyr
#' @export

ComputeSexRatios <- function(data, 
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
                             name.day2) {
  if (!is.data.frame(data)) {
    stop("the dataset provided in the 'data' argument needs to be a data frame")
  }
  data <- CreateDateVariable(data=data,
                             name.disaggregations=name.disaggregations,
                             name.year1=name.year1,
                             name.month1=name.month1,
                             name.day1=name.day1,
                             name.year2=name.year2,
                             name.month2=name.month2,
                             name.day2=name.day2)
  # compute sex ratio within age groups and levels of disaggregation
  data_with_sex_ratio <- data %>% 
    group_by(get(name.age), get(name.disaggregations)) %>%
    mutate("sex_ratio_1"= 100 * 
             get(name.population.year1)[get(name.sex) == name.males] / 
             get(name.population.year1)[get(name.sex) == name.females],
           "sex_ratio_2"= 100 * 
             get(name.population.year2)[get(name.sex) == name.males] / 
             get(name.population.year2)[get(name.sex) == name.females],
           "pop1_both_sexes"=sum(pop1, na.rm=TRUE),
           "pop2_both_sexes"=sum(pop2, na.rm=TRUE)) %>%
    as.data.frame()
  data_with_sex_ratio$sex_ratio_1 <- round(data_with_sex_ratio$sex_ratio_1, 1)
  data_with_sex_ratio$sex_ratio_2 <- round(data_with_sex_ratio$sex_ratio_2, 1)
  # each age group only needs to be represented once (not separately for males and females)
  # but make sure population is summed across the sexes
  data_with_sex_ratio <- data_with_sex_ratio %>% filter(sex == name.females)
  data_with_sex_ratio[, name.population.year1] <- NULL
  data_with_sex_ratio[, name.population.year2] <- NULL
  names(data_with_sex_ratio)[names(data_with_sex_ratio) == "pop1_both_sexes"] <- name.population.year1
  names(data_with_sex_ratio)[names(data_with_sex_ratio) == "pop2_both_sexes"] <- name.population.year2
  data_with_sex_ratio[, "get(name.age)"] <- NULL
  data_with_sex_ratio[, "get(name.disaggregations)"] <- NULL
  data_with_sex_ratio[, name.sex] <- NULL
  
  # data_with_sex_ratio <- data_with_sex_ratio %>% select(-sex_ratio_1,
  #                                                       -sex_ratio_2,
  #                                                       everything()) %>%
  #                        as.data.frame()
  data_with_sex_ratio[, c("year1", "month1", "day1")] <- NULL
  data_with_sex_ratio[, c("year2", "month2", "day2")] <- NULL
  head(data_with_sex_ratio)
  return(data_with_sex_ratio)
}
