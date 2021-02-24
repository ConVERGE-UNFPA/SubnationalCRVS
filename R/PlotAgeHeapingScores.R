#' Plot age heaping scores
#'
#' asdf
#' 
#' @param data data frame that contains at least seven columns representing: (1) single-year age,
#' (2) sex,
#' (3, 4) population counts collected at two different time points (typically adjacent Census years)
#' (5, 6) dates of two different time points
#' (7) the level of subnational disaggregation in addition to sex (e.g. a geographic unit such as a province/state, 
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
#' @param name.national A character string providing the value of `name.disaggregations` variable that indicates national-level results (e.g. "Overall" or "National"). Defaults to NULL, implying `name.disaggregations` variable in `data` only includes values for subnational levels. Defaults to NULL
#' @param show.size.population A logical indixating whether the size of plotted points should vary according to the total population size in the second data year. Defaults to TRUE
#' @param roughness.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param roughness.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of the highest age that is a multiple of 10
#' @param Whipple.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 25
#' @param Whipple.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 65
#' @param Whipple.digit=NULL Equivalent to the `digit` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of c(0, 5)
#' @param Myers.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 10
#' @param Myers.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 89
#' @param Noumbissi.age.min=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param Noumbissi.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 64
#' @param show.population.counts=TRUE A logical indicated whether the population in date2 should be displayed in national plots and disaggregated plots (for Noumbissi indices). Defaults to TRUE
#' @param fig.nrow.Noumbissi=2 An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows on each page should be used to display the disaggregated plots of Noumbissi indices. Defaults to 2
#' @param fig.ncol.Noumbissi=1 An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns on each page should be used to display the disaggregated plots of Noumbissi indices. Defaults to 1
#' @param show.thresholds.Noumbissi=TRUE A logical indicating whether vertical lines at index values of 0.80 and 1.20 should be superimposed on the plots of indices plots. Defaults to TRUE
#' @param label.subnational.level A character label for the axis showing the level of subnational disaggregation present in the data. Defaults to `name.disaggregations` 
#' @param base.size A numeric fed to `ggplot2::theme_classic(base_size)` for the plot of point estimates. Defaults to 12
#' @param save.name.plots A character specifying a custom file name for the plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples
#' age_heaping_plotting <- PlotAgeHeapingScores(data=ecuador_single_year_ages
#'                                                   name.disaggregations="province_name",
#'                                                   name.males="m",
#'                                                   name.females="f",
#'                                                   name.age="age",
#'                                                   name.sex="sex",
#'                                                   name.population.year1="pop1",
#'                                                   name.population.year2="pop2",
#'                                                   name.year1="year1"
#'                                                   name.month1="month1",
#'                                                   name.day1="day1",
#'                                                   name.year2="year2",
#'                                                   name.month2="month2",
#'                                                   name.day2="day2"
#'                                                   plots.dir="Plots/")
#' head(age_heaping_plotting)
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @import reshape2
#' @export

PlotAgeHeapingScores <- function(data, 
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
                          show.population.counts=TRUE,
                          name.national=NULL,
                          show.size.population=TRUE,
                          roughness.age.min=NULL,
                          roughness.age.max=NULL,
                          Whipple.age.min=NULL,
                          Whipple.age.max=NULL,
                          Whipple.digit=NULL,
                          Myers.age.min=NULL,
                          Myers.age.max=NULL,
                          Noumbissi.age.min=NULL,
                          Noumbissi.age.max=NULL,
                          fig.nrow.Noumbissi=2,
                          fig.ncol.Noumbissi=1,
                          show.thresholds.Noumbissi=TRUE,
                          label.subnational.level=name.disaggregations,
                          base.size=12,
                          fig.nrow=1,
                          fig.ncol=1,
                          save.name.plots=NULL,
                          plots.dir="") {
  if (!is.data.frame(data)) {
    stop("the dataset provided in the 'data' argument needs to be a data frame")
  }
 data[, name.disaggregations] <- as.factor(data[, name.disaggregations])
 data <- CreateDateVariable(data=data,
                            name.disaggregations=name.disaggregations,
                            name.year1=name.year1,
                            name.month1=name.month1,
                            name.day1=name.day1,
                            name.year2=name.year2,
                            name.month2=name.month2,
                            name.day2=name.day2)
 date.1 <- as.character(unique(data$date1))
 date.2 <- as.character(unique(data$date2))
 
  # compute age heaping scores by calling ComputeAgeHeapingScores()
  ## variable checks performed within ComputeAgeHeapingScores() (should just call another function to do the checks that doesn't need to be documented)
  data_with_age_heaping_long <- ComputeAgeHeapingScores(data=data,
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
                                    roughness.age.min=roughness.age.min,
                                    roughness.age.max=roughness.age.max,
                                    Whipple.age.min=Whipple.age.min,
                                    Whipple.age.max=Whipple.age.max,
                                    Whipple.digit=Whipple.digit,
                                    Myers.age.min=Myers.age.min,
                                    Myers.age.max=Myers.age.max,
                                    Noumbissi.age.min=Noumbissi.age.min,
                                    Noumbissi.age.max=Noumbissi.age.max)
  data_with_age_heaping_long[, name.disaggregations] <- factor(data_with_age_heaping_long[, name.disaggregations],
                                                               levels=rev(levels(data_with_age_heaping_long[, name.disaggregations])))
    
  # make plots
  all_levels <- unique(levels(data[, name.disaggregations]))
  n_disaggregations <- length(all_levels)
  list_plots <- vector("list", length=n_disaggregations)
  national_check <- FALSE ## needed here?
  if (n_disaggregations == 1 & is.null(name.national) == TRUE) {
    stop(paste("Only one level of disaggregation,",
              all_levels,
              ", was detected in the variable",
              name.disaggregations,
              "but its value is not",
              name.national))
  }
  if (is.null(name.national) == FALSE) {
    if (name.national %in% unique(data[, name.disaggregations]) == FALSE) {
      stop(paste("The value",
                 name.national,
                 "was not found in the variable",
                 name.disaggregations))
    }
  }
  
  max_roughness <- max(data_with_age_heaping_long$roughness, na.rm=TRUE)
  min_roughness <- min(data_with_age_heaping_long$roughness, na.rm=TRUE)
  max_Whipple <- max(data_with_age_heaping_long$Whipple, na.rm=TRUE)
  min_Whipple <- min(data_with_age_heaping_long$Whipple, na.rm=TRUE)
  max_Myers <- max(data_with_age_heaping_long$Myers, na.rm=TRUE)
  min_Myers <- min(data_with_age_heaping_long$Myers, na.rm=TRUE)
  min_Noumbissi <- min(data_with_age_heaping_long[ , paste0("Noumbissi_", 0:9)],
                       na.rm=TRUE)
  max_Noumbissi <- max(data_with_age_heaping_long[ , paste0("Noumbissi_", 0:9)],
                       na.rm=TRUE)

  if (n_disaggregations > 1) { 
    if (is.null(name.national) == FALSE) {
      national_check <- TRUE ## needed here?
      data_with_age_heaping_long_for_overall <- data_with_age_heaping_long[data_with_age_heaping_long[, name.disaggregations] != name.national, ]
    } else {
      data_with_age_heaping_long_for_overall <-  data_with_age_heaping_long
    }
    data_with_age_heaping_long_for_overall$orig_date <- data_with_age_heaping_long_for_overall$date 
    data_with_age_heaping_long_for_overall$date <- as.factor(data_with_age_heaping_long_for_overall$date) # ggplot has issues with Date class
    ## roughness
    g_roughness <- ggplot(data=data_with_age_heaping_long_for_overall,
                          aes(x=roughness,
                              y=get(name.disaggregations)))
    if (show.size.population == TRUE) {
      g_roughness <-g_roughness + 
        geom_point(aes(col=sex,
                       shape=date,
                       size=total_pop)) +
        scale_size_continuous(labels = scales::comma,
                              range=c(1.5, 7),
                              name="Population")
    } else {
      g_roughness <- g_roughness + 
        geom_point(aes(col=sex,
                       shape=date),
                   size=3)
    }
    g_roughness <- g_roughness +
      labs(x="roughness",
           y=label.subnational.level,
           title=paste0("roughness by ", label.subnational.level)) +
      theme_classic(base_size=base.size) +
      scale_color_discrete(name="Sex") +
      scale_shape_discrete(name="Date") +
      coord_cartesian(xlim=c(0, max_roughness))
    
    if (max_roughness > 0.5) {
      g_roughness <- g_roughness +
                     geom_vline(aes(xintercept=0.5),
                                linetype="dashed",
                                col="gray",
                                size=1.2) +
                     labs(caption=("*Gray dashed line shows threshold of 0.5 
                                    above which smoothing is recommended if supported by plots of population counts"))
    }
    
    ## Whipple
    g_Whipple <- ggplot(data=data_with_age_heaping_long_for_overall,
                        aes(x=Whipple,
                            y=get(name.disaggregations)))
    if (show.size.population == TRUE) {
      g_Whipple <- g_Whipple + 
        geom_point(aes(col=sex,
                       shape=date,
                       size=total_pop)) +
        scale_size_continuous(labels = scales::comma,
                              range=c(1.5, 7),
                              name="Population")
    } else {
      g_Whipple <- g_Whipple + 
        geom_point(aes(col=sex,
                       shape=date),
                   size=3)
    }
    g_Whipple <- g_Whipple + 
      labs(x="Whipple's index",
           y=label.subnational.level,
           title=paste0("Whipple's index by ", label.subnational.level)) +
      theme_classic(base_size=base.size) +
      scale_color_discrete(name="Sex") +
      scale_shape_discrete(name="Date") +
      coord_cartesian(xlim=c(min(1, min_Whipple), max_Whipple))
    
    if (max_Whipple > 1.25) {
      g_Whipple <- g_Whipple +
        geom_vline(aes(xintercept=1.25),
                   linetype="dashed",
                   col="gray",
                   size=1.2) +
        labs(caption=("*Gray dashed line shows threshold of 1.25 
                                    above which smoothing is recommended if supported by plots of population counts"))
    }
      
    ## Myers
    g_Myers <- ggplot(data=data_with_age_heaping_long_for_overall,
                      aes(x=Myers,
                          y=get(name.disaggregations)))
    if (show.size.population == TRUE) {
      g_Myers <-g_Myers + 
        geom_point(aes(col=sex,
                       shape=date,
                       size=total_pop)) +
        scale_size_continuous(labels = scales::comma,
                              range=c(1.5, 7),
                              name="Population")
    } else {
      g_Myers <- g_Myers + 
        geom_point(aes(col=sex,
                       shape=date),
                   size=3)
    }
    g_Myers <- g_Myers +
      labs(x="Myers' blended index",
           y=label.subnational.level,
           title=paste0("Myers' blended index by ", label.subnational.level)) +
      theme_classic(base_size=base.size) +
      scale_color_discrete(name="Sex") +
      scale_shape_discrete(name="Date") + 
      coord_cartesian(xlim=c(min(min_Myers, 1), max_Myers))
    
    list_plots <- list(g_roughness,
                       g_Whipple,
                       g_Myers)
    
    overall <- marrangeGrob(list_plots,
                            nrow=1, 
                            ncol=1)
  }
  
  ## Noumbissi (with different values of digit argument)
  all_levels_Noumbissi <- unique(levels(data[, name.disaggregations]))
  n_disaggregations_Noumbissi <- length(all_levels_Noumbissi)
  data_for_Noumbissi <- reshape2::melt(data=data_with_age_heaping_long,
                             id.vars=c("date", name.sex, name.disaggregations, "total_pop"),
                             measure.vars=paste0("Noumbissi_", 0:9),
                             variable.name="Noumbissi_digit",
                             value.name="Noumbissi_value") %>%
                        as.data.frame()
  levels(data_for_Noumbissi$Noumbissi_digit) <- gsub(pattern="Noumbissi_", 
                                                     replacement="", 
                                                     x=levels(data_for_Noumbissi$Noumbissi_digit))
  data_for_Noumbissi$date <- as.factor(data_for_Noumbissi$date) # ggplot has issues with Date class
  list_plots_Noumbissi <- vector("list", length=n_disaggregations_Noumbissi)
  for (i in 1:n_disaggregations_Noumbissi) {
      one_level <- all_levels_Noumbissi[i]
      data_for_Noumbissi_one_level <- data_for_Noumbissi %>% 
                                      filter(get(name.disaggregations) == one_level)
      min_Noumbissi_one_level <- min(data_for_Noumbissi$Noumbissi_value, 
                                     na.rm=TRUE)
      max_Noumbissi_one_level <- max(data_for_Noumbissi$Noumbissi_value, 
                                     na.rm=TRUE)
      g_Noumbissi_one_level <- ggplot(data=data_for_Noumbissi_one_level,
                                      aes(x=Noumbissi_value,
                                          y=Noumbissi_digit))
      g_Noumbissi_one_level <- g_Noumbissi_one_level + 
                               geom_point(aes(col=sex,
                                              shape=date),
                                          size=2.5,
                                          alpha=0.7) +
                               labs(x="Value of Noumbissi's digit heaping index",
                                    y="Terminal digit") +
                               theme_classic() +
                               scale_color_discrete(name="Sex") +
                               scale_shape_discrete(name="Date")
      if (show.population.counts == TRUE) {
        pop2_one_level <- scales::comma(sum(data_for_Noumbissi_one_level[data_for_Noumbissi_one_level$date == date.2 &
                                     data_for_Noumbissi_one_level$Noumbissi_digit == 0, 
                                     "total_pop"],
                                    na.rm=TRUE))
        g_Noumbissi_one_level <- g_Noumbissi_one_level + 
                                 labs(title=paste0("Noumbissi indices in\n", 
                                                  one_level,
                                                  "\n",
                                                  "(Pop: ",
                                                  pop2_one_level,
                                                  ")"))
      } else {
        g_Noumbissi_one_level <- g_Noumbissi_one_level + 
          labs(title=paste("Noumbissi indices in\n", one_level))
      }
      if (show.thresholds.Noumbissi == TRUE) {
        g_Noumbissi_one_level <- g_Noumbissi_one_level +
                                 geom_vline(aes(xintercept=0.80),
                                            linetype="dashed",
                                            col="gray",
                                            size=1.2) +
                                 geom_vline(aes(xintercept=1.20),
                                            linetype="dashed",
                                            col="gray",
                                            size=1.2) +
                                 labs(caption=("*Gray dashed lines show range of 0.80 to 1.20, 
                                                outside of which smoothing is recommended if 
                                                supported by plots of population counts"))
      }
      if (is.null(name.national) == FALSE) {
        if (one_level == name.national) {
          Noumbissi_plot_national <- g_Noumbissi_one_level
          i_national_Noumbissi <- i  # needed so the level corresponding to national-level results is completely removed, not just storing the value NULL which gives error in marrangeGrob() function
        } else {
          list_plots_Noumbissi[[i]] <- g_Noumbissi_one_level
        } 
      } else {
        list_plots_Noumbissi[[i]] <- g_Noumbissi_one_level
      }
  }
  if (national_check == TRUE) {
    list_plots_Noumbissi[i_national_Noumbissi] <- NULL ## removing the national level plot expected in the list 
  }
  if (n_disaggregations_Noumbissi > 1) {
    arranged_Noumbissi_plots <- marrangeGrob(list_plots_Noumbissi, 
                                             nrow=fig.nrow.Noumbissi,
                                             ncol=fig.ncol.Noumbissi)
  }
  
  if (is.null(name.national) == FALSE) {
    national_check <- TRUE
    data_with_age_heaping_long_for_national <- data_with_age_heaping_long[data_with_age_heaping_long[, name.disaggregations] == name.national, ]
    data_with_age_heaping_long_for_national$date <- as.factor(data_with_age_heaping_long_for_national$date)
    head(data_with_age_heaping_long_for_national)
    max_index <- max(data_with_age_heaping_long_for_national[, c("roughness", "Whipple", "Myers")],
                     na.rm=TRUE)
    
    ## reshape
    head(data_with_age_heaping_long_for_national)
    reshape_national <- reshape2::melt(data_with_age_heaping_long_for_national,
               id.vars=c(name.disaggregations, name.sex, "date"),
               measure.vars=c("roughness", "Whipple", "Myers"),
               variable.name="index_name",
               value.name="index_value")
    
    g_national <- ggplot(data=reshape_national,
                aes(x=index_value,
                    y=index_name))
    g_national <- g_national + 
              geom_point(size=2,
                         aes(col=sex,
                             shape=date)) +
              labs(x="Value",
                   y="Index",
                   title=paste0("Age heaping indices -- ", name.national)) +
              theme_classic(base_size=base.size) +
              scale_color_discrete(name="Sex") + 
              coord_cartesian(xlim=c(0, max_index))
  }
  
  # print/save plots according to specified arguments
  graphics.off()
  if (is.null(save.name.plots) == FALSE) {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, save.name.plots, 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             overall)
    }
    if (n_disaggregations__Noumbissi > 1) {
      ggsave(paste0(plots.dir, save.name.plots, 
                    "_Noumbissi_",
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             arranged_Noumbissi_plots)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, save.name.plots, 
                    "_", name.national, "_", Sys.Date(), ".pdf"),
             g_national)
      ggsave(paste0(plots.dir, save.name.plots, 
                    "_Noumbissi_", name.national, "_", Sys.Date(), ".pdf"),
             Noumbissi_plot_national)
    }
  } else {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, "age_heaping_scores_combined_", 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             overall)
    }
    if (n_disaggregations_Noumbissi > 1) {
      ggsave(paste0(plots.dir, "Noumbissi_by_", 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             arranged_Noumbissi_plots)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, "age_heaping_scores_", 
                    name.national, "_", Sys.Date(), ".pdf"),
             g_national) 
      ggsave(paste0(plots.dir, "Noumbissi_", 
                    name.national, "_", Sys.Date(), ".pdf"),
             Noumbissi_plot_national) 
    }
  }
  graphics.off()
  # arrange data to return
  data_to_return <- data_with_age_heaping_long %>%
                    select(name.disaggregations,
                           date,
                           total_pop,
                           name.sex,
                           roughness,
                           Whipple,
                           Myers,
                           everything())
  return(data_to_return)
}
