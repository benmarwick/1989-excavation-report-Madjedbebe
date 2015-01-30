# functions for the Rmd files


################# chronology ###############################
############################################################
#' Read in the dates data (table 1 of the paper)
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' chrono_data <- get_chrono_data()
#' }

get_chrono_data <- function(){

  # get raw data from text table in....
  dates_1989 <- read.csv("data/Date_table_from_paper_on_1989_dig.csv")

  return(dates_1989)

}

############################################################
#' Tidy the chrono data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' chrono_data_tidied <- tidy_chrono_data(chrono_data)
#' }

tidy_chrono_data <- function(dates_1989){

  # clean up depth, where there's a range, get midpoint
  # get depths with ranges, ie nn-nn
  depthwithrange <- as.character(dates_1989$Depth.cm [grepl("-", dates_1989$Depth.cm )])
  # find midpoint
  midpoint <- unlist(lapply(depthwithrange, function(i) abs(eval(parse(text= i)))))/2 + # get half of range
    as.numeric(gsub("-.*", "", depthwithrange)) # depth value
  # replace depth with range with single depth value
  dates_1989$Depth.cm <- as.character(dates_1989$Depth.cm)
  dates_1989$Depth.cm[grep("-", dates_1989$Depth.cm)] <- midpoint
  dates_1989$Depth.m <- as.numeric(dates_1989$Depth.cm)/100 # in meters

  # get the median value from OxCal output
  oxcal_medians <- read.csv("data/Date_table_from_paper_on_1989_dig_C14_OxCal_output.csv")

  # get lab sample ID for matching
  matching <- c("R_Date", " ")
  oxcal_medians$Name <- gsub(paste(matching, collapse = "|"), "", oxcal_medians$Name)

  # get rid of spaces and convert to same type
  dates_1989$Lab.Code <- as.character(gsub(" ", "", dates_1989$Lab.Code))
  dates_1989$Method <- as.character(gsub(" ", "", dates_1989$Method))

  dates_1989_with_C14_medians <- merge(x = dates_1989,
                                       y = oxcal_medians,
                                       by.x = "Lab.Code",
                                       by.y = "Name",
                                       all = TRUE)
  # remove header row
  dates_1989_with_C14_medians <- dates_1989_with_C14_medians[-1,]

  # in kya
  dates_1989_with_C14_medians$age2plot <- (ifelse(dates_1989_with_C14_medians$Method == "TL"|dates_1989_with_C14_medians$Method == "OSL", dates_1989_with_C14_medians$Uncalibrated.Age, ifelse(dates_1989_with_C14_medians$Method == "C14"| dates_1989_with_C14_medians$Method == "ABOX", as.numeric(as.character(dates_1989_with_C14_medians$X.3)), NA)))/1000

  # simplify object name
  dates_1989 <- dates_1989_with_C14_medians

  return(dates_1989)

}


############################################################
#' Interpolate the chrono data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' chrono_data_interpolated <- interpolate_chrono_data(chrono_data_tidied)
#' }

interpolate_chrono_data <- function(dates_1989){

  # interpolate age from loess line with depth
  # this is for the lines that show oldest artefact, etc.
  cal.date.lo <- loess(age2plot ~ Depth.m, dates_1989[dates_1989$Method %in% c("C14", "TL", "OSL"),] , span = 0.4) # exclude ABOX dates
  cal.date.pr <- predict(cal.date.lo, data.frame(Depth.m = seq(0, 5, 0.01)))
  # plot(cal.date.pr) # just to check
  cal.date.pr <- data.frame(age = cal.date.pr, depth = names(cal.date.pr))
  # determine an age from a known depth
  oldest_depth = 287 # depth in cm, to the nearest 1 cm: oldest artefacts
  oldest <- cal.date.pr[cal.date.pr$depth == oldest_depth,]$age # returns age from loess
  base_of_dense_depth = 250 # depth in cm, to the nearest 1 cm: base of dense artefacts
  dense <- cal.date.pr[cal.date.pr$depth == base_of_dense_depth,]$age # returns age from loess

  return(list(oldest_depth = oldest_depth,
              oldest = oldest,
              base_of_dense_depth = base_of_dense_depth,
              dense = dense,
              cal.date.lo = cal.date.lo))
}


############################################################
#' Plot the chrono data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' chrono_data_plotted <- plot_chrono_data(chrono_data_tidied,
#'                        oldest_depth = chrono_data_interpolated$oldest_depth,
#'                        oldest = chrono_data_interpolated$oldest,
#'                        base_of_dense_depth = chrono_data_interpolated$base_of_dense_depth,
#'                        dense = chrono_data_interpolated$dense)
#' }

plot_chrono_data <- function(dates_1989, oldest_depth, oldest, base_of_dense_depth, dense){

  library(ggplot2)
  limits <- aes(ymax = age2plot  + Error/1000, ymin = age2plot  - Error/1000, colour = factor(Method))
  library(scales) # needed for axis number format
  ggplot(dates_1989, aes(Depth.m, age2plot, , label = Lab.Code)) +
    # add loess line that excludes the ABOX dates
    geom_smooth(data =  dates_1989[dates_1989$Method %in% c("C14", "TL", "OSL"),]  , method = "loess", se = FALSE, span = 0.5, colour = "grey", alpha = 0.2, size = 1) +
    # set point size, colour and shape
    geom_point(size = 2, aes(colour = factor(Method), shape = factor(Method))) +
    # add error bars
    geom_linerange(limits, width=0.5) +
    # add lab codes as point labels
    geom_text(angle = 0,  hjust = -0.2, vjust = 0.2, size = 2) +
    # Edit axis labels
    xlab("meters below surface") + ylab("x 1000 years cal. BP") +
    # scale title
    scale_colour_discrete(name  ="Dating\nmethod" ) +
    scale_shape_discrete(name  ="Dating\nmethod" ) +
    # format y axis for readability
    scale_y_continuous(labels = comma, breaks = seq(0,100,20)) +
    scale_x_reverse() + # top to bottom of the trench
    # lines for lowest artefacts at 2.8 m
    annotate("segment", x = oldest_depth/100,
             y = oldest, xend = -4.75,
             yend = oldest, colour = "grey30") +
    annotate("segment",x = oldest_depth/100,
             y = 0, xend = -oldest_depth/100,
             yend = oldest, colour = "grey30") +
    annotate("text", x = 3, y = 15,
             label = paste0("lowest artefacts (", round(oldest,0), " ka BP)"),
             size = 3) +
    # lines for dense concentration at 2.5 m
    annotate("segment", x = base_of_dense_depth/100,
             y = dense, xend = -4.75,
             yend = dense, colour = "grey30") +
    annotate("segment", x = base_of_dense_depth/100,
             y = 0, xend = -base_of_dense_depth/100,
             yend = dense, , colour = "grey30") +
    annotate("text", x = 2.5, y = 15,
             label = paste0("base of dense \noccupation layer (", round(dense,0), " ka BP)"), size = 3) +

    theme_bw() +
    coord_flip() + # rotate
    # format non-data elements
    theme(# panel.background = element_blank(),        # suppress default background
      # panel.grid.major = element_blank(),            # suppress default major gridlines
      # panel.grid.minor = element_blank(),            # suppress default minor gridlines
      # axis.ticks = element_blank(),                  # suppress tick marks
      axis.title.x=element_text(size=17),              # increase axis title size slightly
      axis.title.y=element_text(size=17, angle=90),    # increase axis title size slightly and rotate
      axis.text.x=element_text(size=15, colour = "black"),    # increase size of numbers on x-axis
      axis.text.y=element_text(size=15, colour = "black"))    # increase size of numbers on y-axis
  #  aspect.ratio=0.5 )                                 # make the plot square-ish


  # save plot as SVG for finessing...
  ggsave(file="figures/MKII_dates_from_1989_for_paper.svg", width = 200, height = 200, units = "mm")

  # the some minor edits in inkscape to make the data point labels more readable
  # especially spacing out the overlapping point labels

}


############################################################
#' Difference in slopes of OSL and C14 depth-age lines
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' chrono_data_slopes_plot <- plotting_slopes_chrono_data(chrono_data_tidied)
#' }

plotting_slopes_chrono_data <- function(dates_1989){

  # diff in slopes over
  # the standard error of the difference between the two slopes
  dates_1989$Two_methods <- ifelse(dates_1989$Method == "TL"|dates_1989$Method == "OSL", "OSL/TL", ifelse(dates_1989$Method == "C14"| dates_1989$Method == "ABOX", "C14", "NA"))

  # plot
  library(ggplot2)
  the_plot <- ggplot(dates_1989, aes(age2plot, Depth.m, colour = Two_methods)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal(base_size = 14) +
    scale_y_reverse() +
    geom_text(aes(label = Lab.Code), size = 3, hjust=1, vjust=1) +
    ylab("meters below surface") + xlab("x 1000 years cal. BP")

  return(list(dates_1989 = dates_1989, the_plot = the_plot))

}

############################################################
#' Test of difference in slopes of OSL and C14 depth-age lines
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' anova_p_value <- testing_slopes_chrono_data(chrono_data_slopes_plot$dates_1989)
#' }

testing_slopes_chrono_data <- function(dates_1989){

  # Looking into the question of difference in slope of
  # regression lines using data from 1989

  # separate OSL and C14 dates to test for slope for paper on 1989 data
  OSL_TL <- dates_1989[grepl("TL", dates_1989$Two_methods),]
  C14 <- dates_1989[!grepl("TL", dates_1989$Two_methods),]

  # investigate regressions of depth-age by method
  my_summary <- summary(m1 <- aov( Depth.m ~ age2plot * Two_methods, data = dates_1989))
  # significant difference interaction, These results suggest that the slope of the regression between date and depth width is not similar for the two dating methods.

  anova_p_value <- round(anova(m1)$`Pr(>F)`[3],3)

  return(anova_p_value)

}

############################################################
#' ANCOVA Test of difference in slopes of OSL and C14 depth-age lines
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' ancova_p_values <- ancova_slopes_chrono_data(chrono_data_slopes_plot$dates_1989)
#' }

ancova_slopes_chrono_data <- function(dates_1989){

  # now limit to upper 2m
  upper <- dates_1989[dates_1989$Depth.m < 2, ]

  # investigate regressions of depth-age by method
  # from
  # http://stats.stackexchange.com/a/18397/7744 and
  # http://r-eco-evo.blogspot.com/2011/08/comparing-two-regression-slopes-by.html
  # and Teetor, P. 2011 R Cookbook, O'Reilly.  p 309-311.

  summary(m2 <- aov( Depth.m ~ age2plot * Two_methods, data = upper))

  # NO significant difference interaction at <2m bs, These results suggest that the slope of the regression between cal.date and depth width is not similar for the two dating methods.

  # plot
  the_plot <- ggplot(upper, aes(age2plot, Depth.m, colour = Two_methods)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal(base_size = 14) +
    scale_y_reverse() +
    geom_text(aes(label = Lab.Code), size = 3, hjust=1, vjust=1) +
    ylab("meters below surface") + xlab("x 1000 years cal. BP")

  a1 <- sprintf("%.5f",anova(m2)$`Pr(>F)`[1])
  a2 <- sprintf("%.3f",anova(m2)$`Pr(>F)`[3])

  return(list(a1 = a1, a2 = a2, the_plot = the_plot, upper = upper, m2 = m2))

}

############################################################
#' More parsimonous ANOVA test of difference in slopes of OSL and C14 depth-age lines
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' anova_p_values <- anova_slopes_chrono_data(ancova_p_values$upper, ancova_p_values$m2)
#' }

anova_slopes_chrono_data <- function(upper, m2){

  # more parsimonious model is fit without the interaction to test for a significant differences in the slope.
  summary(m3 <- aov( Depth.m ~ age2plot + Two_methods, data = upper))

  m2m3 <- anova(m2,m3)
  # removing the interaction DOES NOT significantly affect the fit of the model.  We observe that for the dates, the method has a NO significant  effect on age and the effect is NOT different for C14 and OSL
  a3 <- sprintf("%.3f",anova(m2, m3)$`Pr(>F)`[2])

  return(a3)

}

############################################################
#' Posteriors of slopes of OSL and C14 depth-age lines
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' bayes_values <- bayes_slopes_chrono_data(ancova_p_values$upper)
#' }

bayes_slopes_chrono_data <- function(upper){


  # Still looking at just the upper 2 m, now do Bayesian approach...

  OSL_TL_upper <- upper[grepl("OSL/TL", upper$Two_methods),]
  C14_upper <- upper[!grepl("TL", upper$Two_methods),]
  library(MCMCpack)
  posterior1 <- (MCMCregress(age2plot ~ Depth.m, data=OSL_TL_upper))
  # windows()
  # plot(posterior1)
  raftery.diag(posterior1)
  summary(posterior1)

  # intercept
  # hist(posterior1[,1])
  # x1 (ie. slope)
  # hist(posterior1[,2], main = parse(text='Posterior~of~Slope~of~OSL'), breaks = 100)
  # get distribution of slope
  x1_post <- as.numeric(posterior1[,2])
  # this is a bit absurd because just two data points...
  # trim 5% to 95%, cf. https://stat.ethz.ch/pipermail/r-help/2010-August/247632.html
  .limit <- quantile(x1_post, prob=c(.05, .95))
  trm <- subset(x1_post, x1_post >= .limit[1] & x1_post <= .limit[2])
  quantile(trm) # check to see if we've removed some of the extreme values
  x1_post <- trm
  # hist(x1_post)

  posterior2 <- (MCMCregress(age2plot ~ Depth.m, data=C14_upper))
  # windows()
  # plot(posterior2)
  raftery.diag(posterior2)
  summary(posterior2)

  # intercept
  # hist(posterior2[,1])
  # x1 (ie. slope)
  # hist(posterior2[,2], main = parse(text='Posterior~of~Slope~of~""^14*C'), breaks = 100)
  # get distribution of slope
  x2_post <- as.numeric(posterior2[,2])

  # plot the two together
  p1 <- hist(x2_post, breaks = 100, plot = FALSE)
  p2 <- hist(x1_post, breaks = 100, plot = FALSE)
  plot( p2, col=rgb(1,1,0,1/4),main = parse(text='Posterior~of~Slope~of~""^14*C~and~OSL~dates'))
  plot( p1, col=rgb(0,0,1,1/4), add=T)

  return(list(x1_post = x1_post, x2_post = x2_post))

}

############################################################
#' Bayes test of difference in osteriors of slopes of OSL and C14 depth-age lines
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' bayes_test_values <- bayes_test_slopes_chrono_data(bayes_values$x1_post, bayes_values$x2_post)
#' }

bayes_test_slopes_chrono_data <- function(x1_post, x2_post){
  # now compare the two distributions of slopes
  # rather time consuming...
  # BEST http://www.indiana.edu/~kruschke/BEST/
  # devtools::install_github('mikemeredith/BEST')
  library(BEST)
  slopes_test <- BESTmcmc(x1_post, x2_post, verbose = TRUE)
  # save to file so we can use this or a previously saved version
  # of the output
  save(slopes_test, file="data/slopes_test.rda")

  # key output is muDiff from summary
  # http://cran.r-project.org/web/packages/BEST/vignettes/BEST.pdf

}


############################################################
#' Bayesian change point analysis on sedimentation rates
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' bayes_cp_result <- bayes_cp_test(chrono_data_slopes_plot$dates_1989)
#' }

bayes_cp_test <- function(dates_1989){

  #### bayesian change point analysis on sedimentation rates

  library(bcp)

  # interpolate age from loess line with depth
  # this is for the lines that show oldest artefact, etc.
  cal.date.lo <- loess(age2plot ~ Depth.m, dates_1989[dates_1989$Method %in% c("C14", "TL", "OSL"),] , span = 0.4) # exclude ABOX dates
  cal.date.pr <- predict(cal.date.lo, data.frame(Depth.m = seq(0, 5, 0.01)))
  # plot(cal.date.pr)
  cal.date.pr <- data.frame(age = cal.date.pr, depth = names(cal.date.pr))

  # do cp analysis on dates
  # put in order of depth
  dates_1989 <- dates_1989[with(dates_1989, order(Depth.m)), ]
  cp <- bcp(as.numeric(dates_1989[dates_1989$Method %in% c("C14", "TL", "OSL"),]$age2plot))
  plot(cp)
  # legacyplot(cp)

  # what samples are at these change points?
  dates_no_ABOX <- dates_1989[dates_1989$Method %in% c("C14", "TL", "OSL"),]
  probs <- cbind(dates_no_ABOX, cp$posterior.prob)

  # where are the high probability change points?
  hi_probs <- probs[cp$posterior.prob > 0.9, ]

  return(list(hi_probs = hi_probs,
              cal.date.lo = cal.date.lo,
              cal.date.pr = cal.date.pr))

}

############################################################
#' Sedimentation rates during 15-20 ka only
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' sed_rates_15_to_20_result <- sed_rates_15_to_20(chrono_data_slopes_plot$dates_1989)
#' }

sed_rates_15_to_20 <- function(dates_1989){

  # check 15 – 20 ka sedimentation rates
  # examine slope from KTL97 through KTL165 compare C14 to OSL
  # now limit for depth range KTL97 through KTL165

  # get depths of those dates
  KTL97_depth <- dates_1989[dates_1989$Lab.Code == "KTL97", ]$Depth.m
  # two values for KTL162, both the same, let's just get one
  KTL165_depth <- dates_1989[dates_1989$Lab.Code == "KTL165", ]$Depth.m[1]

  # subset all dates in the region of KTL97 - KTL165
  sub <- dates_1989[dates_1989$Depth.m %in% seq(KTL165_depth,KTL97_depth ,0.001), ]
  # plot
  the_plot <- ggplot(sub, aes(age2plot, Depth.m, colour = Two_methods)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal(base_size = 14) +
    scale_y_reverse() +
    geom_text(aes(label = Lab.Code), size = 3, hjust=1, vjust=1) +
    ylab("meters below surface") + xlab("x 1000 years cal. BP")

  # separate OSL and C14 dates to test for slope
  sub$Method <- ifelse(grepl("TL", sub$Method), "OSL", "C14")
  sub_OSL <- sub[grepl("OSL", sub$Method),]
  sub_C14 <- sub[!grepl("OSL", sub$Method),]

  # investigate regressions of depth-age by method
  summary(m1 <- aov( Depth.m ~ age2plot * Method, data = sub))
  summary(m2 <- aov( Depth.m ~ age2plot + Method, data = sub))
  out <- anova(m1,m2)

  return(list(the_plot = the_plot, out = out,
              sub_OSL = sub_OSL, sub_C14 = sub_C14))

}

############################################################
#' Posterior difference in slopes during 15-20 ka only
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' bayes_slope_posteriors <- bayes_slope_difference(sed_rates_15_to_20_result$sub_OSL,
#'                                                  sed_rates_15_to_20_result$sub_C14)
#' }

bayes_slope_difference <- function(sub_OSL, sub_C14){

  library(MCMCpack)
  posterior1 <- (MCMCregress(age2plot ~ Depth.m, data=sub_OSL ))
  raftery.diag(posterior1)
  summary(posterior1)

  # get distribution of slope
  x1_post <- as.numeric(posterior1[,2])

  # trim extreme values cf. https://stat.ethz.ch/pipermail/r-help/2010-August/247632.html
  prob = c(0.47, 0.53)
  .limit <- quantile(x1_post, prob=prob)
  trm <- subset(x1_post, x1_post >= .limit[1] & x1_post <= .limit[2])
  quantile(trm) # check to see if we've removed some of the extreme values
  x1_post <- trm

  posterior2 <- (MCMCregress(age2plot ~ Depth.m, data=sub_C14))
  raftery.diag(posterior2)
  summary(posterior2)

  # get distribution of slope
  x2_post <- as.numeric(posterior2[,2])

  # trim  cf. https://stat.ethz.ch/pipermail/r-help/2010-August/247632.html
  .limit <- quantile(x2_post, prob=prob)
  trm <- subset(x2_post, x2_post >= .limit[1] & x2_post <= .limit[2])
  quantile(trm) # check to see if we've removed some of the extreme values
  x2_post <- trm

  # plot the two together
  p1 <- hist(x2_post, breaks = 100, plot = FALSE)
  p2 <- hist(x1_post, breaks = 100, plot = FALSE)
  plot( p2, col=rgb(1,0,0,1/4),main = parse(text='Posterior~of~Slope~of~""^14*C~and~OSL~dates~(late~Pleistocene)'))
  plot( p1, col=rgb(0,1,0,1/4), add=T)

  return(list(x1_post = x1_post, x2_post = x2_post))
}

############################################################
#'  Bayesian test of difference in slopes  during 15-20 ka only
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' bayes_slope_difference_result <- bayes_slope_test(bayes_slope_difference_result$x1_post,
#'                                                   bayes_slope_difference_result$x2_post)
#' }

bayes_slope_test <- function(x1_post, x2_post){

  # now compare the two distributions of slopes

  # BEST http://www.indiana.edu/~kruschke/BEST/
  # devtools::install_github('mikemeredith/BEST')
  library(BEST)
  slopes_test_sub <- BESTmcmc(x1_post, x2_post, verbose = TRUE)
  # save to file so we can use this or a previously saved version
  # of the output
  save(slopes_test_sub, file="data/slopes_test_sub.rda")

  # key output is muDiff from summary
  # http://cran.r-project.org/web/packages/BEST/vignettes/BEST.pdf

}


################# lithics ##################################
############################################################
#' Read in the lithics raw data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lithics_data <- get_lithics_data()
#' }

get_lithics_data <- function(){

  lithics <- read.csv("data/Lithics_table_from_paper_on_1989_dig.csv",fileEncoding="latin1")

  return(lithics)
}

############################################################
#' Clean the lithics raw data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lithics_data_cleaned <- clean_lithics_data(lithics_data)
#' }

clean_lithics_data <- function(lithics){

  # filter the data for display (remove some dates )
  lithics$C14 <- as.numeric(as.character(lithics$C14))
  lithics$C14[lithics$C14 %in% c(0.7, 15.0, 24)] <- NA

  # filter anomalous spit
  lithics <- lithics[!(lithics$Spit == 62),]

  # get rid of NA rows for total artefact count
  lithics <- lithics[!(is.na(lithics$Total.Artefacts)),]

  return(lithics)

}

############################################################
#' Plots of the lithics raw data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lithics_data_plotted <- plots_lithics_data(lithics_data, bayes_cp_result$cal.date.lo)
#' }

plots_lithics_data <- function(lithics, cal.date.lo){

  # plot
  library(ggplot2)

  # this plot appears in the paper
  p1 <- ggplot(lithics, aes(Spit, Total.Artefacts)) +
    geom_bar(stat="identity") +
    theme_minimal(base_size = 12) +
    ylab("Number of stone artefacts >6 mm") +
    annotate("text", x = lithics$Spit, y = 615, label = round(lithics$C14, 0), size = 4) +
    annotate("text", x = lithics$Spit, y = 615, label = round(lithics$OSL, 0), colour = "red", fontface="bold.italic", size = 4) +
    annotate("text", x = 60, y = 615, label = "ka",size = 4) +
    ylim(0,615)
  p1
  # save plot as SVG for finessing...
  ggsave(file = "figures/lithics_over_time_from_1989_for_paper.svg")


  # plot with ka along x-axis to help read peaks


  # prepare spit and depth data for showing on the plot
  # these are from CC's lithic sheet
  spit <- read.table(header  = TRUE, text = "spit start.depth end.depth
                     51  297 307
                     50  287 297
                     49  281 287
                     48  269 281
                     47	266 269
                     46	259 266
                     45	249 259
                     44	249 256
                     43	250 252 # rubble lens
                     42 242 249
                     41	242 250
                     40	232 242
                     39	232 242
                     38	222 232
                     37	212 222
                     36	205 212
                     35	192 205
                     34	186 192
                     33	181 186
                     32	175 181
                     31	166 172
                     30	161 166
                     28	152 161
                     27	147 152
                     26	142 147
                     25	136 142
                     24	128 136
                     23	125 128
                     22	116 125
                     21	107 116
                     20	100 107
                     19	95 101
                     18	91 95
                     17	83 91
                     16	78 83
                     15	71 78
                     14	65 71
                     13	60 65
                     12	54 60
                     11	50 54
                     10	44 50
                     9	42 44
                     8	36 42
                     7	30 36
                     6	25 30 # incomplete to surface
                     5	20 25
                     4	15 20 # 16,
                     3	10 15
                     2	5 10
                     1	0 5")

  # find midppoint of each spit by averaging start and end depth
  spit$mid <- round((apply(spit[,-1], 1, mean)),0)/100

  # interpolate age from midpoint depth of each spit
  # using output from the depth-age plot
  # interpolate age from loess line with depth

  spit.depth.pr <- predict(cal.date.lo, data.frame(Depth.m = seq(0,4,0.01)))
  # plot(spit.depth.pr)
  spit.depth.pr.df <- data.frame(age = spit.depth.pr, depth = as.numeric(names(spit.depth.pr))/100)
  spit.depth.pr.df$depth <- as.numeric(as.character(spit.depth.pr.df$depth))

  # merge interpolated depths  to lithic data
  interpolated_depths <- merge(x = lithics,
                               y = spit,
                               by.x = "Spit",
                               by.y = "spit",
                               all.x = TRUE)

  # merge interpolated ages to lithics dataframe
  interpolated_ages <- merge(x = interpolated_depths,
                             y = spit.depth.pr.df,
                             by.x = "mid",
                             by.y = "depth",
                             all.x = TRUE)

  # plot with ages, the interpolation isn't very good
  p2 <- ggplot(interpolated_ages, aes(age, Total.Artefacts)) +
    geom_bar(stat="identity") +
    theme_minimal(base_size = 12) +
    ylab("Number of stone artefacts >6 mm")  +
    ylim(0,600) +
    scale_x_continuous(breaks = seq(0, 70, 5))

  # plot with depth
  p3 <- ggplot(interpolated_ages, aes(mid, Total.Artefacts)) +
    geom_bar(stat="identity") +
    theme_minimal(base_size = 12) +
    ylab("Number of stone artefacts >6 mm")  +
    ylim(0,600) +
    scale_x_continuous(breaks = seq(0, 3, 0.5))

return(list(p1 = p1, p2 = p2, p3 = p3, spit = spit))
}

############################################################
#' Plots of the lithics raw materials
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lithics_rawmaterials_plotted <- plots_lithics_rawmaterials(lithics_data,
#'                                        lithics_data_plotted$spit,
#'                                        bayes_cp_result$cal.date.pr)
#' }

plots_lithics_rawmaterials <- function(lithics, spit, cal.date.pr){


  # select raw materials that show time sensitive pattern
  Local.Coarse.Grained.Quartzite_perc <- with(lithics, (Local.Coarse.Grained.Quartzite/Total.Artefacts))
  Silcrete_perc <- with(lithics, (Silcrete/Total.Artefacts))
  Quartz_perc <- with(lithics, (Quartz/Total.Artefacts))
  Chert_perc <- with(lithics, (Chert/Total.Artefacts))
  lithology <- cbind(Local.Coarse.Grained.Quartzite_perc, Silcrete_perc, Quartz_perc, Chert_perc)
  # replace NA with zero
  lithology[is.na(lithology)]  <- 0
  # get spit numbers
  spits <- with(lithics, cbind(Spit))
  # combine spits and lithologies
  plot <- data.frame(cbind(lithology, spits))
  # rename columns
  colnames(plot) <- c("Quartzite", "Silcrete", "Quartz", "Chert", "Spit")
  # order by spit, top to bottom
  plot <- plot[with(plot, order(plot$Spit)),]
  # melt for plotting
  library(reshape2)
  plot.m <- melt(plot, id.vars = "Spit")
  colnames(plot.m) <- c("Spit", "rawmaterial", "percentage")
  library(ggplot2)
  p1 <- ggplot(plot.m, aes(group = rawmaterial, x = Spit, y = percentage)) + geom_line(aes(colour = rawmaterial), size = 1) +
    #theme_bw() +
    xlab("Spit") +
    xlim(0,50) +
    ylab("Percent of assemblage in each spit") +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(axis.title.x = element_text(size = 15)) +
    theme(axis.title.y = element_text(size = 15, angle = 90)) +
    labs(colour = "Raw material") +
    theme_minimal()


  # merge depths and ages to lithic data
  plot1 <- merge(plot[c(1:50),], spit, by.x = "Spit", by.y = "spit")
  plot1$mid <- plot1$mid * 100
  plot2 <- merge(plot1, cal.date.pr, by.x = "mid", by.y = "depth")

  # melt for plotting with age on x-axis - put in paper on 1989 data
  library(reshape2)
  plot.m <- melt(plot2, id.vars = "age", measure.vars = c("Quartzite", "Silcrete", "Quartz", "Chert"))
  colnames(plot.m) <- c("age", "rawmaterial", "percentage")
  nlevels <- length(unique(plot.m$rawmaterial))

  library("RColorBrewer")
  library(ggplot2)
  p2 <- ggplot(plot.m, aes(colour = rawmaterial, x = age, y = percentage, linetype = rawmaterial)) +
    geom_line(size = 1) +
    xlab("Age (cal. ka BP)") +
    xlim(0,65) +
    ylab("Percent of assemblage in each spit") +
    #scale_x_continuous(labels = comma, breaks = seq(0,65,20)) +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(axis.title.x = element_text(size = 15)) +
    theme(axis.title.y = element_text(size = 15, angle = 90)) +
    scale_linetype_manual(values = 1:nlevels, name = "raw material") +
    scale_color_manual(values = c(brewer.pal(nlevels, "Set1")), name = "raw material") +
    theme_minimal(base_size = 16)

  p2
  # save plot as SVG for finessing...
  ggsave(file = "figures/lithics_proportions_from_1989_for_paper.svg")

  return(list(p1 = p1, p2 = p2, plot2 = plot2))

}

############################################################
#' Stratigraphic plot of the lithic raw materials
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lithics_rawmaterials_stratplotted <- stratiplot_lithics(lithics_rawmaterials_plotted$plot2)
#' }

stratiplot_lithics <- function(plot2){


require(analogue)
strat_plot <- plot2[,-c(1, 2, 7, 8)]
# get pit ages for zone labels
Zones <- plot2[plot2$Spit %in% c(41, 43), ]$age
# Stratiplot(age ~ . ,
#            data = strat_plot,
#            type = c("h","g"),
#            sort = "wa",
#            zones = Zones,
#            zoneNames = c("lower", "pit", "upper"))

require("rioja")

# stratigraphically constrained cluster analysis
diss <- dist(strat_plot[,-ncol(strat_plot)])
clust <- chclust(diss, method = "coniss")

# remove NA d[is.na(d)] <- 0
strat_plot[is.na(strat_plot)] <- 0

# save plot to file
png("figures/stratplot.png",width = 1000, height = 500)
x <- strat.plot(strat_plot[,-ncol(strat_plot)] * 100,
                scale.minmax = TRUE,
                yvar = as.numeric(strat_plot$age),
                ylim = range(strat_plot$age, na.rm=TRUE),
                y.rev = TRUE,
                ylabel = "Age (ka)",
                col.bar = "black",
                lwd.bar = 5,
                plot.line = FALSE, # if TRUE, fiddle with col and lwd
                col.line = "white", # try others that you like
                lwd.line = 1,    # ditto
                scale.percent = TRUE,
                clust = clust
)

# draws on red lines to indicate pit feature
addZone(x, Zones, col = 'red')
# text(10, 10, labels = "Pit feature")
dev.off()

# and draw it to the device also
x <- strat.plot(strat_plot[,-ncol(strat_plot)] * 100,
                scale.minmax = TRUE,
                yvar = as.numeric(strat_plot$age),
                ylim = range(strat_plot$age, na.rm=TRUE),
                y.rev = TRUE,
                ylabel = "Age (ka)",
                col.bar = "black",
                lwd.bar = 5,
                plot.line = FALSE, # if TRUE, fiddle with col and lwd
                col.line = "white", # try others that you like
                lwd.line = 1,    # ditto
                scale.percent = TRUE,
                clust = clust
)

# draws on red lines to indicate pit feature
addZone(x, Zones, col = 'red')
# text(10, 59.5, labels = "Pit feature")

}
############################################################
#' Differences in assemblage technology above and below the pit lens
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lens_differences_tech_plot <- lens_differences_tech()
#' }

lens_differences_tech <- function(){


dat_tech <- read.csv("data/MJB technological features-2.csv")


dat_tech$part <- with(dat_tech, ifelse(Spit %in% c(37:40), 'above',
                                       ifelse(Spit %in% c("41 / 43", "62 Pit"), 'lens',
                                              ifelse(Spit %in% c(42, 44:49), 'below',
                                                     "who_cares"))))

library("dplyr")
library("reshape2")
dat_tech1 <- dat_tech %>%
  melt() %>%
  filter(variable %in% c("Convergent.flakes", "Thinning.Flakes", "Scrapers"  )) %>%
  filter(part %in% c("above", "lens", "below")) %>%
  group_by(part, variable) %>%
  summarise (n = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = round(n / sum(n, na.rm = TRUE) * 100, 2))

library("ggplot2")
ggplot(dat_tech1, aes(variable, percentage)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ part) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
  theme_minimal()



# put things in order for the plot
dat_tech1$part <- factor(dat_tech1$part, levels = c('above', 'lens', 'below'))

n <- 15
ggplot(dat_tech1, aes(part, percentage, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("location relattive to lens") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = n)) +
  theme(axis.text.y = element_text(size = n)) +
  theme(axis.title.x = element_text(size = n)) +
  theme(axis.title.y = element_text(size = n, angle = 90))

ggsave("figures/raw-materials-lens.png", width = par("din")[1] * 1.6)
# Figures are not good match with what CC has in paper...

dat_tech2 <- dcast(dat_tech1, part ~ variable, value.var = 'n')[,-1]

tech_chi <- chisq.test(dat_tech2)
# not sig, this seems to agree fine

}


############################################################
#' Differences in assemblage raw materials above and below the pit lens
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lens_differences_raw_plot <- lens_differences_raw()
#' }

lens_differences_raw <- function(){


```{r}
dat_raw <- read.csv("G:/My Documents/My UW/Research/1206 M2 excavation/Malakunanga 89 excavations/JHE-paper-on-1989-excavation/MJB-1989-excavation-paper/data/Lithics_table_from_paper_on_1989_dig.csv")

dat_raw$part <- with(dat_raw, ifelse(Spit %in% c(37:40), 'above',
                                     ifelse(Spit %in% c(41, 43, 62), 'pit',
                                            ifelse(Spit %in% c(42, 44:49), 'below',
                                                   "who_cares"))))

dat_raw[10:15,] <- apply(dat_raw[10:15,], 2, as.numeric)

dat_raw$Quartzite <- dat_raw %>%
  select(Local.Coarse.Grained.Quartzite,
         Fine.Grained.Exotic.Quartzite,
         Brown.Quartzite) %>%
  rowSums(na.rm = TRUE)


library("dplyr")
library("reshape2")
dat_raw1 <- dat_raw %>%
  melt() %>%
  filter(variable %in% c("Quartz", "Quartzite", "Silcrete", "Chert" )) %>%
  filter(part %in% c("above", "pit", "below")) %>%
  group_by(part, variable) %>%
  summarise (n = sum(value, na.rm = TRUE)) %>%
  mutate(perc = round(n / sum(n, na.rm = TRUE) * 100, 2))

library("ggplot2")
ggplot(dat_raw1, aes(variable, perc)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ part)

ggplot(dat_raw1, aes(part, perc, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()

dat_raw2 <- dcast(dat_raw1, part ~ variable, value.var = 'n')[,-1]

chisq.test(dat_raw2)
# this seems to agree fine

# retouch to non-retouch

ret <- dat_raw %>%
  melt() %>%
  filter(variable %in% c("Retouched", "Total.Artefacts" )) %>%
  filter(part %in% c("above", "pit", "below")) %>%
  group_by(part, variable) %>%
  summarise (n = sum(value, na.rm = TRUE)) %>%
  mutate(perc = round(n / sum(n, na.rm = TRUE) * 100, 2))

ggplot(ret, aes(part, perc, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()

dat_ret <- dcast(ret, part ~ variable, value.var = 'n')[,-1]

chisq.test(dat_ret)
# yes sig diff, but small cell values,
fisher.test(dat_ret)
# still sig

