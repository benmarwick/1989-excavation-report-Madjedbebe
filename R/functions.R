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
  library(grid)
  limits <- aes(ymax = age2plot  + Error/1000, ymin = age2plot  - Error/1000, colour = factor(Method))
  library(scales) # needed for axis number format
p1 <-  ggplot(dates_1989, aes(Depth.m, age2plot,  label = Lab.Code)) +
    # add loess line that excludes the ABOX date ANU-9915
    geom_smooth(data =  dates_1989[!(dates_1989$Lab.Code %in% c("ANUA-9915")),]  , method = "loess", se = FALSE, span = 0.5, colour = "grey", alpha = 0.2, size = 1) +
    # set point size, colour and shape
    geom_point(size = 2, aes(colour = factor(Method), shape = factor(Method))) +
    # add error bars
    geom_linerange(limits) + # remove  width=0.5
    # add lab codes as point labels
    geom_text(angle = 0,  hjust = -0.2, vjust = 0.2, size = 2) +
    # Edit axis labels
    xlab("meters below surface") + ylab("x 1000 years cal. BP") +
    # scale title
    scale_colour_discrete(name  = "Dating\nmethod" ) +
    scale_shape_discrete(name  = "Dating\nmethod" ) +
    # format y axis for readability
    scale_y_continuous(labels = comma, breaks = seq(0,100,20)) +
    scale_x_reverse() + # top to bottom of the trench
    # lines for lowest artefacts at 2.8 m
    annotate("segment", x = oldest_depth/100,
             y = oldest, xend = 4.75,
             yend = oldest, colour = "grey30") +
    annotate("segment",x = oldest_depth/100,
             y = 0, xend = oldest_depth/100,
             yend = oldest, colour = "grey30") +
    annotate("text", x = 3, y = 15,
             label = paste0("lowest artefacts (", round(oldest,0), " ka BP)"),
             size = 3) +
    # lines for dense concentration at 2.5 m
    annotate("segment", x = base_of_dense_depth/100,
             y = dense, xend = 4.75,
             yend = dense, colour = "grey30") +
    annotate("segment", x = base_of_dense_depth/100,
             y = 0, xend = base_of_dense_depth/100,
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
  ggsave(file="figures/Fig_5_MKII_dates_from_1989_for_paper.svg", width = 200, height = 200, units = "mm")

  # then some minor edits in inkscape to make the data point labels more readable
  # especially spacing out the overlapping point labels


return(p1)
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

  # filter the data for display (remove some dates that show inversions)
  lithics$C14 <- as.numeric(as.character(lithics$C14))
  lithics$C14[lithics$C14 %in% c(0.7, 15.0, 24)] <- NA


  # filter anomalous spit, which has an usual depth value
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
#' lithics_data_plotted <- plots_lithics_data(lithics_data_cleaned, bayes_cp_result$cal.date.lo)
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
    annotate("text", x = 50, y = 615, label = "ka",size = 4) +
    ylim(0,615) +
    xlim(0,50)
  p1
  # save plot as SVG for finessing...
  ggsave(file = "figures/Fig_6_lithics_over_time_from_1989_for_paper.svg")


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

# save plot to file, this is figure 9
png("figures/Fig_9_stratplot.png",width=10, height=5, units="in", res=1200)

par(oma=c(2,2,2,2))

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
mtext("Percentage of artefacts",
      side = 1, line = 5, cex = 1.0)
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
mtext("Percentage of artefacts",
      side = 1, line = 5, cex = 0.5)
# draws on red lines to indicate pit feature
addZone(x, Zones, col = 'red')
# text(10, 59.5, labels = "Pit feature")

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


  library("dplyr")
  library("reshape2")
  dat_raw <- read.csv("data/Lithics_table_from_paper_on_1989_dig.csv")

  dat_raw$part <- with(dat_raw,
                       ifelse(Spit %in% c(37:40), 'above',
                            ifelse(Spit %in% c(41, 43, 62), 'lens',
                                  ifelse(Spit %in% c(42, 44:49), 'below',
                                                     "who_cares"))))

  dat_raw[10:15,] <- apply(dat_raw[10:15,], 2, as.numeric)

  # combine Quartzite types
  dat_raw$Quartzite <- dat_raw %>%
      dplyr::select(Local.Coarse.Grained.Quartzite,
            Fine.Grained.Exotic.Quartzite,
            Brown.Quartzite) %>%
           rowSums(na.rm = TRUE)


  # put things in order for the plot
  dat_raw$part <- factor(dat_raw$part, levels = c('above', 'lens', 'below'))

  dat_raw1 <- dat_raw %>%
    melt() %>%
    filter(variable %in% c("Quartz", "Quartzite", "Silcrete", "Chert" )) %>%
    filter(part %in% c("above", "lens", "below")) %>%
    group_by(part, variable) %>%
    summarise (n = sum(value, na.rm = TRUE)) %>%
    mutate(perc = round(n / sum(n, na.rm = TRUE) * 100, 2))

  library("ggplot2")
p1 <- ggplot(dat_raw1, aes(variable, perc)) +
    geom_bar(stat = "identity") +
    facet_grid( ~ part) +
    theme_bw(base_size=18) +
    theme(axis.text.x = element_text(angle=90)) +
    xlab("") +
    ylab("Percentage")

ggsave("figures/raw-materials-lens.png", width = par("din")[1] * 1.6)

raw_dat_long <- dat_raw1

p2 <- ggplot(dat_raw1, aes(part, perc, fill = variable)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_minimal()

dat_raw2 <- dcast(dat_raw1, part ~ variable, value.var = 'n')[,-1]

  # chisq.test(dat_raw2)
  # this seems to agree fine
raw_chi_fisher <- fisher.test(dat_raw2, simulate.p.value=TRUE)

  # retouch to non-retouch

  ret <- dat_raw %>%
    melt() %>%
    filter(variable %in% c("Retouched", "Total.Artefacts" )) %>%
    filter(part %in% c("above", "lens", "below")) %>%
    group_by(part, variable) %>%
    summarise (n = sum(value, na.rm = TRUE)) %>%
    mutate(perc = round(n / sum(n, na.rm = TRUE) * 100, 2))

p3 <-   ggplot(ret, aes(part, perc, fill = variable)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_minimal()

raw_table <- dcast(ret, part ~ variable, value.var = 'n')[,-1]

# raw_chsq <-   chisq.test(raw_table)

return(list(p1 = p1,
            p2 = p2,
            p3 = p3,
            raw_dat_long =raw_dat_long,
            raw_chi_fisher = raw_chi_fisher,
            raw_table = raw_table))

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
# put things in order for the plot
dat_tech$part <- factor(dat_tech$part, levels = c('above', 'lens', 'below'))

# replace all NA with zero
dat_tech[is.na(dat_tech)] <- 0

dat_tech$Retouch <- with(dat_tech,  Bifacial.Points + Scrapers)

library("dplyr")
library("reshape2")

# total count of types in each group
dat_tech0 <- dat_tech %>%
  melt() %>%
  filter(variable %in% c("Convergent.flakes", "Thinning.Flakes", "Retouch"  )) %>%
  filter(part %in% c("above", "lens", "below")) %>%
  group_by(part) %>%
  summarise(n = sum(value, na.rm = TRUE))

sum_all_types <- sum(dat_tech0$n)

dat_tech1 <- dat_tech %>%
  melt() %>%
  filter(variable %in% c("Convergent.flakes", "Thinning.Flakes", "Retouch"  )) %>%
  filter(part %in% c("above", "lens", "below")) %>%
  group_by(part, variable) %>%
  summarise(n = sum(value, na.rm = TRUE))  %>%
  mutate(percentage = n / sum(n, na.rm = TRUE) * 100)

dat_tech_all <- dat_tech %>%
  melt() %>%
  filter(variable %in% c("Convergent.flakes", "Thinning.Flakes", "Retouch"  )) %>%
  filter(part %in% c("above", "lens", "below")) %>%
  group_by(part, variable) %>%
  summarise(n = sum(value, na.rm = TRUE))  %>%
  mutate(percentage = n / sum_all_types * 100)

# divide count by total number of types in each group

dat_tech2 <- inner_join(dat_tech0, dat_tech1, by = 'part')

dcast(dat_tech1, part ~ variable)

# The plot of the types in each zone by percentage of the count of types for each zone
library("ggplot2")
p1 <- ggplot(dat_tech1, aes(variable, percentage)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ part) +
  theme_bw(base_size = 16) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
  xlab("") +
  ylab("Percentage")

ggsave("figures/tech-lens-difference.png", width = par("din")[1] * 1.6)

tech_dat_long <- dat_tech1


# The plot of the types in each zone by percentage of the entire count of types for spits
ggplot(dat_tech_all, aes(variable, percentage)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ part) +
  theme_bw(base_size = 16) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
  xlab("") +
  ylab("Percentage")

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

tech_table <- dcast(dat_tech1, part ~ variable, value.var = 'n')[,-1]

# tech_chi <- chisq.test(tech_table)
tech_chi_fisher <- fisher.test(tech_table, simulate.p.value=TRUE)
# not sig, this seems to agree fine

return(list(p1 = p1,
            tech_chi_fisher = tech_chi_fisher,
            tech_table = tech_table,
            tech_dat_long = tech_dat_long))

}

############################################################
#' Combine raw materials and technology plots for upper-lens-lower
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lens_tech_and_raw_plot <- combine_lens_plots(lens_differences_tech_plot$p1,lens_differences_raw_plot$p1)
#' }

combine_lens_plots <- function(tech_dat_plot, raw_dat_plot) {

  png("figures/Fig_10_combine_lens_plots.png",width=16, height=10, units="in", res=600)

  gridExtra::grid.arrange(raw_dat_plot, tech_dat_plot)

  dev.off()

}


############################################################
#' Bayesian contingency table analysis - not using this here
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' lens_bayes_tech_plot <- bayes_conting(lens_differences_tech_plot$dat_tech1)
#' lens_bayes_raw_plot <- bayes_conting(lens_differences_raw_plot$raw_table)
#' }

bayes_conting <- function(lithics_table){


 # Zone = Eye
 # Tech = Hair

  # Load the data:
    fileNameRoot="PoissonExponentialJagsSTZ"
    fileNameRoot = paste( fileNameRoot , "tech" , sep="_" )
    dataFrame = data.frame( #
      Freq = lithics_table$n ,
      Zone  = as.character(lithics_table$part),
      Tech = as.character(lithics_table$variable) )
    y = as.numeric(dataFrame$Freq)
    x1 = as.numeric(dataFrame$Zone)
    x1names = levels(dataFrame$Zone)
    x2 = as.numeric(dataFrame$Tech)
    x2names = levels(dataFrame$Tech)
    Ncells = length(y)
    Nx1Lvl = length(unique(x1))
    Nx2Lvl = length(unique(x2))
    normalize = function( v ){ return( v / sum(v) ) }
    x1contrastList = list( ABOVEvLENS = (x1names=="above")-(x1names=="lens"),
      BELOWvLENS = (x1names=="below")-(x1names=="lens"),
      LENSvOTHERS = (x1names=="lens")-(x1names !="lens") )


# from http://www.indiana.edu/~kruschke/DoingBayesianDataAnalysis/Programs/PoissonExponentialJagsSTZ.R
fileNameRoot="figures/PoissonExponentialJagsSTZ" # for constructing output filenames
# source("openGraphSaveGraph.R")

##### start openGraph #####
openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    X11( width=width*mag , height=height*mag , type="cairo" , ... )
  } else { # Windows OS
    windows( width=width*mag , height=height*mag , ... )
  }
}

saveGraph = function( file="saveGraphOutput" , type="pdf" , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      savePlot( file=paste(file,".",type,sep="") , type=sptype , ... )
    }
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste(file,".",type,sep="") , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste(file,".",type,sep="") , ... )
    }
    if ( type == "png" ) {
      dev.copy2pdf(file=paste(file,".",type,sep="") , ... )
    }
  } else { # Windows OS
    file=paste(file,".",type,sep="") # force explicit extension
    savePlot( file=file , type=type , ... )
  }
}
##### end openGraph #####


require(rjags)         # Kruschke, J. K. (2011). Doing Bayesian Data Analysis:
# A Tutorial with R and BUGS. Academic Press / Elsevier.
#------------------------------------------------------------------------------
# THE MODEL.

modelstring = "
model {
for ( i in 1:Ncells ) {
y[i] ~ dpois( lambda[i] )
lambda[i] <- exp( a0 + a1[x1[i]] + a2[x2[i]] + a1a2[x1[i],x2[i]] )
}
#
a0 ~ dnorm(10,1.0E-6)
#
for ( j1 in 1:Nx1Lvl ) { a1[j1] ~ dnorm( 0.0 , a1tau ) }
a1tau <- 1 / pow( a1SD , 2 )
a1SD ~ dgamma(1.01005,0.01005) # mode=1,sd=100
#
for ( j2 in 1:Nx2Lvl ) { a2[j2] ~ dnorm( 0.0 , a2tau ) }
a2tau <- 1 / pow( a2SD , 2 )
a2SD ~ dgamma(1.01005,0.01005) # mode=1,sd=100
#
for ( j1 in 1:Nx1Lvl ) { for ( j2 in 1:Nx2Lvl ) {
a1a2[j1,j2] ~ dnorm( 0.0 , a1a2tau )
} }
a1a2tau <- 1 / pow( a1a2SD , 2 )
a1a2SD ~ dgamma(1.01005,0.01005) # mode=1,sd=100
# Convert a0,a1[],a2[],a1a2[,] to sum-to-zero b0,b1[],b2[],b1b2[,] :
for ( j1 in 1:Nx1Lvl ) { for ( j2 in 1:Nx2Lvl ) {
m[j1,j2] <- a0 + a1[j1] + a2[j2] + a1a2[j1,j2]
} }
b0 <- mean( m[1:Nx1Lvl,1:Nx2Lvl] )
for ( j1 in 1:Nx1Lvl ) { b1[j1] <- mean( m[j1,1:Nx2Lvl] ) - b0 }
for ( j2 in 1:Nx2Lvl ) { b2[j2] <- mean( m[1:Nx1Lvl,j2] ) - b0 }
for ( j1 in 1:Nx1Lvl ) { for ( j2 in 1:Nx2Lvl ) {
b1b2[j1,j2] <- m[j1,j2] - ( b0 + b1[j1] + b2[j2] )
} }
}
" # close quote for modelstring
# Write model to a file, and send to JAGS:
writeLines(modelstring,con="model.txt")

#------------------------------------------------------------------------------

dataList = list(
  y = y ,
  x1 = x1 ,
  x2 = x2 ,
  Ncells = Ncells ,
  Nx1Lvl = Nx1Lvl ,
  Nx2Lvl = Nx2Lvl
)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAINS.

theData = data.frame( y=log(y) , x1=factor(x1,labels=x1names) ,
                      x2=factor(x2,labels=x2names) )
a0 = mean( theData$y )
a1 = aggregate( theData$y , list( theData$x1 ) , mean )[,2] - a0
a2 = aggregate( theData$y , list( theData$x2 ) , mean )[,2] - a0
linpred = as.vector( outer( a1 , a2 , "+" ) + a0 )
a1a2 = aggregate( theData$y, list(theData$x1,theData$x2), mean)[,3] - linpred
initsList = list( a0 = a0 , a1 = a1 , a2 = a2 ,
                  a1a2 = matrix( a1a2 , nrow=Nx1Lvl , ncol=Nx2Lvl ) ,
                  a1SD = max(sd(a1),0.1) , a2SD = max(sd(a2),0.1) , a1a2SD = max(sd(a1a2),0.1) )

#------------------------------------------------------------------------------
# RUN THE CHAINS

parameters = c( "a0" ,  "a1" ,  "a2" ,  "a1a2" ,
                "b0" ,  "b1" ,  "b2" ,  "b1b2" ,
                "a1SD" , "a2SD" , "a1a2SD" )
adaptSteps = 1000
burnInSteps = 2000
nChains = 5
numSavedSteps=50000
thinSteps=1
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
# Create, initialize, and adapt the model:
jagsModel = jags.model( "model.txt" , data=dataList , inits=initsList ,
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                            n.iter=nIter , thin=thinSteps )
# resulting codaSamples object has these indices:
#   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]

#------------------------------------------------------------------------------
# EXAMINE THE RESULTS

checkConvergence = F
if ( checkConvergence ) {
  show( summary( codaSamples ) )
  openGraph()
  plot( codaSamples , ask=F )
  openGraph()
  autocorr.plot( codaSamples , ask=F )
}

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
mcmcChain = as.matrix( codaSamples )


# Extract the SDs:
a1SDSample = mcmcChain[,"a1SD"]
a2SDSample = mcmcChain[,"a2SD"]
a1a2SDSample = mcmcChain[,"a1a2SD"]

# Extract b values:
b0Sample = mcmcChain[, "b0" ]
chainLength = length(b0Sample)
b1Sample = array( 0 , dim=c( dataList$Nx1Lvl , chainLength ) )
for ( x1idx in 1:dataList$Nx1Lvl ) {
  b1Sample[x1idx,] = mcmcChain[, paste("b1[",x1idx,"]",sep="") ]
}
b2Sample = array( 0 , dim=c( dataList$Nx2Lvl , chainLength ) )
for ( x2idx in 1:dataList$Nx2Lvl ) {
  b2Sample[x2idx,] = mcmcChain[, paste("b2[",x2idx,"]",sep="") ]
}
b1b2Sample = array(0, dim=c( dataList$Nx1Lvl , dataList$Nx2Lvl , chainLength ) )
for ( x1idx in 1:dataList$Nx1Lvl ) {
  for ( x2idx in 1:dataList$Nx2Lvl ) {
    b1b2Sample[x1idx,x2idx,] = mcmcChain[, paste( "b1b2[",x1idx,",",x2idx,"]",
                                                  sep="" ) ]
  }
}

# source("plotPost.R")



openGraph(10,3)
layout( matrix(1:3,nrow=1) )
par( mar=c(3,1,2.5,0) , mgp=c(2,0.7,0) )
histInfo = plotPost( a1SDSample , xlab="a1SD" , main="a1 SD" , showMode=T )
histInfo = plotPost( a2SDSample , xlab="a2SD" , main="a2 SD" , showMode=T )
histInfo = plotPost( a1a2SDSample , xlab="a1a2SD" , main="Interaction SD" ,
                     showMode=T )
saveGraph(file=paste( fileNameRoot,"SD", sep=""), type="png")

# Plot b values:
openGraph((dataList$Nx1Lvl+1)*2.75,(dataList$Nx2Lvl+1)*2.25)
layoutMat = matrix( 0 , nrow=(dataList$Nx2Lvl+1) , ncol=(dataList$Nx1Lvl+1) )
layoutMat[1,1] = 1
layoutMat[1,2:(dataList$Nx1Lvl+1)] = 1:dataList$Nx1Lvl + 1
layoutMat[2:(dataList$Nx2Lvl+1),1] = 1:dataList$Nx2Lvl + (dataList$Nx1Lvl + 1)
layoutMat[2:(dataList$Nx2Lvl+1),2:(dataList$Nx1Lvl+1)] = matrix(
  1:(dataList$Nx1Lvl*dataList$Nx2Lvl) + (dataList$Nx2Lvl+dataList$Nx1Lvl+1) ,
  ncol=dataList$Nx1Lvl , byrow=T )
layout( layoutMat )
par( mar=c(4,0.5,2.5,0.5) , mgp=c(2,0.7,0) )
histinfo = plotPost( b0Sample , xlab=expression(beta * 0) , main="Baseline" )
for ( x1idx in 1:dataList$Nx1Lvl ) {
  histinfo = plotPost( b1Sample[x1idx,] , xlab=bquote(beta*1[.(x1idx)]) ,
                       main=paste("x1:",x1names[x1idx]) )
}
for ( x2idx in 1:dataList$Nx2Lvl ) {
  histinfo = plotPost( b2Sample[x2idx,] , xlab=bquote(beta*2[.(x2idx)]) ,
                       main=paste("x2:",x2names[x2idx]) )
}
for ( x2idx in 1:dataList$Nx2Lvl ) {
  for ( x1idx in 1:dataList$Nx1Lvl ) {
    hdiLim = HDIofMCMC(b1b2Sample[x1idx,x2idx,])
    if ( hdiLim[1]>0 | hdiLim[2]<0 ) { compVal=0 } else { compVal=NULL }
    histinfo = plotPost( b1b2Sample[x1idx,x2idx,] , compVal=compVal ,
                         xlab=bquote(beta*12[list(x1==.(x1idx),x2==.(x2idx))]) ,
                         main=paste("x1:",x1names[x1idx],", x2:",x2names[x2idx])  )
  }
}
saveGraph(file=paste(fileNameRoot,"b",sep=""),type="png")

# Display contrast analyses
nContrasts = length( x1contrastList )
if ( nContrasts > 0 ) {
  nPlotPerRow = 5
  nPlotRow = ceiling(nContrasts/nPlotPerRow)
  nPlotCol = ceiling(nContrasts/nPlotRow)
  openGraph(3.75*nPlotCol,2.5*nPlotRow)
  layout( matrix(1:(nPlotRow*nPlotCol),nrow=nPlotRow,ncol=nPlotCol,byrow=T) )
  par( mar=c(4,0.5,2.5,0.5) , mgp=c(2,0.7,0) )
  for ( cIdx in 1:nContrasts ) {
    contrast = matrix( x1contrastList[[cIdx]],nrow=1) # make it a row matrix
    incIdx = contrast!=0
    histInfo = plotPost( contrast %*% b1Sample , compVal=0 ,
                         xlab=paste( round(contrast[incIdx],2) , x1names[incIdx] ,
                                     c(rep("+",sum(incIdx)-1),"") , collapse=" " ) ,
                         cex.lab = 1.0 ,
                         main=paste( "X1 Contrast:", names(x1contrastList)[cIdx] ) )
  }
  saveGraph(file=paste(fileNameRoot,"x1Contrasts",sep=""),type="png")

}


# Compute credible cell probability at each step in the MCMC chain
lambda12Sample = 0 * b1b2Sample
for ( chainIdx in 1:chainLength ) {
  lambda12Sample[,,chainIdx] = exp(
    b0Sample[chainIdx]
    + outer( b1Sample[,chainIdx] , b2Sample[,chainIdx] , "+" )
    + b1b2Sample[,,chainIdx] )
}
cellp = 0 * lambda12Sample
for ( chainIdx in 1:chainLength ) {
  cellp[,,chainIdx] = ( lambda12Sample[,,chainIdx]
                        / sum( lambda12Sample[,,chainIdx] ) )
}
# Display credible cell probabilities
openGraph((dataList$Nx1Lvl)*2.75,(dataList$Nx2Lvl)*2.25)
layoutMat = matrix( 1:(dataList$Nx2Lvl*dataList$Nx1Lvl) ,
                    nrow=(dataList$Nx2Lvl) , ncol=(dataList$Nx1Lvl) , byrow=T )
layout( layoutMat )
par( mar=c(4,1.5,2.5,0.5) , mgp=c(2,0.7,0) )
maxp = max( cellp )
for ( x2idx in 1:dataList$Nx2Lvl ) {
  for ( x1idx in 1:dataList$Nx1Lvl ) {
    histinfo = plotPost( cellp[x1idx,x2idx,] ,
                         breaks=seq(0,maxp,length=51) , xlim=c(0,maxp) ,
                         xlab=bquote(probability[list(x1==.(x1idx),x2==.(x2idx))]) ,
                         main=paste("x1:",x1names[x1idx],", x2:",x2names[x2idx]) ,
                         HDItextPlace=0.95 )
  }
}
saveGraph(file=paste(fileNameRoot,"CellP",sep=""),type="png")


#==============================================================================
# Conduct NHST Pearson chi-square test of independence.

# Convert dataFrame to frequency table:
obsFreq = matrix( 0 , nrow=Nx1Lvl , ncol=Nx2Lvl )
for ( x1idx in 1:Nx1Lvl ) {
  for ( x2idx in 1:Nx2Lvl ) {
    obsFreq[x1idx,x2idx] = y[ dataFrame[,2]==x1names[x1idx]
                              & dataFrame[,3]==x2names[x2idx] ]
  }
}
obsFreq = t(obsFreq) # merely to match orientation of histogram display
chisqtest = chisq.test( obsFreq )
# print( "obs :" )
# print( chisqtest$observed )
# print( "( obs - exp )^2 / exp :" )
# print( ( chisqtest$observed - chisqtest$expected )^2 / chisqtest$expected )

#==============================================================================



return(chisqtest)
}

############################################################
#' HDIofMCMC
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' HDIofMCMC()
#' }

#### begin HDIofMCMC.R ####
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = floor( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}
#### end HDIofMCMC.R ####

############################################################
#' plotPost
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' plotPost()
#' }
#### start plotPost.R #####
plotPost <-  function( paramSampleVec , credMass=0.95 , compVal=NULL ,
                     HDItextPlace=0.7 , ROPE=NULL , yaxt=NULL , ylab=NULL ,
                     xlab=NULL , cex.lab=NULL , cex=NULL , xlim=NULL , main=NULL ,
                     col=NULL , border=NULL , showMode=F , showCurve=F , breaks=NULL ,
                     ... ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Parameter"
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"

  postSummary = matrix( NA , nrow=1 , ncol=11 ,
                        dimnames=list( c( xlab ) ,
                                       c("mean","median","mode",
                                         "hdiMass","hdiLow","hdiHigh",
                                         "compVal","pcGTcompVal",
                                         "ROPElow","ROPEhigh","pcInROPE")))
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]

  # source("HDIofMCMC.R")




  HDIofMCMC = function( sampleVec , credMass=0.95 ) {
    # Computes highest density interval from a sample of representative values,
    #   estimated as shortest credible interval.
    # Arguments:
    #   sampleVec
    #     is a vector of representative values from a probability distribution.
    #   credMass
    #     is a scalar between 0 and 1, indicating the mass within the credible
    #     interval that is to be estimated.
    # Value:
    #   HDIlim is a vector containing the limits of the HDI
    sortedPts = sort( sampleVec )
    ciIdxInc = floor( credMass * length( sortedPts ) )
    nCIs = length( sortedPts ) - ciIdxInc
    ciWidth = rep( 0 , nCIs )
    for ( i in 1:nCIs ) {
      ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
    }
    HDImin = sortedPts[ which.min( ciWidth ) ]
    HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
    HDIlim = c( HDImin , HDImax )
    return( HDIlim )
  }

  HDI = HDIofMCMC( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]

  # Plot histogram.
  if ( is.null(breaks) ) {
    breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                     by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display mean or mode:
  if ( showMode==F ) {
    meanParam = mean( paramSampleVec )
    text( meanParam , cenTendHt ,
          bquote(mean==.(signif(meanParam,3))) , adj=c(.5,0) , cex=cex )
  } else {
    dres = density( paramSampleVec )
    modeParam = dres$x[which.max(dres$y)]
    text( modeParam , cenTendHt ,
          bquote(mode==.(signif(modeParam,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    cvCol = "darkgreen"
    pcgtCompVal = round( 100 * sum( paramSampleVec > compVal )
                         / length( paramSampleVec )  , 1 )
    pcltCompVal = 100 - pcgtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) ,
           lty="dotted" , lwd=1 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(pcltCompVal)*"% < " *
                    .(signif(compVal,3)) * " < "*.(pcgtCompVal)*"%" ) ,
          adj=c(pcltCompVal/100,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pcGTcompVal"] = ( sum( paramSampleVec > compVal )
                                    / length( paramSampleVec ) )
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    ropeCol = "darkred"
    pcInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                 / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(ROPE) , ROPEtextHt ,
          bquote( .(round(100*pcInROPE))*"% in ROPE" ) ,
          adj=c(.5,0) , cex=1 , col=ropeCol )

    postSummary[,"ROPElow"]=ROPE[1]
    postSummary[,"ROPEhigh"]=ROPE[2]
    postSummary[,"pcInROPE"]=pcInROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}
#### end plotPost.R #####

############################################################
#' Plot shell data
#'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' shell_data_plotted <- plot_shell_data()
#' }

plot_shell_data <- function(){

  shells <- read.csv("data/Shell_table_from_paper_on_1989_dig.csv", header=TRUE, stringsAsFactors = FALSE)

  # edit colnames
  shells$`P. coaxans` <- shells$Geloina
  shells$`T. telescopium` <- shells$Telescopium
  shells$`Cerithidea sp.` <- shells$Cerithidea
  shells$`Nerita sp.` <- shells$Nerita

  # filter rows
  shells <- shells[shells$Spit %in% c("DE30/4", "DE30/5", "DE30/6", "DE30/7", "DE30/8"),]

  # add in missing row (read from excel chart embedded in draft)
  shells <- rbind(shells, c("DE30/7", 589.2,  113.5,
                            0, 0, 0,
                            62, 0, 0,
                            0, 292, 113.5,
                            589.2, 62))

  # subset just certain species
  shells <- shells[, c("Spit", "P. coaxans", "T. telescopium", "Cerithidea sp.", "Nerita sp.")]

  # sort by spit
  shells <- shells[with(shells, order(gsub("[[:alpha:]]", "", shells$Spit)  )), ]

  # reshape
  library(reshape2)
  shells_m <- melt(shells, id = 'Spit')
  shells_m$value  <- as.numeric(shells_m$value)

  # plot mass
  library(ggplot2)


  # compute percent of spit total
  shells[, c("P. coaxans", "T. telescopium", "Cerithidea sp.", "Nerita sp.")] <- sapply(shells[, c("P. coaxans", "T. telescopium", "Cerithidea sp.", "Nerita sp.")], as.numeric)

  spit_total <- unname(rowSums(shells[, c("P. coaxans", "T. telescopium", "Cerithidea sp.", "Nerita sp.")], na.rm = TRUE))

  shells_perc <- shells[, c("P. coaxans", "T. telescopium", "Cerithidea sp.", "Nerita sp.")]

  # compute percentages
  df <- shells_perc
  for(i in 1:nrow(shells_perc)){
    df[i,] <- shells_perc[i,] / spit_total[i] * 100
  }

  df$Spit <- shells$Spit
  library(reshape2)
  df_m <- melt(df, id = 'Spit')
  df_m$value  <- as.numeric(df_m$value)



# get perc mass and mass in one table, then plot
perc <- df_m
mass <-  shells_m

perc_and_mass <- merge(perc, mass, by = c("Spit", "variable"))
names(perc_and_mass) <- c("Spit", "Taxon", "Percentage", "Mass")

perc_and_mass_m <- melt(perc_and_mass)


g <- ggplot(perc_and_mass_m, aes(Taxon, value)) +
  geom_bar( position = "dodge", stat="identity") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("") +
  ylab("") +
  facet_grid(variable ~ Spit, scales = "free_y")

# save plot as PNG
ggsave(file = "figures/Fig_14_shell_from_1989_for_paper.png", g, dpi = 1200)

return(g)

}
