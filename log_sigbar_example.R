#' ---
#' title: "Example of the logscale_sigabr_generator"
#'output:
#'  html_document:
#'    knitr:
#'      opts_knitr:
#'        warning: FALSE
#'        tidy: FALSE
#'    toc: true
#'    theme: united
#' ---


#/*making a note book with knitr.spin is preferable because of the dynamic possible code debugging*/
#/* YAML header, #' for drop into Rmarkdown (#, ## headers), #+ for chunks (try not to disrupt code chunks with comments, place before)*/

#' #Set up
#'  
#+ import-libraries, message=FALSE
#import libraries
require(gplots)
require(ggplot2)
require(ggthemes)
require(scales)
require(grid)
require(dplyr)
require(plyr)
require(data.table)

#+ style
theme_mod <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

#+ functions
logscale_sigbars_generator <- function (max_draw_dim, min_draw_dim, number_bar_levels = 1, tick_size = 0.01, default_step =1.5) {
  #someday it'll be nice to have some input verification
  print("positions generated:", quote = FALSE)
  print("the levels are counted from the bottom to top",quote = FALSE) 
  print("p[level, 1:4] are the positions", quote = FALSE)
  print("p[level, 5] contains the text position", quote = FALSE) #some guidance
  range <- log(max_draw_dim) - log(min_draw_dim) #the range that the bars will be plotted in
  tick_size_log <- log(max_draw_dim) * tick_size # the size of the downturned ticks
  step <- ifelse((range / number_bar_levels) < 1.5, (range / number_bar_levels), 1.5) # the spacing between bars
  p <- matrix(0,number_bar_levels, 5) # the matrix of the results
  for (i in 1:number_bar_levels) {
    bar_position <- log(min_draw_dim) + (step*i)
    tick_postion <- bar_position - tick_size_log
    text_position <- bar_position + (1.5 * tick_size_log)
    p[i,] <- as.numeric(c(exp(tick_postion), exp(bar_position), exp(bar_position),exp(tick_postion), exp(text_position)))
  } #make it
  return(p)
}

#+ Session-info
sessionInfo() #for reproducibility

#' #Load Data
#' 
load(example)

#' ##Relabel
#' 
#' now it's showtime
#' 

#' ##Make graphs
#' 
#' the data, plot the p-values from the dunn's post hoc in overlay
#' 
#+ labels
label1 <- expression(italic(agrA[C123F]))
label2 <- expression(italic(Delta*atl))
label3 <- expression(italic(paste("icaA", ":", ":", "erm")))
label4 <- expression(italic(paste("srtA", ":", ":", "erm")))
label5 <- "wild type"
label6 <- expression(italic(paste("hla", ":", ":", "erm")))
facet1 <- data.frame(x = 1:4, y = 1:4, timepoint = "72 hours") #for the overlay later to allow for drawing stats comparison paths
facet2 <- data.frame(x = 1:4, y = 1:4, timepoint = "120 hours") #for the overlay later to allow for drawing stats comparison paths
norm_summary$timepoint <- mapvalues(norm_summary$timepoint, c("72", "120"), c("72 hours", "120 hours"))
norm_summary$sample_id <- factor(norm_summary$sample_id, levels(norm_summary$sample_id)[c(5,1,2,6,3,4)])
#state the comparisons
comparisons <- list(c(1,2), c(1,3), c(1,4), c(1,5), c(1,6))
# to calculate the postions for statisitical significance bars o a log scale
p <- logscale_sigbars_generator(1e11, 2e9, 5)

#+ overview-plot, fig.width=7, fig.height=7
KO_overview <- ggplot(norm_summary, aes(sample_id, cfu)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.25), size = 0.3) +
  scale_x_discrete(labels = c(label5, label1, label2, label6, label3, label4)) +
  labs(x = " ", y = "CFU / raft") +
  coord_cartesian(ylim = c(1e5, 1e11)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~timepoint) +
  theme_mod + 
  #facet 1
  geom_path(aes(x=rep(comparisons[[1]], each = 2),y=p[1,1:4]), data = facet1) +
  geom_text(aes(x=median(comparisons[[1]]),y=p[1,5],label="p==4.34%*%10^{-6}"), data = facet1, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[2]], each = 2),y=p[2,1:4]), data = facet1) +
  geom_text(aes(x=median(comparisons[[2]]),y=p[2,5],label="p==8.71%*%10^{-5}"), data = facet1, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[3]], each = 2),y=p[3,1:4]), data = facet1) +
  geom_text(aes(x=median(comparisons[[3]]),y=p[3,5],label="p==2.41%*%10^{-1}"), data = facet1, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[4]], each = 2),y=p[4,1:4]), data = facet1) +
  geom_text(aes(x=median(comparisons[[4]]),y=p[4,5],label="p==1.94%*%10^{-1}"), data = facet1, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[5]], each = 2),y=p[5,1:4]), data = facet1) +
  geom_text(aes(x=median(comparisons[[5]]),y=p[5,5],label="p==1.00"), data = facet1, parse = TRUE) +
  # facet 2
  geom_path(aes(x=rep(comparisons[[1]], each = 2),y=p[1,1:4]), data = facet2) +
  geom_text(aes(x=median(comparisons[[1]]),y=p[1,5],label="p==3.25%*%10^{-7}"), data = facet2, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[2]], each = 2),y=p[2,1:4]), data = facet2) +
  geom_text(aes(x=median(comparisons[[2]]),y=p[2,5],label="p==6.67%*%10^{-2}"), data = facet2, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[3]], each = 2),y=p[3,1:4]), data = facet2) +
  geom_text(aes(x=median(comparisons[[3]]),y=p[3,5],label="p==5.23%*%10^{-1}"), data = facet2, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[4]], each = 2),y=p[4,1:4]), data = facet2) +
  geom_text(aes(x=median(comparisons[[4]]),y=p[4,5],label="p==9.95%*%10^{-1}"), data = facet2, parse = TRUE) +
  geom_path(aes(x=rep(comparisons[[5]], each = 2),y=p[5,1:4]), data = facet2) +
  geom_text(aes(x=median(comparisons[[5]]),y=p[5,5],label="p==1.00"), data = facet2, parse = TRUE)

KO_overview #let's see what it makes!