# N. flexilis viability
# Dissertation part 1: Seed colour and stain viability
# Elizabeth Stroud; s1828407@ed.ac.uk
# 15.11.22

# WD + libraries----

library(vegan)
library(ape)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)

# Data processing
library(tidyverse)  # the good stuff 
library(readxl)  # read in .xlsx files

# Modelling 
library(lme4)  # mixed linear models 
library(sjPlot)  # 
library(ggeffects)  # 
library(cowplot)  # 
library(broom)
library(broom.mixed)
library(glue)
install.packages("xfun", type="binary")
library(ggplot2)
library(patchwork)
library(sjPlot)



# WD----

setwd("~/Desktop/Coding/CodingClub/Dissertation")
colvi <- read.csv("seed_colvi_long.csv")


# Functions----

#plot theme function 

plot_theme <- function(...){
  theme_classic() +
    theme(                                
      axis.text = element_text(size = 14,                          # adjust axes
                               color = "black"),
      axis.text.x = element_text(margin = margin(5, b = 10)),
      axis.title = element_text(size = 14,
                                color = 'black'),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "white",               # adjust background colors
                                     color = NA),
      panel.background = element_rect(fill = "white",
                                      color = NA),
      legend.background = element_rect(fill = NA,
                                       color = NA),
      legend.title = element_text(size = 14),                      # adjust titles
      legend.text = element_text(size = 14, hjust = 0,
                                 color = "black"),
      plot.title = element_text(size = 17,
                                color = 'black',
                                margin = margin(10, 10, 10, 10),
                                hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "black",
                                   margin = margin(0, 0, 30, 0)),
      plot.caption = element_text(size = 15,
                                  color = 'black',
                                  margin = margin(10, 10, 10, 10),
                                  hjust = 0.5))
}

plots_theme <- function(...) {
  
  theme_bw() + 
    theme(
      # Title and subtitle
      plot.title = element_text(face = 'bold', 
                                hjust = 0.5, 
                                size = 24),
      plot.subtitle = element_text(hjust = 0.5,
                                   size = 16,
                                   colour = '#ffc95e'),
      
      # Axes 
      axis.title = element_text(colour = 'black', size = 48, face = 'bold'),
      axis.text = element_text(colour = 'black', size = 48),
      axis.title.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
      axis.title.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
      
      # Grid 
      panel.grid = element_blank(),
      
      # Legend
      legend.text = element_text(color = 'black', size = 12, hjust = 0.5, vjust = 0.5),
      legend.position = 'bottom',
      legend.key.width = unit(0.5, 'cm'), 
      
      # Strips
      strip.background = element_blank(),
      strip.text = element_blank()
      
      
    )
  
}


# SE function

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarised
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm:  whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



# Viability modelling---- 

# Null model
null.stain.mod <- glm(stain ~ 1, data = colvi)
summary(null.stain.mod)

null.viab.mod <- glm(viability ~ 1, data = colvi, family = binomial)
summary(null.viab.mod)


# Simple binomial model: stain ~ nat col
stain.mod <- glm(stain ~ nat_col, 
               data = colvi)
summary(stain.mod)

viab.mod <- glm(viability ~ nat_col, data = colvi, family = binomial)
summary(viab.mod)

# for the sediment type
viab.mod <- glm(viability ~ seed_type, data = colvi, family = binomial)
summary(viab.mod)
stain.mod <- glm(stain ~ seed_type, 
                 data = colvi)
summary(stain.mod)


# Binomial model with seed type
bin.mod.type <-glm(stain ~ nat_col*seed_type, 
               data = colvi)

summary(bin.mod.type)



# OLD PLOT - SE dot and line plot ----


# dataframe of summary values
colvic <- summarySE(colvi, measurevar="stain", groupvars=c("nat_col","seed_type"))
colvic

# position dodge for overlapping error bars
pd <- position_dodge(0.35) # move them .05 to the left and right

# plots with SE
(ggplot(colvic, aes(x=nat_col, y=stain, colour=seed_type, group=seed_type)) + 
  geom_errorbar(aes(ymin=stain-se, ymax=stain+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Lightness of natural seed colour") +
  ylab("Probaility of full viability TTC stain") +
  scale_colour_manual(
    values = c("#E67AA1", "#5BBFF3", "#648900"),
    name = "Seed stock",
    labels = c("Autoclave 23/9/22", "Original 15/6/22" , "Transplant 1. 11/8/22")) +
  theme_classic())


# modeling stain tests
# GLM for binary viablity

colvi_mod <- glm(viability ~ nat_col + seed_type, 
               data = colvi, family = binomial)

summary(colvi_mod)
plot(colvi_mod) # dont really need this as our model is binomial..

#make a statistical model for ANOVA
#we do a linear model for this data
aov(colvi_mod)
summary(colvi_mod)
(colvi_aov <- aov(viability ~ nat_col + seed_type, 
                  data = colvi, family = binomial))
TukeyHSD(colvi_aov)


#TEST ANOVA ASSUMPTIONS
#extract residuals
height_resids <- resid(colvi_mod)
#shapiro tests normality
shapiro.test(height_resids)
#equality of variances
bartlett.test(viability ~ nat_col, 
              data = colvi)
bartlett.test(viability ~ seed_type, 
              data = colvi)
#press enter in plots to see model fit
plot(colvi_mod)

#TEST between CATEGORY SIGNIFICANCE - TUKEY
(height_aov <- aov(viability ~ nat_col + seed_type, 
                   data = colvi))
TukeyHSD(height_aov)





# ordinal regression
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

m <- polr(stain ~ nat_col, data = colvi, Hess=TRUE)
summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(m)




colvi_mod <- polr(stain~nat_col, data = colvi, Hess = TRUE)
colvi_table <- coef(summary(colvi_mod))
pval <- pnorm(abs(colvi_table[, "t value"]),lower.tail = FALSE)* 2
colvi_table <- cbind(colvi_table, "p value" = round(pval,3))
colvi_table
summary(colvi_mod)
anova(colvi_mod)
colvi_resids <- resid(colvi_mod)

# get confidence intervals
# profiled CIs
ci <- round(confint(colvi_mod), 4)
# log odd coefficients
or <- round(coef(colvi_mod), 4)
# convert coefficients into odds ratio, combine with CIs
round(exp(cbind(OR = or, ci)), 4)

#shapiro tests normality
shapiro.test(colvi_resids)
#equality of variances
bartlett.test(stain~nat_col, data=colvi)
#press enter in plots to see model fit
plot(colvi_mod)

#TEST between CATEGORY SIGNIFICANCE - TUKEY
(height_aov <- aov(height~fertiliser,data=alldata))
TukeyHSD(height_aov)


model_fit <- polr(stain~col, data = colvi, Hess = TRUE)
summary(model_fit)
summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table


#make a statistical model for ANOVA
#we do a linear model for this data
(height_lm <- lm(height~fertiliser,data=alldata))
anova(height_lm)
summary(height_lm)

#TEST ANOVA ASSUMPTIONS
#extract residuals
height_resids <- resid(height_lm)
#shapiro tests normality
shapiro.test(height_resids)
#equality of variances
bartlett.test(height~fertiliser,data=alldata)
#press enter in plots to see model fit
plot(height_lm)

#TEST between CATEGORY SIGNIFICANCE - TUKEY
(height_aov <- aov(height~fertiliser,data=alldata))
TukeyHSD(height_aov)

### BAR PLOTS----

# seed colour stacked bar

(plot1 <- ggplot(colvi, 
                 aes(x = factor(nat_col,
                                labels = c("1\n\nn=30", "2\n\nn=30", "3\n\nn=30", 
                                           "4\n\nn=30", "5\n\nn=33", "6\n\nn=31")),
                     fill = factor(stain, 
                                   levels = c("1", "0.5", "0", "-1"),
                                   labels = c("full", 
                                              "partial", 
                                              "none",
                                              "no embryo")))) +
    geom_bar(colour = "black", position = "fill") +
    scale_color_manual(values=c("#660033", "#CC6699", "#FFCCCC", "white")) +
    scale_fill_manual(values=c("#660033", "#CC6699", "#FFCCCC", "white")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Proportion of seeds", 
         fill = "TTC stain",
         x = 'Natural seed coat\ncolour lightness',
         title = 'a)') +
    guides(pattern = guide_legend(override.aes = list(fill = "white")))+
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    plot_theme() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(size = 9),
          axis.title.x = element_text(size = 13),
          plot.title = element_text(hjust = -0.05))
)



# seed stock stacked bar

(plot2 <- ggplot(colvi, 
             aes(x = factor(seed_type,
                labels = c("autoclaved\nsediment\nn=60", "original\n\nn=64", "transplant\n\nn=60")),
                fill = factor(stain, 
                              levels = c("1", "0.5", "0", "-1"),
                              labels = c("full", 
                                         "partial", 
                                         "none",
                                         "no embryo")))) + 
    #scale_pattern_density_manual(values = c(0.1, 0.1, 0.1)) + # manually assign pattern
    geom_bar(colour = "black", position = "fill") +
    scale_color_manual(values=c("#660033", "#CC6699", "#FFCCCC", "white")) +
    scale_fill_manual(values=c("#660033", "#CC6699", "#FFCCCC", "white")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "", 
         fill = "TTC stain",
         x = 'Seed stock',
         title = 'b)') +
    guides(pattern = guide_legend(override.aes = list(fill = "white")))+
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    theme(legend.key.size = unit(1.5, 'cm'))+
    plot_theme() +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y.left = element_blank(),
          axis.text.x = element_text(size = 9),
          axis.title.x = element_text(size = 13),
          plot.title = element_text(hjust = -0.05)) 
)


# combining the plots into a panel

patch <- plot1 + plot2

patch

# STATABRACADABRA
library(stats)

# Make colours into ordinal data
colvi$nat_col <- factor(colvi$nat_col, 
                        order = TRUE, 
                        levels = c("1", "2", "3","4", "5", "6"))
colvi$col <- factor(colvi$col,
                    order = TRUE,
                    levels = c("1", "2", "3")) # the 3 colour groups
# Make stain into categorical unordered data 
# (the difference between the categories is not equal to allow ordinal)
colvi$stain <- factor(colvi$stain,
                      order = FALSE)
                #levels = c("-1", "0", "0.5", "1")) # 3 stain groups
colvi$viability <- as.numeric(colvi$viability) # 2 viablility

str(colvi)

# kruskal test for categories
kruskal.test(stain ~ nat_col, data = colvi) 
kruskal.test(stain ~ col, data = colvi) 
kruskal.test(stain ~ seed_type, data = colvi) 
kruskal.test(viability ~ seed_type, data = colvi) 

# post-hoc Dunn test
library(PMCMRplus)
library(rcompanion)
DT = kwAllPairsDunnTest(stain ~ nat_col, data=colvi, method="bh")
DTT =PMCMRTable(DT)
DTT


# binary regression for viability

model0 <- glm(viability ~ 1 ,family="binomial",data=colvi)
model1 <- glm(viability ~ nat_col + seed_type ,family="binomial",data=colvi)
summary(aov(model1))
summary(aov(model1))

summary(model1)
plot(model1)
confint(model1)
report(model1)


# ordinal regression model
colvi_mod <- polr(stain~nat_col, data = colvi, Hess = TRUE)
summary(colvi_mod)


#make a statistical model for ANOVA
#we do a linear model for this data
(stain_lm <- glm(stain~nat_col,data=colvi))
anova(stain_lm)
summary(stain_lm)

#TEST ANOVA ASSUMPTIONS
#extract residuals
height_resids <- resid(height_lm)
#shapiro tests normality
shapiro.test(height_resids)
#equality of variances
bartlett.test(height~fertiliser,data=alldata)
#press enter in plots to see model fit
plot(height_lm)

#TEST between CATEGORY SIGNIFICANCE - TUKEY
(height_aov <- aov(height~fertiliser,data=alldata))
TukeyHSD(height_aov)


### GERMINATION SUCCESS ----
seedat <- read_csv("seed_germ.csv")
View(seedat)

model0 <- glm(germination ~ 1 ,family="binomial",data=seedat)
summary(model0)


model1 <- glm(germination ~ colour + sediment ,family="binomial",data=seedat)
summary(model1)
plot(model1)
confint(model1)


predicted <- sjPlot::plot_model(model1, type = "pred", terms = c("sediment"))$data


(germ_p <- ggplot(seedat, 
                  aes(factor(sediment, 
                             levels=c("autoclaved","natural","control")), 
                      germination)) +
      geom_point(position = position_jitter(width=.1,height=.1),
                   aes(colour = colour), size = 2, alpha = 0.7) +
      scale_color_manual(values=c("#660000", "#CC6633", "#FFCC00")) +
      scale_fill_manual(values=c("#660000", "#CC6633", "#FFCC00")) +
      plot_theme() +
      scale_x_discrete(labels=c("autoclaved" = "n=48", 
                                "natural" = "n=48",
                                "control" = "n=48")) +
      geom_point(aes(x=1,y=0.67),colour="black") +
      geom_errorbar(aes(x = 1, 
                        ymin = 0.48,
                        ymax = 0.82)) +
      geom_point(aes(x=2,y=0.44),colour="black") +
      geom_errorbar(aes(x = 2, 
                        ymin = 0.27,
                        ymax = 0.63)) +
      geom_point(aes(x=3,y=0.20),colour="black") +
      geom_errorbar(aes(x = 3, 
                        ymin = 0.09,
                        ymax = 0.37)) +
      geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5) +
      labs(y = "Germination probability",
           x = "Sediment type",
           title = 'a)',
           color = substitute(paste(italic(' N. flexilis '), 'seed colour'))) +
     theme(axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = -0.05)) +
     scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.25))
    )


library(tidyverse)
library(PupillometryR)



### ROOT:SHOOT ----

ratio_mod <- lm(root/shoot ~ sediment, seedat)
summary(ratio_mod)
library(dplyr)
library(caret)
library(tidyverse)

seedat <- mutate(seedat, ratio = (root/shoot))


(ratio_p <- ggplot(seedat, 
                   aes(factor(sediment, levels=c("autoclaved","natural","control")), 
                       root/shoot)) +
   geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
   #geom_point(position = position_jitter(width = 0.1), size = 1, alpha = 0.9,
               #aes(colour = colour)) +
   geom_point(position = position_jitter(width=.1, height=.05),
               aes(colour = colour), size = 2,  alpha = 0.7) +
   geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.0) +
   #geom_violin(trim=FALSE) +
   #geom_point(position = position_jitter(width=.05,height=.05),
              #aes(colour = colour)) +
   scale_color_manual(values=c("#660000", "#CC6633", "#FFCC00")) +
   scale_fill_manual(values=c("#660000", "#CC6633", "#FFCC00")) +
   geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
   labs(y = "Root:shoot",
        x = "Sediment type",
        title = 'c)',
        color = substitute(paste(italic(' N. flexilis '), 'seed colour'))) +
    scale_x_discrete(labels=c("autoclaved" = "n=30\n\nautoclaved", 
                              "natural" = "n=20\n\nnatural",
                              "control" = "n=8\n\ncontrol")) +
   scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
   plot_theme() +
   theme(legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         legend.position = 'none',
         plot.title = element_text(hjust = -0.05))
)

# BIT TO RUN NOW
(ratio_p <- ggplot(seedat, 
                   aes(factor(sediment, levels=c("autoclaved","natural","control")), 
                       root/shoot)) +
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8, aes(fill = sediment)) +
    #geom_point(position = position_jitter(width = 0.1), size = 1, alpha = 0.9,
    #aes(colour = colour)) +
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5, aes(fill = sediment, alpha = 0.0)) +
    geom_point(position = position_jitter(width=.1, height=.05),
               aes(shape = colour), size = 2,  alpha = 1) +
    #geom_violin(trim=FALSE) +
    #geom_point(position = position_jitter(width=.05,height=.05),
    #aes(colour = colour)) +
    #scale_color_manual(values=c("#007F5F", "#FF9800","#80B918")) +
    scale_fill_manual(values=c("#007F5F", "#FF9800","#80B918")) +
    scale_shape_manual(values=c(1,2,4)) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
    labs(y = "Root:shoot",
         x = "Sediment type",
         title = 'c)',
         color = substitute(paste(italic(' N. flexilis '), 'seed colour'))) +
    scale_x_discrete(labels=c("autoclaved" = "n=30\n\nautoclaved", 
                              "natural" = "n=20\n\nnatural",
                              "control" = "n=8\n\ncontrol")) +
    scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
    plot_theme() +
    theme(legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position = 'none',
          plot.title = element_text(hjust = -0.05))
)

library(ggstatsplot)

ggbetweenstats(
  data = seedat,
  x = sediment,
  y = leaf_no,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

ggbetweenstats(
  data = seedat,
  x = sediment,
  y = ratio,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)


### LEAF COUNT ----

leaf_mod <- lm(leaf_no ~ sediment, seedat)
summary(leaf_mod)

View(seedat)


(leaf_p <- ggplot(seedat, aes(factor(sediment, levels=c("autoclaved","natural","control")), leaf_no)) +
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    #geom_point(position = position_jitter(width = 0.1), size = 1, alpha = 0.9,
               #aes(colour = colour)) +
    geom_point(position = position_jitter(width=.1,height=.05),
               aes(colour = colour), size = 2,  alpha = 0.7) +
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.0) +
    scale_color_manual(values=c("#660000", "#CC6633", "#FFCC00")) +
    scale_fill_manual(values=c("#660000", "#CC6633", "#FFCC00")) +
    labs(y = "Number of leaves",
         x = "Sediment type",
         color = substitute(paste(italic(' N. flexilis '), 'seed colour')),
         title = 'b)') +
    #legend.title = element_text(size = 12),
    #legend.text = element_text(size = 10)) +
    scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
    scale_x_discrete(labels=c("autoclaved" = "n=30", 
                              "natural" = "n=20",
                              "control" = "n=8")) +
    plot_theme() +
    theme(axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = -0.05))
)

# combine germination plots
patch3 <- germ_p + leaf_p + ratio_p + plot_layout(ncol = 1, heights = c(10,7,7), guides = "collect")



### CHEMICAL CONCs ----
library(readr)
water_log <- read_csv("water_log.csv", 
                      col_types = cols(ID_no = col_character(), Sediment = col_factor(),
                                       Time = col_integer(), Repeat = col_character(), 
                                       DO = col_number(), `DO%` = col_number(), 
                                       Dotemp = col_number(), Cond = col_number(), 
                                       Condtemp = col_number(), pH = col_number(), 
                                       pHtemp = col_number(), DOC = col_number(), 
                                       DN = col_number(), SRP = col_number(), 
                                       TP = col_number(), DIC = col_number()))
View(water_log)


(SRP_p <- ggplot(water_log, aes(Time, SRP)) +
      geom_point(aes(colour = Sediment)) +
      geom_smooth(aes(colour = Sediment)) +
      scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
      scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
      labs(title = "Soluble reactive\nphosphorous",
         y = expression("Concentration (mg l" ^-1*")")) +
      plot_theme() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()))
(DOC_p <- ggplot(water_log, aes(Time, DOC)) +
    geom_point(aes(colour = Sediment)) +
    geom_smooth(aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Dissolved organic\ncarbon",
         y = expression("Concentration (mg l" ^-1*")")) +
    plot_theme() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank()))
(DN_p <- ggplot(water_log, aes(Time, DN)) +
      geom_point(aes(colour = Sediment)) +
      geom_smooth(aes(colour = Sediment)) +
      scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
      scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
      labs(title = "Dissolved nitrogen",
           y = expression("Concentration (mg l" ^-1*")")) +
      plot_theme() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank()))
(DO_p <- ggplot(water_log, aes(Time, DO)) +
      geom_point(aes(colour = Sediment)) +
      geom_smooth(aes(colour = Sediment)) +
      scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
      scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
      labs(title = "Dissolved oxygen") +
      plot_theme() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()))
(TP_p <- ggplot(water_log, aes(Time, TP)) +
      geom_point(aes(colour = Sediment)) +
      geom_smooth(aes(colour = Sediment)) +
      scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
      scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
      labs(title = "Total phosphorous",
           x = "Time\n(hours from microcosm set up)",
           color = "Sediment type") +
      plot_theme() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            axis.title.x = element_blank()))
(pH_p <- ggplot(water_log, aes(Time, pH)) +
    geom_point(aes(colour = Sediment)) +
    geom_smooth(aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "pH",
         y = "pH",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(5, 8), breaks = seq(4, 8, by = 1))
  )

aut_extrap_water <- water_log[ which(water_log$Time>90 & water_log$Sediment=="autoclaved"), ]
view(aut_extrap_water)

(Cond_p <- ggplot(water_log, aes(Time, Cond)) +
    geom_point(aes(colour = factor(Sediment, levels=c("autoclaved","natural","control")))) +
    #geom_smooth(extrap_water, method = 'lm', fullrange=TRUE, se=FALSE, aes(colour = Sediment)) + 
    #stat_smooth(fullrange=TRUE, data=subset(extrap_water, Time >= 100), method = 'lm', se = TRUE, aes(colour = Sediment)) +
    geom_smooth(aes(colour = Sediment), se=FALSE) +
    geom_line(data = aut_predicted,  colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Conductivity ",
         y = expression("Conductivity (" * mu ~ "s/cm)"),
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    geom_point(aes(x=264,y=385.17), stroke = 2, size = 5, shape = 4, colour="red") +
    geom_point(aes(x=264,y=330.50), stroke = 2, size = 5, shape = 4, colour="red") +
    geom_point(aes(x=264,y=313.83), stroke = 2, size = 5, shape = 4, colour="red") +
    theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_blank()))


# Build prediction data frame
new <- data.frame(Time = seq(264, 400))

aut_extrap_water <- water_log[ which(water_log$Time>90 & water_log$Sediment=="autoclaved"), ]
nat_extrap_water <- water_log[ which(water_log$Time>90 & water_log$Sediment=="natural"), ]
con_extrap_water <- water_log[ which(water_log$Time>90 & water_log$Sediment=="control"), ]

aut_model <- lm(Cond~Time, aut_extrap_water)
nat_model <- lm(Cond~Time, nat_extrap_water)
con_model <- lm(Cond~Time, con_extrap_water)

aut_predicted <- data.frame(Time = (seq(264, 400)))
aut_predicted$Cond <- predict(aut_model, new)
aut_predicted
nat_predicted <- data.frame(Time = (seq(264, 400)))
nat_predicted$Cond <- predict(nat_model, new)
nat_predicted
con_predicted <- data.frame(Time = (seq(264, 400)))
con_predicted$Cond <- predict(con_model, new)
con_predicted


end_chem %>%
  group_by(chem) %>%
  summarise_at(vars(conc), list(name = mean))





my_lm <- lm(Cond~Time, water_log)
summary(my_lm)
pred_x = c(min(extrap_water$Time),rep(max(extrap_water$Time),2),max(extrap_water$Time)+1)
pred_lines = data.frame(x=pred_x,
                        y=predict(my_lm, data.frame(x=pred_x)),
                        obs_Or_Pred=rep(c("Obs","Pred"), each=2))

ggplot(pred_lines, aes(x, y, colour=obs_Or_Pred, shape=obs_Or_Pred, linetype=obs_Or_Pred)) +
  geom_point(data=data.frame(x,y, obs_Or_Pred="Obs"), size=3) +
  geom_line(size=1) +
  scale_shape_manual(values=c(16,NA)) +
  theme_bw()


patch2 <- DOC_p + SRP_p + DO_p + DN_p + TP_p + guide_area() + Cond_p + pH_p +  guide_area() + plot_layout(ncol = 3, guides = "collect")



# MODELLED LINES NOT TREND LINES - |IGNORE THIS WHOLE SECTION YO----

(SRP_p <- ggplot(water_log, aes(Time, SRP)) +
    geom_point(aes(colour = Sediment)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    stat_smooth(method="lm",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Soluble reactive\nphosphorous",
         y = expression("Concentration (mg l" ^-1*")")) +
    plot_theme() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()))
(DOC_p <- ggplot(water_log, aes(Time, DOC)) +
    geom_point(aes(colour = Sediment)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    stat_smooth(method="lm",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Dissolved organic\ncarbon",
         y = expression("Concentration (mg l" ^-1*")")) +
    plot_theme() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank()))
(DN_p <- ggplot(water_log, aes(Time, DN)) +
    geom_point(aes(colour = Sediment)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    stat_smooth(method="lm",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Dissolved nitrogen",
         y = expression("Concentration (mg l" ^-1*")")) +
    plot_theme() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank()))


myPlot1 <- ggplot(aut_water, aes(Time, DO)) +
  stat_smooth(method='lm', linetype=1, se=TRUE, formula=y~poly(x,2, raw=TRUE), size=0.5, color="dark red")
smooth1 <- ggplot_build(myPlot1)$data[[1]][c("x","y")]
myPlot2 <- ggplot(con_water, aes(Time, DO)) +
  stat_smooth(method='lm', linetype=1, se=TRUE, formula=y~poly(x,2, raw=TRUE), size=0.5, color="dark red")
smooth2 <- ggplot_build(myPlot2)$data[[1]][c("x","y")]
myPlot3 <- ggplot(nat_water, aes(Time, DO)) +
  stat_smooth(method='lm', linetype=1, se=TRUE, formula=y~poly(x,2, raw=TRUE), size=0.5, color="dark red")
smooth3 <- ggplot_build(myPlot3)$data[[1]][c("x","y")]

smooth1$y[which(smooth1$y  == max(smooth1$y)):nrow(smooth1)] <- max(smooth1$y)
smooth2$y[which(smooth2$y  == max(smooth2$y)):nrow(smooth2)] <- max(smooth2$y)
smooth3$y[which(smooth3$y  == max(smooth3$y)):nrow(smooth3)] <- max(smooth3$y)


(DO_p <- ggplot(water_log, aes(Time, DO)) +
    geom_point(aes(colour = Sediment)) +
    geom_line(data = smooth1, aes(x = x, y = y)) +
    geom_line(data = smooth2, aes(x = x, y = y)) +
    geom_line(data = smooth3, aes(x = x, y = y)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    #stat_smooth(method="loess",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Dissolved oxygen") +
    plot_theme() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()))


(TP_p <- ggplot(water_log, aes(Time, TP)) +
    geom_point(aes(colour = Sediment)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    stat_smooth(method="lm",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Total phosphorous",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()))
(pH_p <- ggplot(water_log, aes(Time, pH)) +
    geom_point(aes(colour = Sediment)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    stat_smooth(method="lm",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "pH",
         y = "pH",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(5, 8), breaks = seq(4, 8, by = 1))
)
(Cond_p <- ggplot(water_log, aes(Time, Cond)) +
    geom_point(aes(colour = Sediment)) +
    #geom_smooth(aes(colour = Sediment)) +
    xlim(0,700) +
    stat_smooth(method="lm",fullrange=TRUE, aes(colour = Sediment)) +
    scale_color_manual(values=c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values=c("#006633", "#009966", "#33FF99")) +
    labs(title = "Conductivity ",
         y = expression("Conductivity (" * mu ~ "s/cm)"),
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank()))


patch2 <- DOC_p + SRP_p + DO_p + DN_p + TP_p + guide_area() + Cond_p + pH_p +  guide_area() + plot_layout(ncol = 3, guides = "collect")



# LINEAR PLATEAU MODEL FOR AUTO DO----
aut_water <- water_log[ which(water_log$Sediment=='autoclaved'), ]
library(rcompanion)
library(nlstools)
library(nlstools)

###  Find reasonable initial values for parameters
fit.lm    = lm(DO ~ Time, data=aut_water)
a.ini     = fit.lm$coefficients[1]
b.ini     = fit.lm$coefficients[2]
clx.ini   = mean(aut_water$Time)

###  Define linear plateau function
linplat = function(x, a, b, clx)
{ifelse(x < clx, a + b * x,
        a + b * clx)}

###  Find best fit parameters
model = nls(DO ~ linplat(Time, a, b, clx),
            data = aut_water,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model)
plotPredy(data  = aut_water,
          x     = Time,
          y     = DO,
          model = model,
          xlab  = "Time (hours)",
          ylab  = "DO concentration")

###  Define null model
nullfunct = function(x, m){m}
m.ini    = mean(aut_water$DO)
null = nls(DO ~ nullfunct(Time, m),
           data = aut_water,
           start = list(m   = m.ini),
           trace = FALSE,
           nls.control(maxiter = 1000))

###  Find p-value and pseudo R-squared
nagelkerke(model,
           null)
confint2(model,
         level = 0.95)
Boot = nlsBoot(model)
summary(Boot)

# residual plots
x = residuals(model)
plotNormalHistogram(x)
plot(fitted(model),
     residuals(model))





# Chemistry statistics----

View(water_log)
last_water <- water_log[ which(water_log$Time=='264'), ]
CN_last_water <- water_log[ which(water_log$Time=='100'), ]

# LINEAR MODEL ANOVA
water_modSRP <- lm(SRP ~ Sediment, last_water)
summary(water_modSRP)
#TEST ANOVA ASSUMPTIONS
resids <- resid(water_modSRP) #extract residuals
shapiro.test(resids) #shapiro tests normality
bartlett.test(SRP ~ Sediment, last_water) #equality of variances
plot(water_modSRP) #press enter in plots to see model fit
#TEST between CATEGORY SIGNIFICANCE - TUKEY
(height_aov <- aov(height~fertiliser,data=alldata))
TukeyHSD(height_aov)

# NON_PARA - KRUSKAL WALLACE + DUNN

last_water$Sediment <- as.factor(last_water$Sediment)
CN_last_water$Sediment <- as.factor(last_water$Sediment)
str(colvi)
library(FSA)
# kruskal test for non-normal
kruskal.test(DOC ~ Sediment, data = CN_last_water) 
kruskal.test(DN ~ Sediment, data = CN_last_water) 
kruskal.test(SRP ~ Sediment, data = last_water) 
kruskal.test(DO ~ Sediment, data =  last_water) 
kruskal.test(TP ~ Sediment, data = last_water) 
kruskal.test(Cond ~ Sediment, data = last_water) 
kruskal.test(pH ~ Sediment, data = last_water) 
kruskal.test(DIC ~ Sediment, data = last_water) 

#perform Dunn's Test with BH correction for p-values
dunnTest(DOC ~ Sediment, data = CN_last_water, method="bh")
dunnTest(DN ~ Sediment, data = CN_last_water, method="bh")
dunnTest(SRP ~ Sediment, data = last_water, method="bh")
dunnTest(DO ~ Sediment, data = last_water, method="bh")
dunnTest(TP ~ Sediment, data = last_water, method="bh")
dunnTest(Cond ~ Sediment, data = last_water, method="bh")
dunnTest(DIC ~ Sediment, data = last_water, method="bh")

ggplot(end_chem, aes(chem, conc)) +
  geom_point(aes(colour = sediment)) 
ggplot(end_chemo, aes(sediment, percentage)) +
  #scale_y_continuous(labels = scales::percent) +
  geom_bar(aes(colour = chem)) 

# STACKED BAR CHSRT FOR XHEM CONCENTRATIONS _ ITS A START----
ggplot(end_chem, aes(fill = chem,
                      y = sediment, x = conc)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Weather Data of 4 Cities !")
  #theme(plot.title = element_text(hjust = 0.5))

ggplot(end_chemo, aes(fill = chem,
                     y = sediment, x = percentage)) +
  geom_bar(position = "stack", stat = "identity") 

  
  boxplot(x = sediment, y = conc)
(plot5 <- ggplot(end_chem, 
                 aes(x = factor(sediment,
                                labels = c("autoclaved\nn=60", "natural\n\nn=64", "control\n\nn=60")),
                     fill = factor(chem, 
                                   levels = c("SRP", "TP", "DIC", "DOC", "DN", "DO"),
                                   labels = c("SRP", 
                                              "TP", 
                                              "DIC",
                                              "DOC",
                                              "DN",
                                              "DO")),
                     y = conc)) + 
    geom_bar(colour = "black", position = "fill") +
    #scale_color_manual(values=c("#660033", "#CC6699", "#FFCCCC", "white")) +
    #scale_fill_manual(values=c("#660033", "#CC6699", "#FFCCCC", "white")) +
    #scale_y_continuous(labels = scales::percent) +
    labs(y = "", 
         fill = "Chemical test",
         x = 'Sediment type',
         title = 'a)') +
    guides(pattern = guide_legend(override.aes = list(fill = "white")))+
    theme(legend.key.size = unit(1.5, 'cm'))+
    plot_theme() +
    theme(axis.text.y.left = element_blank(),
          axis.text.x = element_text(size = 9),
          axis.title.x = element_text(size = 13),
          plot.title = element_text(hjust = -0.05)) 
)

