# metals


library(readxl)
library(readr)
metals <- read_csv("metals.csv", 
                   col_types = cols(Mg = col_number(), 
                                    V = col_double(), Cr = col_double(), 
                                    Mn = col_double(), Fe = col_double(), 
                                    Co = col_double(), Ni = col_double(), 
                                    Cu = col_double(), Zn = col_double(), 
                                    As = col_double(), Cd = col_skip(), Pb206 = col_skip(), 
                                    Pb207 = col_skip(), Pb208 = col_skip()))
View(metals)

library(dplyr)

metals_tib <-
  metals %>%
  filter(!row_number() %in% c(1, 34, 37)) %>%   # remove calibration and rows that were nitric acid blanks
  mutate(V = na_if(V, V<0.04227964)) %>%   # make all readings under detection limit NAs.
  mutate(Cr = na_if(Cr, Cr<0.00647711)) %>% 
  mutate(Mn = na_if(Mn, Mn<0.06912871)) %>% 
  mutate(Fe = na_if(Fe, Fe<0.22896393)) %>% 
  mutate(Co = na_if(Co, Co<0.00694153)) %>% 
  mutate(Ni = na_if(Ni, Ni<0.12337598)) %>% 
  mutate(Cu = na_if(Cu, Cu<0.0412007)) %>% 
  mutate(Zn = na_if(Zn, Zn<0.01370208)) %>% 
  mutate(As = na_if(As, As<0.05940449)) %>% 
  mutate(Mg = na_if(Mg, Mg<0.44648244))


# time trends
# create dataframes of timeseries to extrapolate from
# 100-264 hours 
aut_extrap_met <- metals_tib[ which(metals_tib$hour>1 & metals_tib$Sediment=="autoclaved"), ]
nat_extrap_met <- metals_tib[ which(metals_tib$hour>1 & metals_tib$Sediment=="natural"), ]
con_extrap_met <- metals_tib[ which(metals_tib$hour>1 & metals_tib$Sediment=="control"), ]

# linear models for the 100-264 hour
V_aut_model <- lm(V~hour, aut_extrap_met)
V_nat_model <- lm(V~hour, nat_extrap_met)
V_con_model <- lm(V~hour, con_extrap_met)

Cr_aut_model <- lm(Cr~hour, aut_extrap_met)
Cr_nat_model <- lm(Cr~hour, nat_extrap_met)
Cr_con_model <- lm(Cr~hour, con_extrap_met)

Mn_aut_model <- lm(Mn~hour, aut_extrap_met)
Mn_nat_model <- lm(Mn~hour, nat_extrap_met)
Mn_con_model <- lm(Mn~hour, con_extrap_met)

Fe_aut_model <- lm(Fe~hour, aut_extrap_met)
Fe_nat_model <- lm(Fe~hour, nat_extrap_met)
Fe_con_model <- lm(Fe~hour, con_extrap_met)

Co_aut_model <- lm(Co~hour, aut_extrap_met)
Co_nat_model <- lm(Co~hour, nat_extrap_met)
Co_con_model <- lm(Co~hour, con_extrap_met)

Ni_aut_model <- lm(Ni~hour, aut_extrap_met)
Ni_nat_model <- lm(Ni~hour, nat_extrap_met)
Ni_con_model <- lm(Ni~hour, con_extrap_met)

Cu_aut_model <- lm(Cu~hour, aut_extrap_met)
Cu_nat_model <- lm(Cu~hour, nat_extrap_met)
Cu_con_model <- lm(Cu~hour, con_extrap_met)

Zn_aut_model <- lm(Zn~hour, aut_extrap_met)
Zn_nat_model <- lm(Zn~hour, nat_extrap_met)
Zn_con_model <- lm(Zn~hour, con_extrap_met)

As_aut_model <- lm(As~hour, aut_extrap_met)
As_nat_model <- lm(As~hour, nat_extrap_met)
As_con_model <- lm(As~hour, con_extrap_met)

Mg_aut_model <- lm(Mg~hour, aut_extrap_met)
Mg_nat_model <- lm(Mg~hour, nat_extrap_met)
Mg_con_model <- lm(Mg~hour, con_extrap_met)



# Build prediction data frame
new <- data.frame(hour = seq(264, 400))

# make data frames of predicted values for future projection to 400 hours
# for autoclaved chemistry
aut_predicted <- data.frame(hour = (seq(264, 400)))
aut_predicted$V <- predict(V_aut_model, new)
aut_predicted$Cr <- predict(Cr_aut_model, new)
aut_predicted$Mn <- predict(Mn_aut_model, new)
aut_predicted$Fe <- predict(Fe_aut_model, new)
aut_predicted$Co <- predict(Co_aut_model, new)
aut_predicted$Ni <- predict(Ni_aut_model, new)
aut_predicted$Cu <- predict(Cu_aut_model, new)
aut_predicted$Zn <- predict(Zn_aut_model, new)
aut_predicted$As <- predict(As_aut_model, new)
aut_predicted$Mg <- predict(Mg_aut_model, new)
aut_predicted
nat_predicted <- data.frame(hour = (seq(264, 400)))
nat_predicted$V <- predict(V_nat_model, new)
nat_predicted$Cr <- predict(Cr_nat_model, new)
nat_predicted$Mn <- predict(Mn_nat_model, new)
nat_predicted$Fe <- predict(Fe_nat_model, new)
nat_predicted$Co <- predict(Co_nat_model, new)
nat_predicted$Ni <- predict(Ni_nat_model, new)
nat_predicted$Cu <- predict(Cu_nat_model, new)
nat_predicted$Zn <- predict(Zn_nat_model, new)
nat_predicted$As <- predict(As_nat_model, new)
nat_predicted$Mg <- predict(Mg_nat_model, new)
nat_predicted
con_predicted <- data.frame(hour = (seq(264, 400)))
con_predicted$V <- predict(V_con_model, new)
con_predicted$Cr <- predict(Cr_con_model, new)
con_predicted$Mn <- predict(Mn_con_model, new)
con_predicted$Fe <- predict(Fe_con_model, new)
con_predicted$Co <- predict(Co_con_model, new)
con_predicted$Ni <- predict(Ni_con_model, new)
con_predicted$Cu <- predict(Cu_con_model, new)
con_predicted$Zn <- predict(Zn_con_model, new)
con_predicted$As <- predict(As_con_model, new)
con_predicted$Mg <- predict(Mg_con_model, new)
con_predicted


#line graphs
(V_p <- ggplot(metals_tib, aes(hour, V)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm, se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "V",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 7.5) +
    geom_point(aes(x=264, y=1.39), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=2.83), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=1.57), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Cr_p <- ggplot(metals_tib, aes(hour, Cr)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm, se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Cr",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 1) +
    geom_point(aes(x=264, y=0.34), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.37), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.15), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Mn_p <- ggplot(metals_tib, aes(hour, Mn)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm, se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Mn",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 2000) +
    geom_point(aes(x=264, y=880.04), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=4.05), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=45.16), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Fe_p <- ggplot(metals_tib, aes(hour, Fe)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm, se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Fe",
         y = expression("Concentration (" * mu ~ "g L" ^-1*")"),
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 500) +
    geom_point(aes(x=264, y=235.44), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=6.48), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=19.07), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Co_p <- ggplot(metals_tib, aes(hour, Co)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm, se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Co",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 0.5) +
    geom_point(aes(x=264, y=0.31), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.04), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.13), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Ni_p <- ggplot(metals_tib, aes(hour, Ni)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm , se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Ni",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 5) +
    geom_point(aes(x=264, y=3.39), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.97), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.85), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Cu_p <- ggplot(metals_tib, aes(hour, Cu)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm , se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Cu",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 25) +
    geom_point(aes(x=264, y=7.29), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=7.84), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=5.46), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank())
)


(Zn_p <- ggplot(metals_tib, aes(hour, Zn)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm , se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Zn",
         y = "",
         x = "",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 60) +
    geom_point(aes(x=264, y=16.46), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=15.35), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=18.42), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5))
)

(As_p <- ggplot(metals_tib, aes(hour, As)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm , se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "As",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(0, 15) +
    geom_point(aes(x=264, y=9.6), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=1.72), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=0.44), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)

(Mg_p <- ggplot(metals_tib, aes(hour, Mg)) +
    geom_point(aes(colour = factor(Sediment, levels = c("autoclaved","natural","control")))) +
    geom_smooth(aes(colour = Sediment), method = lm , se=FALSE, fullrange=FALSE) +
    geom_line(data = aut_predicted, colour="#006633", linewidth = 1, linetype = "dashed") +
    geom_line(data = nat_predicted, colour="#009966", linewidth = 1, linetype = "dashed") +
    geom_line(data = con_predicted, colour="#33FF99", linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("#006633", "#009966", "#33FF99")) +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Mg",
         y = "",
         x = "Time\n(hours from microcosm set up)",
         color = "Sediment type") +
    plot_theme() +
    xlim(0, 400) +
    ylim(6000, 11000) +
    geom_point(aes(x=264, y=10034.14), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=7162.69), stroke = 1, size = 5, shape = 4, colour = "red") +
    geom_point(aes(x=264, y=9267.34), stroke = 1, size = 5, shape = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank())
)



library(patchwork)
patch <- V_p + Cr_p + Mn_p + plot_spacer() + Fe_p + Co_p + Ni_p + guide_area() + Cu_p + Zn_p + As_p + Mg_p + plot_layout(ncol = 4, guides = "collect") + xlab(NULL) + theme(plot.margin = margin(5.5, 5.5, 5.5, 0))
wrap_elements(patch) +
      labs(tag = "Time\n(hours from microcosm set up)") +
      theme(
      plot.tag = element_text(size = rel(1.25)),
      plot.tag.position = "bottom"
          ) 




# BOX PLOTS
long_met <- gather(last_met, met, conc, Mg:As, factor_key=TRUE)
view(long_met)

V_water <- long_met[ which(long_met$met=="V"), ]
(V_bp <- ggplot(V_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "V\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0.5, 3.5) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
)

Cr_water <- long_met[ which(long_met$met=="Cr"), ]
(Cr_bp <- ggplot(Cr_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Cr\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 1) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
)


Mn_water <- long_met[ which(long_met$met=="Mn"), ]
(Mn_bp <- ggplot(Mn_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Mn\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 1500) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
)


Fe_water <- long_met[ which(long_met$met=="Fe"), ]
(Fe_bp <- ggplot(Fe_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Fe\nn=17",
         y = expression("       Concentration (" * mu ~ "g L" ^-1*")"),
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 500) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
)

Co_water <- long_met[ which(long_met$met=="Co"), ]
(Co_bp <- ggplot(Co_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Co\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 0.5) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
)

Ni_water <- long_met[ which(long_met$met=="Ni"), ]
(Ni_bp <- ggplot(Ni_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Ni\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 5) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
)

Cu_water <- long_met[ which(long_met$met=="Cu"), ]
(Cu_bp <- ggplot(Cu_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Cu\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 23) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1))
)


Zn_water <- long_met[ which(long_met$met=="Zn"), ]
(Zn_bp <- ggplot(Zn_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Zn\nn=17",
         y = "",
         x = "",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 55) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust=1))
)


As_water <- long_met[ which(long_met$met=="As"), ]
(As_bp <- ggplot(As_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "As\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(0, 13) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1))
)

Mg_water <- long_met[ which(long_met$met=="Mg"), ]
(Mg_bp <- ggplot(Mg_water, aes(factor(Sediment, levels = c("control","natural","autoclaved"), ordered = TRUE), conc)) +
    geom_boxplot(aes(fill = factor(Sediment, levels = c("autoclaved","natural","control"))), show.legend = FALSE) +
    geom_point() +
    scale_fill_manual(values = c("#006633", "#009966", "#33FF99")) +
    labs(title = "Mg\nn=17",
         y = "",
         x = "Sediment type",
         color = "Sediment type",
         fill = "Sediment type") +
    plot_theme() +
    ylim(6000, 11000) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1))
)



library(patchwork)
patch <- V_bp + Cr_bp + Mn_bp + plot_spacer() + Fe_bp + Co_bp + Ni_bp + plot_spacer() + Cu_bp + Zn_bp + As_bp + Mg_bp + plot_layout(ncol = 4, guides = "collect")
wrap_elements(patch) +
  labs(tag = "Sediment type") +
  theme(
    plot.tag = element_text(size = rel(1.25)),
    plot.tag.position = "bottom"
  ) 









#STATISTICS

# get last timepoint
last_met <- metals_tib[ which(metals_tib$hour=='264'), ]



library(dplyr)
library(tidyr)
install.packages("pastecs")
library(psych)
met_sum <- describeBy(last_met, last_met$Sediment)

citation(lme4)

# linear model test and assumptions
# LINEAR MODEL ANOVA
modV <- lm(V ~ Sediment, last_met)
check_model(modV)
resids <- resid(modV) 
shapiro.test(resids) # normal
bartlett.test(V ~ Sediment, last_met) # homog
plot(modV)
summary(modV)
anova(modV)
Vaov <- aov(V ~ Sediment, last_met) 
TukeyHSD(Vaov)
plot(TukeyHSD(Vaov, conf.level=.95), las = 2)

modCr <- lm(log(Cr) ~ Sediment, last_met)
check_model(modCr)
resids <- resid(modCr) 
shapiro.test(resids) # normal with log
bartlett.test(log(Cr) ~ Sediment, last_met) # homog with log
plot(modCr)
summary(modCr)
anova(modCr)
Craov <- aov(log(Cr) ~ Sediment, last_met) 
TukeyHSD(Craov)
plot(TukeyHSD(Craov, conf.level=.95), las = 2)


modMn <- lm(Mn ~ Sediment, last_met)
check_model(modMn)
resids <- resid(modMn) 
shapiro.test(resids) # normal if log
bartlett.test(log(Mn) ~ Sediment, last_met) # NOT homog!!

# non-parametric tests
kruskal.test(Mn ~ Sediment, data = last_met) 
attach(last_met)
conover.test(Mn, Sediment, method="bh", list=TRUE)
detach(last_met)





modFe <- lm(log(Fe) ~ Sediment, last_met)
check_model(modFe)
resids <- resid(modFe) 
shapiro.test(resids) # normal with log
bartlett.test(log(Fe) ~ Sediment, last_met) # homog with log
plot(modFe)
summary(modFe)
anova(modFe)
Feaov <- aov(log(Fe) ~ Sediment, last_met) 
TukeyHSD(Feaov)
plot(TukeyHSD(Feaov, conf.level=.95), las = 2)


modCo <- lm(Co ~ Sediment, last_met)
check_model(modCo)
resids <- resid(modCo) 
shapiro.test(resids) # normal 
bartlett.test(Co ~ Sediment, last_met) # homog 
plot(modCo)
summary(modCo)
anova(modCo)
Coaov <- aov(Co ~ Sediment, last_met) 
TukeyHSD(Coaov)
plot(TukeyHSD(Coaov, conf.level=.95), las = 2)


modNi <- lm(Ni ~ Sediment, last_met)
check_model(modNi)
resids <- resid(modNi) 
shapiro.test(resids) # normal 
bartlett.test(Ni ~ Sediment, last_met) # homog 
plot(modNi)
summary(modNi)
anova(modNi)
Niaov <- aov(Ni ~ Sediment, last_met) 
TukeyHSD(Niaov)
plot(TukeyHSD(Niaov, conf.level=.95), las = 2)


modCu <- lm(log(Cu) ~ Sediment, last_met)
check_model(modCu)
resids <- resid(modCu) 
shapiro.test(resids) # normal with log
bartlett.test(log(Cu) ~ Sediment, last_met) # homog with log
plot(modCu)
summary(modCu)
anova(modCu) # no significance



modZn <- lm(log(Zn) ~ Sediment, last_met)
check_model(modZn)
resids <- resid(modZn) 
shapiro.test(resids) # normal with log
bartlett.test(log(Zn) ~ Sediment, last_met) # homog with log
plot(modZn)
summary(modZn)
anova(modZn) # not significant


modAs <- lm(log(As) ~ Sediment, last_met)
check_model(modAs)
resids <- resid(modAs) 
shapiro.test(resids) # normal with log
bartlett.test(log(As) ~ Sediment, last_met) # homog with log
plot(modAs)
summary(modAs)
anova(modAs)
Asaov <- aov(log(As) ~ Sediment, last_met) 
TukeyHSD(Asaov)
plot(TukeyHSD(Asaov, conf.level=.95), las = 2)


modMg <- lm(log(Mg) ~ Sediment, last_met)
check_model(modMg)
resids <- resid(modMg) 
shapiro.test(resids) # normal
bartlett.test(log(Mg) ~ Sediment, last_met) # not homog even with log

# non-parametric tests
kruskal.test(Mg ~ Sediment, data = last_met) 
attach(last_met)
conover.test(Mg, Sediment, method="bh", list=TRUE)
detach(last_met)





# stacked bar plots


# dataframe of averages
endy_metaly <- 
  long_met %>% 
  group_by(Sediment, met) %>% 
  summarise(
    conc_av = mean(conc))

# create plot using data - NO POINT INCLUDING YO

endy_mendy <- read_csv("endy_mendy.csv")
View(endy_mendy)


(endier_mendier <-
     endy_mendy %>% ggplot(aes(x= conc, 
                               y= Sediment,
                               fill= factor(met, levels=c("V","Cr","Mn","Fe","Co","Ni", "Cu", "Zn", "As"))))+
    geom_bar(color= "black", width = 0.6, stat= "identity") +
    geom_area(
      # Last two stacked bars
      data = ~ subset(.x, Sediment %in% c("control", "natural")),
      # These exact values depend on the 'width' of the bars
      aes(y = c("control" = 2.7, "natural" = 2.3)[as.character(Sediment)]),
      position = "stack", outline.type = "both", 
      # Alpha set to 0 to hide the fill colour
      alpha = 0, colour = "black",
      orientation = "y"
    ) +
    geom_area(
      # First two stacked bars
      data = ~ subset(.x, Sediment %in% c("natural", "autoclaved")),
      aes(y = c("autoclaved" = 1.3, "natural" = 1.7)[as.character(Sediment)]),
      position = "stack", outline.type = "both", alpha = 0, colour = "black",
      orientation = "y"
    ) +
    scale_fill_manual(name= NULL,
                      breaks = c("V","Cr","Mn","Fe","Co","Ni", "Cu", "Zn", "As"),
                      values=  c("#DFE667","#9DBD62", "#72B59C", "#96BCC6", "#5D7CAA","#526080", "red", "blue", "green"))+
    scale_y_discrete(breaks=c("control",
                              "natural",
                              "autoclaved"),
                     labels=c("control  (n=6)",
                              "natural  (n=6)",
                              "autoclaved  (n=6)"))+
    labs(x = expression("Concentration (mg L" ^-1*")"), 
         fill = "Chemical parameter",
         y = "Sediment type") +
    geom_segment(aes(x=10.3, xend=12.3, y=2.7, yend=2.3), size = 1, colour = "white", linetype=2)+
    geom_segment(aes(x=39, xend=41, y=3, yend=3), colour = "black", linetype=2) +
    annotate(geom = "point", shape=8, size=3, colour = "red", x = 11.25, y = 2.5) + 
    annotate(geom = "point", shape=8, size=3, colour = "red", x = 40, y = 3) + 
    annotate(geom = "text", x = 44, y = 3, size=5, label="statistically\ninsignificant") +
    plot_theme() +
    theme(legend.position = c(0.88, 0.6))
)






  