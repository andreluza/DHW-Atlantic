

# -------------------------------------------

# Model interpretation - i)  latitude effect

# -------------------------------------------

rm(list=ls())
require(here)
require(ggplot2)
require(dplyr)
require(reshape)
library(performance)
require(brms)
require(ggbreak)


# load
DHW_data <- read.csv (here("data", "DHW_raw_stats_table.csv"),sep=";")

# replace NAs by zeros (cases where no anomaly was observed)
DHW_data$intensity[is.na(DHW_data$intensity)] <- 0
DHW_data$duration[is.na(DHW_data$duration)] <- 0
DHW_data$interval[is.na(DHW_data$interval)] <- 0

# load estimated models
load(here ("RData","model_count_lat.RData"))
load(here ("RData","model_count2_lat.RData"))
load(here ("RData","model_count3_lat.RData"))


# calculate the Bayes R^2
bayes_R2(model_count3_lat)
bayes_R2(model_count2_lat)
bayes_R2(model_count_lat)


require(gridExtra)
require(ggbreak)
png(here("output", "pp-check_lat.png"),width=15,height=15,unit="cm", res=300)
grid.arrange(pp_check(model_count3_lat,ndraws = 50)+ylab("Frequency")+ggtitle("a")+theme(legend.position = c(0.8,0.8)),
             pp_check(model_count2_lat,ndraws = 50)+ylab("Frequency")+ggtitle("b")+theme(legend.position = "none"),
             pp_check(model_count_lat,ndraws = 50)+ylab("Frequency")+ggtitle("c")+theme(legend.position = "none"),
             nrow=3)

dev.off()


# intensity -----------------------------------------
ce <- conditional_effects(model_count3_lat)
p1 <-ce$lat %>%
  ggplot(aes(x=lat,y=estimate__))+
  theme_classic()+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_point(data = DHW_data %>% 
               filter (intensity >0) %>%
               mutate (region,Region = recode(region, ILOC = "Oceanic islands")),
             aes(x=lat,y=intensity,colour=Region),
             shape=19,
             size=2,
             alpha=0.5)+
  xlab ("") + 
  ylab ("Maximum DHW value in ºC-weeks")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  
  scale_fill_grey(start = 0.1,end=0.1) +
  scale_y_continuous(breaks = seq(0,32, 4) ) + 
  scale_x_continuous(breaks = seq(-30,10, 5) ) + 
  #scale_y_break(c(2500, 7500), scales = 0.5)+
  ggtitle ("a")+
  theme(text = element_text (family = "serif"),
        legend.position = "none")


# duration -----------------------------------------
# conditional effects
ce <- conditional_effects(model_count2_lat)
p2 <-ce$lat %>%
  ggplot(aes(x=lat,y=estimate__))+
  theme_classic()+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_point(data = DHW_data %>% 
               filter (duration >0) %>%
               mutate (region,Region = recode(region, ILOC = "Oceanic islands")),
             aes(x=lat,y=duration,colour=Region),
             shape=19,
             size=2,
             alpha=0.5)+
  xlab ("Latitude") + 
  ylab ("Duration (Number of days\nunder thermal stress)")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  
  scale_fill_grey(start = 0.1,end=0.1) +
  scale_x_continuous(breaks = seq(-30,10, 5) ) + 
  #scale_y_break(c(2500, 7500), scales = 0.5)+
  ggtitle ("b")+
  theme(text = element_text (family = "serif"),
        legend.position = c(0.25,0.8))

# interval -----------------------------------------
# conditional effects
ce <- conditional_effects(model_count_lat)
p3 <-ce$lat %>%
  ggplot(aes(x=lat,y=estimate__))+
  theme_classic()+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_point(data = DHW_data  %>% 
               filter (interval >0) %>% 
               mutate (region,Region = recode(region, ILOC = "Oceanic islands")),
             aes(x=lat,y=interval,colour=Region),
             shape=19,
             size=2,
             alpha=0.5)+
  xlab ("") + 
  ylab ("Frequency (Number of days between\nthermal stress episodes)")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  
  scale_fill_grey(start = 0.1,end=0.1) +
  scale_x_continuous(breaks = seq(-30,10, 5) ) + 
  #scale_y_break(c(2500, 7500), scales = 0.5)+
  ggtitle ("c")+
  theme(text = element_text (family = "serif"),
        legend.position = "none")


# save plot
ggsave (here ("output", "trend-lat.png"),
        grid.arrange(plot(p1),(p2),(p3),ncol=3),
        width = 12,height = 5)

# pdf
# save plot
ggsave (here ("output", "trend-lat.pdf"),
        grid.arrange(plot(p1),(p2),(p3),ncol=3),
        width = 12,height = 5)


# Posterior exceedance probabilities --------------------------------
PEP <- rbind(interval = table(as_draws_array(model_count_lat,variable = "b_scalelat")>0)/prod(dim(as_draws_array(model_count_lat,variable = "b_scalelat"))),
             duration = table(as_draws_array(model_count2_lat,variable = "b_scalelat")>0)/prod(dim(as_draws_array(model_count2_lat,variable = "b_scalelat"))),
             intensity = table(as_draws_array(model_count3_lat,variable = "b_scalelat")>0)/prod(dim(as_draws_array(model_count3_lat,variable = "b_scalelat"))))

# posterior summary
tab_coef_lat <- rbind(
  data.frame(posterior_summary(model_count_lat)[1:6,],model="Interval"),
  data.frame (posterior_summary(model_count2_lat)[1:6,],model="Duration"),
  data.frame(posterior_summary(model_count3_lat)[1:7,],model="Intensity")
  ) 

# kable
#(tab_coef_lat) %>%
#  knitr::kable(align="c",
#               digits =3,
#               col.names = c("Estimate", 
#                             "Est. Error",
#                             "Lower CI", 
#                             "Upper CI", "Model")) 
#
#
# end

