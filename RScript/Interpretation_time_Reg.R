# -------------------------------------------

# Model interpretation - ii) region and time effect

# -------------------------------------------
rm(list=ls())

# load packages 
source("RScript/packages.R")

# load
DHW_data <- read.csv (here("data", "DHW_raw_stats_table.csv"),sep=";")

# replace NAs by zeros (cases where no anomaly was observed)
DHW_data$intensity[is.na(DHW_data$intensity)] <- 0
DHW_data$duration[is.na(DHW_data$duration)] <- 0
DHW_data$interval[is.na(DHW_data$interval)] <- 0


# load estimated models
load("RData/model_count.RData")
load("RData/model_count2.RData")
load("RData/model_count3.RData")

# calculate the Bayes R^2
bayes_R2(model_count3) # conditional + marginal
bayes_R2(model_count2)  # conditional + marginal
bayes_R2(model_count2,re_formula=NA) # marginal
bayes_R2(model_count)  # conditional + marginal
bayes_R2(model_count,re_formula=NA) # marginal

# obtain the marginal effects
require(emmeans)

emmeans(model_count,~ region*year,mode = "count", lin.pred = F,  type = "response" ) # interval
emmeans(model_count2,~ region*year, mode = "count", lin.pred = F,  type = "response") # duration
emmeans(model_count3,~ region*year,mode = "count", lin.pred = F,  type = "response") # intensity

# Extrair coeficientes fixos (na escala log)
fixed_effects_interval <- fixef(model_count)
# Transformar para a escala original
round(exp(fixed_effects_interval),4)

# Extrair coeficientes fixos (na escala log)
fixed_effects_duration <- fixef(model_count2)
# Transformar para a escala original
round(exp(fixed_effects_duration),4)

# Extrair coeficientes fixos (na escala log)
fixed_effects_intensity <- fixef(model_count3)
# Transformar para a escala original
round(exp(fixed_effects_intensity),4)

# standard deviation on year
mean(DHW_data  %>% 
  filter (intensity >0) %>% select(year)%>%unlist())
sd(DHW_data  %>% 
       filter (intensity >0) %>% select(year)%>%unlist())

# duration
# standard deviation on year
mean(DHW_data  %>% 
       filter (duration >0) %>% select(year)%>%unlist())
sd(DHW_data  %>% 
     filter (duration >0) %>% select(year)%>%unlist())

# frequency
# standard deviation on year
mean(DHW_data  %>% 
       filter (interval >0) %>% select(year)%>%unlist())
sd(DHW_data  %>% 
     filter (interval >0) %>% select(year)%>%unlist())

# posterior predictive checks -----------------------------------------

require(gridExtra)
require(ggbreak)
png(here("output", "pp-check_reg.png"),width=15,height=15,unit="cm", res=300)
grid.arrange(pp_check(model_count3,ndraws = 50)+ylab("Frequency")+ggtitle("a")+theme(legend.position = c(0.8,0.8)),
             pp_check(model_count2,ndraws = 50)+ylab("Frequency")+ggtitle("b")+theme(legend.position = "none"),
             pp_check(model_count,ndraws = 50)+ylab("Frequency")+ggtitle("c")+theme(legend.position = "none"),
             nrow=3)
             
dev.off()


# intensity -----------------------------------------
ce <- conditional_effects(model_count3,effects = NULL)
p1 <- ce$`year:region` %>%
  mutate (Region = region) %>%
  mutate (Region, Region = factor(Region, levels = c("North","Northeast","East","South",
                                                     "ILOC","Africa"
  ))) %>%
  mutate (Region,Region = recode(Region, ILOC = "Oceanic islands")) %>%
  ggplot(aes(x=year,y=estimate__, fill=Region,col=Region))+
  geom_point(data = DHW_data  %>% 
               filter (intensity >0) %>% 
               mutate (region,region = recode(region, ILOC = "Oceanic islands")),
             aes(x=year,y=intensity,fill=region,col=region),
             shape=19,
             size=2,
             alpha=0.5)+
  theme_classic()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_line(stat="identity")+
  xlab ("Time") + 
  ylab ("Maximum DHW value in ºC-weeks")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  scale_fill_grey(start = 0.7,end=0.7) +
  #scale_y_break(c(50, 100), scales = 0.5)
  ggtitle ("a") +
  theme(legend.position = "none",plot.title=element_text(size=22),
        axis.text.x = element_text(angle=90),axis.title.x = element_blank()) +
  scale_y_continuous(breaks = seq(0,32, 4) ) + 
  scale_x_continuous(breaks = seq(1985,2024, 3) ) +
  theme(text = element_text (family = "serif"))

# duration -----------------------------------------
# conditional effects
ce <- conditional_effects(model_count2,effects = NULL )
p2 <-ce$`year:region` %>%
  mutate (Region = region) %>%
  mutate (Region, Region = factor(Region, levels = c("North","Northeast","East","South",
                                                     "ILOC","Africa"
  ))) %>%
  mutate (Region,Region = recode(Region, ILOC = "Oceanic islands")) %>%
  ggplot(aes(x=year,y=estimate__, fill=Region,col=Region))+
  geom_point(data = DHW_data  %>%
               filter (duration >0) %>% 
               mutate (region,region = recode(region, ILOC = "Oceanic islands")),
             aes(x=year,y=duration,fill=region,col=region),
             shape=19,
             size=2,
             alpha=0.5)+
  theme_classic()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_line(stat="identity")+
  xlab ("Time") + 
  ylab ("Duration (number of days under\nthermal stress)")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  scale_fill_grey(start = 0.7,end=0.7) +
  theme(legend.position = c(0.3,0.8),plot.title=element_text(size=22),
        axis.text.x = element_text(angle=90),axis.title.x = element_blank()) +
  #scale_y_break(c(2500, 7500), scales = 0.5)+
  ggtitle ("b")+ 
  scale_x_continuous(breaks = seq(1985,2024, 3) ) +
  theme(text = element_text (family = "serif"))

  
# interval -----------------------------------------
# conditional effects
ce <- conditional_effects(model_count,effects = NULL)
p3 <- ce$`year:region` %>%
  mutate (Region = region) %>%
  mutate (Region, Region = factor(Region, levels = c("North","Northeast","East","South",
                                                     "ILOC","Africa"
  ))) %>%
  mutate (Region,Region = recode(Region, ILOC = "Oceanic islands"))  %>%
  filter (upper__ < 30000) %>%

  #mutate (upper__ = ifelse (Region == "Oceanic islands",0,upper__)) %>%
  ggplot(aes(x=year,y=estimate__, fill=Region,col=Region))+
  
  theme_classic()+
  geom_point(data = DHW_data %>% 
               filter (interval >0) %>% 
               mutate (region,region = recode(region, ILOC = "Oceanic islands"))
             ,aes(x=year,y=interval,fill=region,col=region),
             shape=19,
             size=2,
             alpha=0.5)+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_line(stat="identity")+
  xlab ("Time") + 
  ylab ("Frequency (number of days \nbetween thermal stress episodes)")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  scale_fill_grey(start = 0.7,end=0.7) +
  ggtitle ("c")+
  theme(legend.position = "none",plot.title=element_text(size=22),
        axis.text.x = element_text(angle=90),axis.title.x = element_blank()) +
  scale_x_continuous(breaks = seq(1985,2024, 3) ) +
  #scale_y_break(c(5000, 10000), scales = 0.1)+ 
  theme(text = element_text (family = "serif"))

p3

# organize the plots
pdf(here ("output", "Figu-2.pdf"),width=12,heigh=5)
grid.arrange((p1),p2,p3,
             ncol=3
             )

dev.off()

# png

# organize the plots
png(here ("output", "Figu-2.png"),width=25,height = 13,units = "cm",res=300)
grid.arrange((p1),p2,p3,
             ncol=3
             )

dev.off()


# breaking the figures per region

# intensity -----------------------------------------
ce <- conditional_effects(model_count3,effects = NULL)
p1reg <- ce$`year:region` %>%
  mutate (Region = region) %>%
  mutate (Region, Region = factor(Region, levels = c("North","Northeast","East","South",
                                                     "ILOC","Africa"
  ))) %>%
  mutate (Region,Region = recode(Region, ILOC = "Oceanic islands")) %>%
  ggplot(aes(x=year,y=estimate__, fill=Region,col=Region))+
  geom_point(data = DHW_data %>% 
               filter (intensity>0) %>%
               mutate (region,Region = recode(region, ILOC = "Oceanic islands")),
             aes(x=year,y=intensity,fill=Region,col=Region),
             shape=19,
             size=2,
             alpha=0.5)+
  theme_classic()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_line(stat="identity")+
  xlab ("Time") + 
  ylab ("Maximum DHW value in Celsius degrees-weeks")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  scale_fill_grey(start = 0.7,end=0.7) +
  #scale_y_break(c(50, 100), scales = 0.5)
  ggtitle ("a") +
  theme(legend.position = "none",plot.title=element_text(size=22),
        axis.text.x = element_text(angle=90),axis.title.x = element_blank()) +
  scale_y_continuous(breaks = seq(0,32, 4) ) + 
  scale_x_continuous(breaks = seq(1985,2024, 3) ) +
  theme(text = element_text (family = "serif")) + 
  facet_wrap(~Region,nrow=1)

p1reg

# duration -----------------------------------------
# conditional effects
ce <- conditional_effects(model_count2,effects = NULL )
p2reg <-ce$`year:region` %>%
  mutate (Region = region) %>%
  mutate (Region, Region = factor(Region, levels = c("North","Northeast","East","South",
                                                     "ILOC","Africa"
  ))) %>%
  mutate (Region,Region = recode(Region, ILOC = "Oceanic islands")) %>%
  ggplot(aes(x=year,y=estimate__, fill=Region,col=Region))+
  geom_point(data = DHW_data  %>% 
               filter (duration>0) %>%
               mutate (region,Region = recode(region, ILOC = "Oceanic islands")),
             aes(x=year,y=duration,fill=Region,col=Region),
             shape=19,
             size=2,
             alpha=0.5)+
  theme_classic()+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  xlab ("Time") + 
  ylab ("Duration (number of days under\nthermal stress)")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  scale_fill_grey(start = 0.7,end=0.7) +
  theme(legend.position = c(0.3,0.8),plot.title=element_text(size=22),
        axis.text.x = element_text(angle=90),axis.title.x = element_blank()) +
  #scale_y_break(c(2500, 7500), scales = 0.5)+
  ggtitle ("b")+ 
  scale_x_continuous(breaks = seq(1985,2024, 3) ) +
  theme(text = element_text (family = "serif"))+ 
  facet_wrap(~Region,nrow=1)

p2reg


# interval -----------------------------------------
# conditional effects
ce <- conditional_effects(model_count)
p3reg <- ce$`year:region` %>%
  mutate (Region = region) %>%
  mutate (Region, Region = factor(Region, levels = c("North","Northeast","East","South",
                                                     "ILOC","Africa"
  ))) %>%
  mutate (Region,Region = recode(region, ILOC = "Oceanic islands")) %>%
  #mutate (upper__ = ifelse (Region == "Oceanic islands",0,upper__)) %>%
  filter (upper__ < 30000) %>%
  
  ggplot(aes(x=year,y=estimate__, fill=Region,col=Region))+
  
  theme_classic()+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = lower__, ymax = upper__),colour=NA,alpha=0.2)+
  geom_point(data = DHW_data %>% 
               filter (interval>0) %>%
               mutate (region,Region = recode(region, ILOC = "Oceanic islands"))
             ,aes(x=year,y=interval,fill=Region,col=Region),
             shape=19,
             size=2,
             alpha=0.5)+
  xlab ("Time") + 
  ylab ("Frequency (number of days \nbetween thermal stress episodes)")+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  scale_fill_grey(start = 0.7,end=0.7) +
  ggtitle ("c")+
  theme(legend.position = "none",plot.title=element_text(size=22),
        axis.text.x = element_text(angle=90),axis.title.x = element_blank()) +
  scale_x_continuous(breaks = seq(1985,2024, 3) ) +
  theme(text = element_text (family = "serif"))+ 
  facet_wrap(~Region,nrow=1)

p3reg

# organize the plots
pdf(here ("output", "Figu-S-reg.pdf"),width=10,heigh=8)
grid.arrange((p1reg),p2reg+theme(legend.position = "none"),print(p3reg),
             nrow=3)

dev.off()


# organize the plots
png(here ("output", "Figu-S-reg.png"),width=25,heigh=20,units = "cm",res=300)
grid.arrange((p1reg),p2reg+theme(legend.position = "none"),print(p3reg),
             nrow=3)

dev.off()

# posterior exceedance
draws_interval <- as_draws_df(model_count)
table(draws_interval[,2]>0)/sum(table(draws_interval[,2]>0))
table((draws_interval[,8] - draws_interval[,2])>0)/sum(table((draws_interval[,3] - draws_interval[,2])>0))
table((draws_interval[,9] - draws_interval[,2])>0)/sum(table((draws_interval[,4] - draws_interval[,2])>0))
table((draws_interval[,10] - draws_interval[,2])>0)/sum(table((draws_interval[,5] - draws_interval[,2])>0))
table((draws_interval[,11] - draws_interval[,2])>0)/sum(table((draws_interval[,6] - draws_interval[,2])>0))
table((draws_interval[,12] - draws_interval[,2])>0)/sum(table((draws_interval[,7] - draws_interval[,2])>0))

# posterior exceedance
draws_duration <- as_draws_df(model_count2)
table(draws_duration[,2]>0)/sum(table(draws_duration[,2]>0))
table((draws_duration[,8])>0)/sum(table((draws_duration[,3] - draws_duration[,2])>0))
table((draws_duration[,9])>0)/sum(table((draws_duration[,4] - draws_duration[,2])>0))
table((draws_duration[,10])>0)/sum(table((draws_duration[,5] - draws_duration[,2])>0))
table((draws_duration[,11])>0)/sum(table((draws_duration[,6] - draws_duration[,2])>0))
table((draws_duration[,12])>0)/sum(table((draws_duration[,7] - draws_duration[,2])>0))

conditional_effects(model_count2)
# posterior exceedance
draws_intensity <- as_draws_df(model_count3)
table(draws_intensity[,2]>0)/sum(table(draws_intensity[,2]>0))
table((draws_intensity[,8] - draws_intensity[,2])>0)/sum(table((draws_intensity[,3] - draws_intensity[,2])>0))
table((draws_intensity[,9] - draws_intensity[,2])>0)/sum(table((draws_intensity[,4] - draws_intensity[,2])>0))
table((draws_intensity[,10] - draws_intensity[,2])>0)/sum(table((draws_intensity[,5] - draws_intensity[,2])>0))
table((draws_intensity[,11] - draws_intensity[,2])>0)/sum(table((draws_intensity[,6] - draws_intensity[,2])>0))
table((draws_intensity[,12] - draws_intensity[,2])>0)/sum(table((draws_intensity[,7] - draws_intensity[,2])>0))

# end
