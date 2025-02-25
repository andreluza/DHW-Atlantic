

# -----------------------------------------------------------

# R Code used in the manuscript by Destri et al. (submitted to Global Change Biology)


# with this code we wanted to know (using GLMMs) whether the intensity, duration and frequency of DHWs are increasing with latitude (i) and time (ii).
# Also, we did the analysis for all data set and for data from each individual region (Interaction between year and region ID). 

# Analyses done by AL Luza, with many helps from:
# https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
# https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/

rm(list=ls())

# load packages 
source("RScript/packages.R")

# create dir to receive results
dir.create ("RData")

# ----------------------------------------
# load DHW data

DHW_data <- read.csv (here("data", "DHW_raw_stats_table.csv"),sep=";")


# replace NAs by zeros (cases where no anomaly was observed)
DHW_data$intensity[is.na(DHW_data$intensity)] <- 0
DHW_data$duration[is.na(DHW_data$duration)] <- 0
DHW_data$interval[is.na(DHW_data$interval)] <- 0

# random factor for overdispersion
DHW_data$nID <- seq (1,nrow(DHW_data))

# check the distribution of values - tons of zeroes
png(here ("output", "Fig-S1.png"),width = 16,height = 7,units = "cm",res=300)
par(mfrow=c(1,3),mar=c(4,4,4,4))
  hist(DHW_data$intensity[which(DHW_data$intensity>0)],main="a",xlab= "ºC-weeks",ylim=c(0,120))
  hist(DHW_data$duration[which(DHW_data$duration>0)],main="b",xlab= "Number of days",ylim=c(0,120),ylab="")
  hist(DHW_data$interval[which(DHW_data$interval>0)],main="c",xlab= "Number of days",ylim=c(0,120),ylab="")
dev.off()

# also in pdf
pdf(here ("output", "Figu-S1.pdf"),width = 6,height = 4)
par(mfrow=c(1,3),mar=c(4,4,4,4))
hist(DHW_data$intensity[which(DHW_data$intensity>0)],main="a",xlab= "ºC-weeks",ylim=c(0,120))
hist(DHW_data$duration[which(DHW_data$duration>0)],main="b",xlab= "Number of days",ylim=c(0,120),ylab="")
hist(DHW_data$interval[which(DHW_data$interval>0)],main="c",xlab= "Number of days",ylim=c(0,120),ylab="")
dev.off()

# plot exploration
# over years
DHW_data %>%
  filter (intensity >0) %>%
  ggplot() +
  geom_point(aes(x=year, y=intensity))+
  geom_smooth(aes(x=year, y=intensity)) + 
  facet_wrap(~region,ncol=3)

# interval/frequency
DHW_data %>%
  filter (interval >0) %>%
  ggplot() +
  geom_point(aes(x=year, y=interval))+
  geom_smooth(aes(x=year, y=interval), method = "glm", methods.arg = list(family="poisson")) + 
  facet_wrap(~region,ncol=3)


# intensity vs duration
DHW_data %>%
  filter (duration >0) %>%
  mutate (region,Region = recode(region, ILOC = "Oceanic islands")) %>%
  mutate (int = cut_interval(year, 6)) %>%
  ggplot() +
  geom_point(aes(x=intensity, y=duration,colour=Region))+
  geom_smooth(aes(x=intensity, y=duration),
              method = "glm", se=T,formula = "y ~ x" ,
              method.args = list(family = "poisson")) + 
  facet_wrap(~int,scales="fixed",ncol=6)+
  theme_classic()+
  scale_color_manual(values = c("Oceanic islands" = "green",
                                "North" = "blue",
                                "Northeast" = "pink",
                                "East" = "yellow",
                                "South" = "orange",
                                "Africa" = "red")) +
  theme(legend.position = c(0.10,0.75))+
  xlab ("Intensity (Maximum DHW value in ºC-weeks)")+
  ylab ("Duration (number of days under thermal stress)")+
  theme(text = element_text (family = "serif"))
  
ggsave(here("output","Fig-3.png"),
       width = 10,height = 5
       )

# a lot of zeroes
sum(DHW_data$intensity == 0)/length(DHW_data$intensity)
sum(DHW_data$duration == 0)/length(DHW_data$duration)
sum(DHW_data$interval == 0)/length(DHW_data$interval)


# check overdispersion
(check_overdispersion(glm(intensity ~ lat, family = Gamma, data = DHW_data %>% filter (intensity >0)))) # no overdispersion here
check_overdispersion(glm(duration ~ lat, family = poisson, data = DHW_data %>% filter (duration >0)))
check_overdispersion(glm(interval ~ lat, family = poisson, data = DHW_data %>% filter(interval>0)))

# number of observations
nrow(DHW_data %>% filter (intensity>0))+nrow(DHW_data %>% filter (duration>0))+nrow(DHW_data %>% filter (interval>0))

# i) Models evaluating latitude ----------------------

# an alternative could be 
#model_nb <- brm(bf(interval ~ scale(lat)), family = negbinomial(), data = #DHW_data %>%
#    filter (interval > 0))
#bayes_R2(model_nb,re.formula=NA)
#pp_check(model_nb)
#pp_check(model_count_lat)


# interval ~ latitude
model_count_lat <- brm(
  bf(interval ~ scale(lat) + (1|nID)),
  family = poisson(link = "log"), 
  data = DHW_data %>%
    filter (interval > 0),
  chains = 3, cores = 3,
  iter = 20000,
  warmup = 18000,
  thin=1,
  seed=1234,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15)
)
pp_check(model_count_lat)
save(model_count_lat,file=here ("RData","model_count_lat.RData"))

# duration ~ latitude
model_count2_lat <- brm(
  bf(duration ~ scale(lat) + (1|nID)),
  family = poisson(link = "log"), 
  data = DHW_data %>%
    filter (duration > 0),
  chains = 3, cores = 3,
  iter = 20000,
  warmup = 18000,
  thin=1,
  seed=1234,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15)
)
pp_check(model_count2_lat)
conditional_effects(model_count2_lat)
save(model_count2_lat,file=here ("RData","model_count2_lat.RData"))

# intensity ~ latitude
model_count3_lat <- brm(
  bf(intensity ~ scale(lat) ), # no random effect here as no overdispersion was detected
  family = Gamma(link = "log"), # ou zero_inflated_negbinomial(link = "log")
  data = DHW_data%>%
    filter (intensity > 0),
  chains = 3, cores = 3,
  iter = 20000,
  warmup = 18000,
  thin=1,
  seed=1234,
  control = list(adapt_delta = 0.99)
)
pp_check(model_count3_lat,ndraws = 50)
save(model_count3_lat,file=here ("RData","model_count3_lat.RData"))

# ----------------------------------------------------------

# ii) Models evaluating region and time


# check overdispersion
(check_overdispersion(glm(intensity ~  scale(year) * region, family = Gamma, data = DHW_data %>% filter (intensity >0)))) # no overdispersion here
check_overdispersion(glm(duration ~  scale(year) * region, family = poisson, data = DHW_data %>% filter (duration >0)))
check_overdispersion(glm(interval ~  scale(year) * region, family = poisson, data = DHW_data %>% filter(interval>0)))

# Interaction between year and region ID
# interval ~ year * region
model_count <- brm(
  bf(interval ~ scale(year) * region + (1|nID)),
  family = poisson(link = "log"), # ou zero_inflated_negbinomial(link = "log")
  data = DHW_data %>%
    filter (interval >0),
  chains = 3, cores = 3,
  iter = 20000,
  warmup = 18000,
  thin=1,
  seed=1234,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15)
)
pp_check(model_count)
save(model_count,file=here ("RData","model_count.RData"))


# Ajuste para variável de contagem 2 (com zero-inflation, se necessário)
model_count2 <- brm(
  bf(duration ~ scale(year) * region + (1|nID)),
  family = poisson(link = "log"), # ou zero_inflated_negbinomial(link = "log")
  data = DHW_data %>%
    filter (duration >0),
  chains = 3, cores = 3,
  iter = 20000,
  warmup = 18000,
  thin=1,
  seed=1234,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15)
)
pp_check(model_count2)
save(model_count2,file=here ("RData","model_count2.RData"))

# Ajuste para variável de contagem 2 (com zero-inflation, se necessário)
model_count3 <- brm(
  bf(intensity ~ scale(year) * region ),
  family = Gamma(link = "log"), # ou zero_inflated_negbinomial(link = "log")
  data = DHW_data %>%
    filter (intensity >0),
  chains = 3, cores = 3,
  iter = 20000,
  warmup = 18000,
  thin=1,
  seed=1234,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 15)
)
pp_check(model_count3,ndraws = 50)
save(model_count3,file=here ("RData","model_count3.RData"))

#end