### chapter 2 ### 
## dibang community data ##

library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("A:/dibang_community")
metadata<- read.csv("metadata_dibang_v2.csv")
metadata<- metadata[!is.na(metadata$scientific_name), ]

metadata_matrix<- metadata %>%
  group_by(plotcode, scientific_name) %>%
  summarise(n=n())

metadata_matrix_wide<- metadata_matrix %>%
  pivot_wider(names_from = plotcode, values_from = n, values_fill=0)

## how many species are there in total ###
metadata_names = subset(metadata, select=c(lulc, scientific_name))
unique(metadata_names$scientific_name) #119 species in total

###### species richness (alpha diversity) ####
library(iNEXT)
####### pre-winter######

create_birds_matrix <- function(data) {
  birds_pre_w_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_pre_w_matrix_wide <- birds_pre_w_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_pre_w_matrix_wide <- ifelse(birds_pre_w_matrix_wide >= 1, 1, 0)
  
  return(birds_pre_w_matrix_wide)
}

# Usage
birds_pre_w_for_matrix <- create_birds_matrix(metadata %>% filter(season=="pre-winter", lulc== "forest"))
birds_pre_w_bam_matrix <- create_birds_matrix(metadata %>% filter(season=="pre-winter", lulc== "bamboo"))
birds_pre_w_jhu_matrix <- create_birds_matrix(metadata %>% filter(season=="pre-winter", lulc== "jhum"))
birds_pre_w_abn_matrix <- create_birds_matrix(metadata %>% filter(season=="pre-winter", lulc== "abandoned"))


### inext analysis ###
birds_pre_w_list<- list(forest=birds_pre_w_for_matrix, bamboo=birds_pre_w_bam_matrix, jhum=birds_pre_w_jhu_matrix, abandoned=birds_pre_w_abn_matrix)
rich_pre_w<- iNEXT(birds_pre_w_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=100)
plot(rich_pre_w, type=1, cex.axis = 1.5) # sample size-based R/E curve
plot(rich_pre_w, type=2, cex.axis = 1.5) # completeness curve
plot(rich_pre_w, type=3, cex.axis = 1.5) # coverage-based rarefaction/extrapolation curve

#rarefied_richness_coverage_pre<- rich_pre_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
#lapply(rich_pre_w, function(x) write.table( data.frame(x), 'richness_rarefied_pre-winter.csv'  , append= T, sep=',' )) # to export

####### post-winter######

create_birds_matrix <- function(data) {
  birds_post_w_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_post_w_matrix_wide <- birds_post_w_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_post_w_matrix_wide <- ifelse(birds_post_w_matrix_wide >= 1, 1, 0)
  
  return(birds_post_w_matrix_wide)
}

# Usage
birds_post_w_for_matrix <- create_birds_matrix(metadata %>% filter(season=="post-winter", lulc== "forest"))
birds_post_w_bam_matrix <- create_birds_matrix(metadata %>% filter(season=="post-winter", lulc== "bamboo"))
birds_post_w_jhu_matrix <- create_birds_matrix(metadata %>% filter(season=="post-winter", lulc== "jhum"))
birds_post_w_abn_matrix <- create_birds_matrix(metadata %>% filter(season=="post-winter", lulc== "abandoned"))

### inext analysis ###
birds_post_w_list<- list(forest=birds_post_w_for_matrix, bamboo=birds_post_w_bam_matrix, jhum=birds_post_w_jhu_matrix, abandoned=birds_post_w_abn_matrix)
rich_post_w<- iNEXT(birds_post_w_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=100)
plot(rich_post_w, type=1, cex.axis = 1.5) # sample size-based R/E curve
plot(rich_post_w, type=2, cex.axis = 1.5) # completeness curve
plot(rich_post_w, type=3, cex.axis = 1.5) # coverage-based rarefaction/extrapolation curve

#rarefied_richness_coverage_post<- rich_post_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
#lapply(rich_post_w, function(x) write.table( data.frame(x), 'richness_rarefied_post-winter.csv'  , append= T, sep=',' )) # to export

##### richness for two years combined ######
create_birds_matrix <- function(data) {
  birds_w_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_w_matrix_wide <- birds_w_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_w_matrix_wide <- ifelse(birds_w_matrix_wide >= 1, 1, 0)
  
  return(birds_w_matrix_wide)
}

# Usage
birds_w_for_matrix <- create_birds_matrix(metadata %>% filter(lulc== "forest"))
birds_w_bam_matrix <- create_birds_matrix(metadata %>% filter(lulc== "bamboo"))
birds_w_jhu_matrix <- create_birds_matrix(metadata %>% filter(lulc== "jhum"))
birds_w_abn_matrix <- create_birds_matrix(metadata %>% filter(lulc== "abandoned"))

### inext analysis ###
birds_w_list<- list(forest=birds_w_for_matrix, bamboo=birds_w_bam_matrix, jhum=birds_w_jhu_matrix, abandoned=birds_w_abn_matrix)
rich_w<- iNEXT(birds_w_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=100)
plot(rich_w, type=1, cex.axis = 1.5) # sample size-based R/E curve
plot(rich_w, type=2, cex.axis = 1.5) # completeness curve
plot(rich_w, type=3, cex.axis = 1.5) # coverage-based rarefaction/extrapolation curve


### phase 1 richness#####
create_birds_matrix <- function(data) {
  birds_1_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_1_matrix_wide <- birds_1_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_1_matrix_wide <- ifelse(birds_1_matrix_wide >= 1, 1, 0)
  
  return(birds_1_matrix_wide)
}

# Usage
birds_1_for_matrix <- create_birds_matrix(metadata %>% filter(phase == "1", lulc== "forest"))
birds_1_bam_matrix <- create_birds_matrix(metadata %>% filter(phase == "1", lulc== "bamboo"))
birds_1_jhu_matrix <- create_birds_matrix(metadata %>% filter(phase == "1", lulc== "jhum"))
birds_1_abn_matrix <- create_birds_matrix(metadata %>% filter(phase == "1", lulc== "abandoned"))

### inext analysis ###
birds_1_list<- list(forest=birds_1_for_matrix, bamboo=birds_1_bam_matrix, jhum=birds_1_jhu_matrix, abandoned=birds_1_abn_matrix)
rich_1_w<- iNEXT(birds_1_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=50)
plot(rich_1_w, type=1, cex.axis = 1.2) # sample size-based R/E curve
plot(rich_1_w, type=2, cex.axis = 1.2) # completeness curve
plot(rich_1_w, type=3, cex.axis = 1.2) # coverage-based rarefaction/extrapolation curve


#rarefied_richness_coverage_1<- rich_1_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
#lapply(rich_1_w, function(x) write.table( data.frame(x), 'richness_rarefied_phase1.csv'  , append= T, sep=',' )) # to export


####### phase 2 richness#####
create_birds_matrix <- function(data) {
  birds_2_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_2_matrix_wide <- birds_2_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_2_matrix_wide <- ifelse(birds_2_matrix_wide >= 1, 1, 0)
  
  return(birds_2_matrix_wide)
}

# Usage
birds_2_for_matrix <- create_birds_matrix(metadata %>% filter(phase == "2", lulc== "forest"))
birds_2_bam_matrix <- create_birds_matrix(metadata %>% filter(phase == "2", lulc== "bamboo"))
birds_2_jhu_matrix <- create_birds_matrix(metadata %>% filter(phase == "2", lulc== "jhum"))
birds_2_abn_matrix <- create_birds_matrix(metadata %>% filter(phase == "2", lulc== "abandoned"))

### inext analysis ###
birds_2_list<- list(forest=birds_2_for_matrix, bamboo=birds_2_bam_matrix, jhum=birds_2_jhu_matrix, abandoned=birds_2_abn_matrix)
rich_2_w<- iNEXT(birds_2_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=50)
plot(rich_2_w, type=1, cex.axis = 1.2) # sample size-based R/E curve
plot(rich_2_w, type=2, cex.axis = 1.2) # completeness curve
plot(rich_2_w, type=3, cex.axis = 1.2) # coverage-based rarefaction/extrapolation curve

rarefied_richness_coverage_2<- rich_2_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
lapply(rich_2_w, function(x) write.table( data.frame(x), 'richness_rarefied_phase2.csv'  , append= T, sep=',' )) # to export

####### phase 3 richness#####
create_birds_matrix <- function(data) {
  birds_3_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_3_matrix_wide <- birds_3_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_3_matrix_wide <- ifelse(birds_3_matrix_wide >= 1, 1, 0)
  
  return(birds_3_matrix_wide)
}

# Usage
birds_3_for_matrix <- create_birds_matrix(metadata %>% filter(phase == "3", lulc== "forest"))
birds_3_bam_matrix <- create_birds_matrix(metadata %>% filter(phase == "3", lulc== "bamboo"))
birds_3_jhu_matrix <- create_birds_matrix(metadata %>% filter(phase == "3", lulc== "jhum"))
birds_3_abn_matrix <- create_birds_matrix(metadata %>% filter(phase == "3", lulc== "abandoned"))

### inext analysis ###
birds_3_list<- list(forest=birds_3_for_matrix, bamboo=birds_3_bam_matrix, jhum=birds_3_jhu_matrix, abandoned=birds_3_abn_matrix)
rich_3_w<- iNEXT(birds_3_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=50)
plot(rich_3_w, type=1, cex.axis = 1.2) # sample size-based R/E curve
plot(rich_3_w, type=2, cex.axis = 1.2) # completeness curve
plot(rich_3_w, type=3, cex.axis = 1.2) # coverage-based rarefaction/extrapolation curve

rarefied_richness_coverage_3<- rich_3_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
lapply(rich_3_w, function(x) write.table( data.frame(x), 'richness_rarefied_phase3.csv'  , append= T, sep=',' )) # to export

####### phase 4 richness#####
create_birds_matrix <- function(data) {
  birds_4_matrix <- data %>%
    group_by(plotcode, scientific_name) %>%
    summarise(n = n())
  
  birds_4_matrix_wide <- birds_4_matrix %>%
    pivot_wider(names_from = plotcode, values_from = n, values_fill = 0) %>%
    column_to_rownames("scientific_name")
  
  # Convert counts to binary (0 or 1)
  birds_4_matrix_wide <- ifelse(birds_4_matrix_wide >= 1, 1, 0)
  
  return(birds_4_matrix_wide)
}

# Usage
birds_4_for_matrix <- create_birds_matrix(metadata %>% filter(phase == "4", lulc== "forest"))
birds_4_bam_matrix <- create_birds_matrix(metadata %>% filter(phase == "4", lulc== "bamboo"))
birds_4_jhu_matrix <- create_birds_matrix(metadata %>% filter(phase == "4", lulc== "jhum"))
birds_4_abn_matrix <- create_birds_matrix(metadata %>% filter(phase == "4", lulc== "abandoned"))

### inext analysis ###
birds_4_list<- list(forest=birds_4_for_matrix, bamboo=birds_4_bam_matrix, jhum=birds_4_jhu_matrix, abandoned=birds_4_abn_matrix)
rich_4_w<- iNEXT(birds_4_list, q=0, datatype = "incidence_raw", size=NULL, endpoint=NULL, se= TRUE, knots=40, conf=0.95, nboot=50)
plot(rich_4_w, type=1, cex.axis = 1.2) # sample size-based R/E curve
plot(rich_4_w, type=2, cex.axis = 1.2) # completeness curve
plot(rich_4_w, type=3, cex.axis = 1.2) # coverage-based rarefaction/extrapolation curve

rarefied_richness_coverage_4<- rich_4_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
lapply(rich_4_w, function(x) write.table( data.frame(x), 'richness_rarefied_phase4.csv'  , append= T, sep=',' )) # to export

##### GLMMS using rarefied richness ####

richness<- read.csv("rarefied_rich_seasons.csv")

library(Matrix)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)

corr1 <- cor.test(richness$mean_temp, richness$mean_rh)
print(corr1) #64%, t = -9.549, p-value < 2.2e-16


richness$lulc<- factor(richness$lulc)
richness$lulc<- relevel(richness$lulc, ref = "forest")
richness$year<- factor(richness$year)
richness$season<- factor(richness$season)
richness$season<- relevel(richness$season, ref = "pre-winter")

### mean temp ##
## null model ## 
mod0<- lmer(coverage_based.qD ~ 1+ (1|site), data = richness)
AIC(mod0)

model1<- lmerTest::lmer(coverage_based.qD ~ mean_temp + season +lulc + (1|site),
                         data=richness)
summary(model1)
AIC(model1)
res1<- residuals(model1)
qqnorm(res1)
qqline(res1)

residuals1 <- simulateResiduals(model1) #null hypothesis is that there is no overdispersion
plot(residuals1)
testDispersion(residuals1) 

model2<- lmerTest::lmer(coverage_based.qD ~ mean_temp+ lulc + (1|site),
                        data=richness)
summary(model2)
AIC(model2)


model3<- lmerTest::lmer(coverage_based.qD ~ mean_temp+ season + (1|site),
                        data=richness)
summary(model3)
AIC(model3)


model_set1 <- list(mod0, model1, model2, model3)
mod.names1<- c('null', 'temp+season+lulc','mean_temp+ lulc', 'mean_temp+season')
aictab(cand.set = model_set1, modnames = mod.names1)

length(model_set1)
length(mod.names1)

### mean RH ##

model1a<- lmerTest::lmer(coverage_based.qD ~ mean_rh + season +lulc + (1|site),
                        data=richness)
summary(model1a)
AIC(model1a)
res1a<- residuals(model1a)
qqnorm(res1a)
qqline(res1a)

residuals1a <- simulateResiduals(model1a) #null hypothesis is that there is no overdispersion
plot(residuals1a)
testDispersion(residuals1a) 


model2a<- lmerTest::lmer(coverage_based.qD ~ mean_rh+ season + (1|site),
                        data=richness)
summary(model2a)
AIC(model2a)

model3a<- lmerTest::lmer(coverage_based.qD ~ mean_rh+ lulc + (1|site),
                        data=richness)
summary(model3a)
AIC(model3a)

model_set2 <- list(mod0, model1a, model2a, model3a)
mod.names2<- c('null', 'rh+season+lulc','mean_rh+ season', 'mean_rh+lulc')
aictab(cand.set = model_set2, modnames = mod.names2)



# model2<- lmerTest::lmer(coverage_based.qD ~ mean_temp *season * lulc + (1|site),
#                         data=richness)
# summary(model2)
# AIC(model2)
# res2<- residuals(model2)
# qqnorm(res2)
# qqline(res2)

# residuals2 <- simulateResiduals(model2) #null hypothesis is that there is no overdispersion
# plot(residuals2)
# testDispersion(residuals2) 




####
model3<- lmerTest::lmer(coverage_based.qD ~ mean_rh *season * lulc + (1|site),
                        data=richness)
summary(model3)
AIC(model3)
res3<- residuals(model3)
qqnorm(res3)
qqline(res3)

residuals3 <- simulateResiduals(model3) #null hypothesis is that there is no overdispersion
plot(residuals3)
testDispersion(residuals3)


model4<- lmerTest::lmer(coverage_based.qD ~ mean_rh + season + lulc + (1|site),
                        data=richness)
summary(model4)
AIC(model4)
res4<- residuals(model4)
qqnorm(res4)
qqline(res4)

residuals4 <- simulateResiduals(model4) #null hypothesis is that there is no overdispersion
plot(residuals4)
testDispersion(residuals4)


# Create the ggplot
richness$lulc<- factor(richness$lulc, levels = c("forest", "bamboo", "jhum", "abandoned"))
ggplot(richness, aes(x = mean_temp, y = coverage_based.qD, color = lulc)) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = NULL), size = 2.0, alpha = 0.2) + 
  labs(x = "Mean Temperature", y = "Species Richness") +
  theme_minimal() +  # Minimal theme
  scale_color_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9"), name = "LULC types") +  
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9"), name = "LULC types") + 
 # facet_wrap(~ season)+
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 14),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold")
  )






# Create the ggplot
ggplot(richness, aes(x = mean_rh, y = coverage_based.qD, color = lulc)) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = NULL), size = 2.0, alpha = 0.2) + 
  labs(x = "Mean Relative humidity", y = "Species Richness") +
  theme_minimal() +  # Minimal theme
  scale_color_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9"), name = "LULC types") +  
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9"), name = "LULC types") + 
  facet_wrap(~ season)+
  scale_x_continuous(limits = c(40, 100)) +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 14),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold")
  )

 

### GLMMS using phylogenetic div ######
install.packages("optimx")
library("optimx")

phylo<- read.csv("results_pd.csv")
phylo$lulc<- factor(phylo$lulc)
phylo$lulc<- relevel(phylo$lulc, ref = "forest")
phylo$season<- factor(phylo$season)
phylo$season<- relevel(phylo$season, ref = "pre-winter")

## null model ##
library(AICcmodavg)

AIC(model_0) #235.5766

model3<- glmmTMB(Average_PD ~ mean_temp + season+ lulc+ (1|year), 
                        data=phylo,
                 family = gaussian(link = "log")) # full model

summary(model3)
AIC(model3) #209.4253
res3<- residuals(model3)
qqnorm(res3)
qqline(res3)

residuals3 <- simulateResiduals(model3) #null hypothesis is that there is no overdispersion
plot(residuals3)
testDispersion(residuals3) 

## simpler models
model_0<- glmmTMB(log(Average_PD) ~ 1+ (1|year), data = phylo)
summary(model_0)


model3a<- glmmTMB(log(Average_PD) ~  mean_temp + (1|year),
                  data=phylo,
                  family = gaussian) # mean temp
summary(model3a)
AIC(model3a)

model3b<- glmmTMB(log(Average_PD) ~ mean_rh + (1|year),
                  data=phylo,
                  family = gaussian) # lulc
summary(model3b)
AIC(model3b)


model3c<- glmmTMB(log(Average_PD) ~ season + (1|year),
                  data=phylo,
                  family = gaussian) # season
summary(model3c)
AIC(model3c)

model3d<- glmmTMB(log(Average_PD) ~ lulc + (1|year),
                  data=phylo,
                  family = gaussian) # lulc
summary(model3d)
AIC(model3d)

model_set1 <- list(model_0, model3a, model3b, model3c, model3d)
mod.names1<- c('null', 'temp', 'rh', 'season', 'lulc')
aictab(cand.set = model_set1, modnames = mod.names1) # model_1 with FHD performs best


# 
# model3a<- glmmTMB(Average_PD ~ mean_temp + (1|year), 
#                   data=phylo,
#                   family = gaussian(link = "log"))
# 
# summary(model3a)
# AIC(model3a) #222.7986
# res3a<- residuals(model3a)
# qqnorm(res3a)
# qqline(res3a)
# 
# 
# model3b<- glmmTMB(Average_PD ~ season +(1|year), 
#                   data=phylo,
#                   family = gaussian(link = "log"))
# 
# summary(model3b)
# AIC(model3b) #237.2795
# res3b<- residuals(model3b)
# qqnorm(res3b)
# qqline(res3b)
# 
# model3c<- glmmTMB(Average_PD ~lulc +(1|year), 
#                   data=phylo,
#                   family = gaussian(link = "log"))
# 
# summary(model3c)
# AIC(model3c) #208.5417
# res3c<- residuals(model3c)
# qqnorm(res3c)
# qqline(res3c)






model4<- glmmTMB(Average_PD~ mean_rh+season +lulc+ (1|year),
                 data=phylo,
                 family = gaussian(link = "log"))
summary(model4)
AIC(model4) #210.7754
res4<- residuals(model4)
qqnorm(res4)
qqline(res4)


residuals4 <- simulateResiduals(model4) #null hypothesis is that there is no overdispersion
plot(residuals4)
testDispersion(residuals4) 


## null model ##

summary(mod_PSR_0)
AIC(mod_PSR_0)

model5<- glmmTMB(Average_PSR ~ mean_temp +  season +lulc + (1|year), # null model
                 data=phylo,
                 family = poisson())

summary(model5)
AIC(model5) #98.51281
res5<- residuals(model5)
qqnorm(res5)
qqline(res5)


mod_PSR_0<- glmmTMB(Average_PSR ~ 1 + (1|year), 
                    data=phylo,
                    family = poisson())


model5a<- glmmTMB(Average_PSR ~  mean_temp + (1|year),
                  data=phylo,
                  family = poisson()) # mean temp
summary(model5a)
AIC(model5a)

model5b<- glmmTMB(Average_PSR ~ mean_rh + (1|year),
                  data=phylo,
                  family = poisson()) # lulc
summary(model5b)
AIC(model5b)


model5c<- glmmTMB(Average_PSR ~ season + (1|year),
                  data=phylo,
                  family = poisson()) # season
summary(model5c)
AIC(model5c)

model5d<- glmmTMB(Average_PSR ~ lulc + (1|year),
                  data=phylo,
                  family = poisson()) # lulc
summary(model5d)
AIC(model5d)


model_set1 <- list(mod_PSR_0, model5a, model5b, model5c, model5d)
mod.names1<- c('null', 'temp', 'rh', 'season', 'lulc')
aictab(cand.set = model_set1, modnames = mod.names1) # model_1 with FHD performs best



model5a<- glmmTMB(Average_PSR ~ mean_temp + (1|year), 
                  data=phylo,
                  family = poisson())

summary(model5a)
res5a<- residuals(model5a)
qqnorm(res5a)
qqline(res5a)


model5b<- glmmTMB(Average_PSR ~ mean_rh + (1|year), 
                  data=phylo,
                  family = poisson())

summary(model5b)
res5b<- residuals(model5b)
qqnorm(res5b)
qqline(res5b)


model5c<- glmmTMB(Average_PSR ~ season+ (1|year), 
                  data=phylo,
                  family = poisson())

summary(model5c)
res5c<- residuals(model5c)
qqnorm(res5c)
qqline(res5c)


model6<- glmmTMB(Average_PSR~ mean_rh+lulc+ season +(1|year),
                 data=phylo,
                 family = poisson())
summary(model6)
AIC(model6)
res6<- residuals(model6)
qqnorm(res6)
qqline(res6)


### GLMMS using FDiv####
install.packages("optimx")
library(optimx)

fun<- read.csv("funspace_fd.csv")
fun$lulc<- factor(fun$lulc)
fun$lulc<- relevel(fun$lulc, ref= "forest")
fun$season<- factor(fun$season)
fun$season<- relevel(fun$season, ref= "pre-winter")

## null model 
mod_frich0<- glmmTMB(FRich1 ~ 1+ (1|year), data= fun)
AIC(mod_frich0) #-3.886536


# model7<- glmmTMB(FRich1 ~ mean_temp+lulc+ season + (1|year), data= fun) # full model
# summary(model7)
# AIC(model7) #-3.795976
# 
# residuals7 <- simulateResiduals(model7) #null hypothesis is that there is no overdispersion
# plot(residuals7)
# testDispersion(residuals7) 
# 
# 
# model7a<- glmmTMB(FRich1 ~ mean_temp + lulc + (1|year), data= fun) # 
# summary(model7a)
# AIC(model7a)
# 
# model7b<- glmmTMB(FRich1 ~ mean_temp + season + (1|year), data= fun) # season + lulc
# summary(model7b)
# AIC(model7b)
# 
# 
# model_set3 <- list(mod_frich0, model7, model7a, model7b)
# mod.names3<- c('null', 'temp+season+lulc','mean_temp+ lulc', 'mean_temp+ season')
# aictab(cand.set = model_set3, modnames = mod.names3)


## simpler models## 
 
model7c1<- glmmTMB(FRich1 ~  mean_temp + (1|year), data= fun) # mean temp
summary(model7c1)
AIC(model7c1)

model7c2<- glmmTMB(FRich1 ~ mean_rh + (1|year), data= fun) # lulc
summary(model7c2)
AIC(model7c2)

model7c3<- glmmTMB(FRich1 ~ season + (1|year), data= fun) # season
summary(model7c3)
AIC(model7c3)

model7c4<- glmmTMB(FRich1 ~ lulc + (1|year), data= fun) # lulc
summary(model7c4)
AIC(model7c4)


model_set1 <- list(mod_frich0, model7c1, model7c2, model7c3, model7c4)
mod.names1<- c('null', 'mean_temp', 'mean_rh', 'season', 'lulc')
aictab(cand.set = model_set1, modnames = mod.names1) # model_1 with FHD performs best


# 
# model8<- glmmTMB(FRich ~ mean_rh+lulc+season + (1|year),
#                  data = fun)
# summary(model8)
# AIC(model8)
# 
# 
# residuals8 <- simulateResiduals(model8) #null hypothesis is that there is no overdispersion
# plot(residuals8)
# testDispersion(residuals8)
# 
# model8a<- glmmTMB(FRich ~ mean_rh + (1|year), data= fun)
# summary(model8a)
# AIC(model8a)

### null model ##
mod_fdiv0<- glmmTMB(log(FDiv1) ~ 1 + (1|year), data= fun)
AIC(mod_fdiv0) #-49.17069

# 
# model9<- glmmTMB(FDiv1 ~ mean_temp+lulc+ season + (1|year), data= fun) # full model
# summary(model9)
# AIC(model9)
# 
# residuals9 <- simulateResiduals(model9) #null hypothesis is that there is no overdispersion
# plot(residuals9)
# testDispersion(residuals9) 


model9a<- glmmTMB(log(FDiv1)  ~  mean_temp + (1|year), data= fun) # mean temp
summary(model9a)
AIC(model9a)

model9b<- glmmTMB(log(FDiv1) ~ mean_rh + (1 | year), data = fun)
summary(model9b)
AIC(model9b)


model9c<- glmmTMB(log(FDiv1)  ~ season + (1|year), data= fun) # season
summary(model9c)
AIC(model9c)

model9d<- glmmTMB(log(FDiv1)  ~ lulc + (1|year), data= fun) # lulc
summary(model9d)
AIC(model9d)

model_set1 <- list(mod_fdiv0, model9a, model9b, model9c, model9d)
mod.names1<- c('null', 'mean_temp', 'mean_rh', 'season', 'lulc')
aictab(cand.set = model_set1, modnames = mod.names1) # model_1 with FHD performs best


### community assemblages ###
### nmds broadly- combined two years ###====
library(vegan)
library(betapart)

metadata$proxycode <- gsub('.{3}$', '', metadata$plotcode)
metadata_dist<- metadata %>%
  group_by(scientific_name, proxycode) %>%
  summarise(n=n())

metadata_dist_wide<- metadata_dist %>%
  pivot_wider(names_from = scientific_name, values_from = n, values_fill=0) %>%
  column_to_rownames("proxycode")

metadata_dist_mat<- vegdist(metadata_dist_wide, method="jaccard") ## calculate distance matrix - dissimilarity ##
metadata_dist_mat1 <- as.matrix(metadata_dist_mat, labels =T) ## create matrix

nmds_metadata_dist<- metaMDS(metadata_dist_mat1, distance= "jaccard", k=2, maxit= 999, trymax=500, wascores= TRUE, autotransform = FALSE)
goodness(nmds_metadata_dist) ### goodness of fit ###
stressplot(nmds_metadata_dist)

nmds_scores_metadata<- as.data.frame(scores(nmds_metadata_dist)) # extract scores of nmds
nmds_scores_metadata$sites<- rownames(nmds_scores_metadata)

plot(nmds_metadata_dist, type= "t") # display empty ordination space
orditorp(nmds_metadata_dist, display = "sites", cex=1)

nmds_scores_metadata <- nmds_scores_metadata %>%
  mutate(grp = ifelse(startsWith(sites, "A"), "abandoned",
                      ifelse(startsWith(sites, "B"), "bamboo",
                             ifelse(startsWith(sites, "F"), "forest",
                                    ifelse(startsWith(sites, "J"), "jhum", NA)))))

grp.a <- nmds_scores_metadata[nmds_scores_metadata$grp == "abandoned", ][chull(nmds_scores_metadata[nmds_scores_metadata$grp == 
                                                                                             "abandoned", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- nmds_scores_metadata[nmds_scores_metadata$grp == "bamboo", ][chull(nmds_scores_metadata[nmds_scores_metadata$grp == 
                                                                                          "bamboo", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.c <- nmds_scores_metadata[nmds_scores_metadata$grp == "forest", ][chull(nmds_scores_metadata[nmds_scores_metadata$grp == 
                                                                                           "forest", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.d <- nmds_scores_metadata[nmds_scores_metadata$grp == "jhum", ][chull(nmds_scores_metadata[nmds_scores_metadata$grp == 
                                                                                        "jhum", c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d)  #combine grp.a and grp.b
hull.data$grp<- factor(hull.data$grp, levels= c("forest", "bamboo", "jhum", "abandoned"))

nmds_scores_metadata$grp<- factor(nmds_scores_metadata$grp, levels= c("forest", "bamboo", "jhum", "abandoned"))

ggplot() + 
  geom_point(data = nmds_scores_metadata, aes(x = NMDS1, y = NMDS2, shape = grp, colour = grp), size = 3, alpha = 0.5) +
  scale_colour_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"), name="Land-use land-cover types")+
  scale_shape_manual(values = c("forest" = 19, "bamboo" = 19, "jhum" = 19, "abandoned" = 19))+
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2, group=grp, fill=grp), alpha=0.20)+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"), name = "Land-use land-cover types") +
  coord_equal() +
  theme_bw()

#geom_circle(data = nmds_scores_metadata %>% 
#             group_by(grp) %>% 
#           summarise(cx = mean(NMDS1), cy = mean(NMDS2), r = sqrt((max(NMDS1) - min(NMDS1))^2 + (max(NMDS2) - min(NMDS2))^2) / 2),
# aes(x0 = cx, y0 = cy, r = r, colour= grp), fill = NA, size = 0.5) +

beta_1 <- adonis2(metadata_dist_mat1 ~ nmds_scores_metadata$grp)
print(beta_1)


# Split dissimilarity matrix into subsets based on group labels
dissimilarity_subsets <- split(metadata_dist_mat1, nmds_scores_metadata$grp)

# Calculate mean dissimilarity within each subset
mean_dissimilarity_within_habitat <- sapply(dissimilarity_subsets, function(x) mean(x))

# Print mean dissimilarity within each habitat
print(mean_dissimilarity_within_habitat)



### nmds seasonally #### 


metadata_season <- metadata %>%
  mutate(proxycode = ifelse(season =="pre-winter", 
                            paste0(proxycode, "_pre"), paste0(proxycode, "_post")))

metadata_season_dist<- metadata_season  %>%
  group_by(scientific_name, proxycode) %>%
  summarise(n=n())

metadata_season_dist_wide<- metadata_season_dist %>%
  pivot_wider(names_from = scientific_name, values_from = n, values_fill=0) %>%
  column_to_rownames("proxycode")

metadata_dist_mat_season<- vegdist(metadata_season_dist_wide, method="jaccard") ## calculate distance matrix - dissimilarity ##
metadata_dist_mat1_season <- as.matrix(metadata_dist_mat_season, labels =T) ## create matrix

nmds_metadata_dist_season<- metaMDS(metadata_dist_mat1_season, distance= "jaccard", k=2, maxit= 999, trymax=500, wascores= TRUE, autotransform = FALSE)
goodness(nmds_metadata_dist_season) ### goodness of fit ###
stressplot(nmds_metadata_dist_season)

plot(nmds_metadata_dist_season, type= "t") # display empty ordination space
orditorp(nmds_metadata_dist_season, display = "sites", cex=1)

nmds_scores_metadata_season<- as.data.frame(scores(nmds_metadata_dist_season)) # extract scores of nmds
nmds_scores_metadata_season$sites<- rownames(nmds_scores_metadata_season)


nmds_scores_metadata_season <- nmds_scores_metadata_season %>%
  mutate(grp = case_when(
    grepl("^A.*pre$", sites) ~ "abandoned (pre-winter)",
    grepl("^A.*post$", sites) ~ "abandoned (post-winter)",
    grepl("^B.*pre$", sites) ~ "bamboo (pre-winter)",
    grepl("^B.*post$", sites) ~ "bamboo (post-winter)",
    grepl("^F.*pre$", sites) ~ "forest (pre-winter)",
    grepl("^F.*post$", sites) ~ "forest (post-winter)",
    grepl("^J.*pre$", sites) ~ "jhum (pre-winter)",
    grepl("^J.*post$", sites) ~ "jhum (post-winter)",
    TRUE ~ NA_character_
  ))

grp.a1 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "abandoned (pre-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                          "abandoned (pre-winter)", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.a2 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "abandoned (post-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                          "abandoned (post-winter)", c("NMDS1", "NMDS2")]), ]
grp.b1 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "bamboo (pre-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                       "bamboo (pre-winter)", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b2 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "bamboo (post-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                       "bamboo (post-winter)", c("NMDS1", "NMDS2")]), ]
grp.c1 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "forest (pre-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                        "forest (pre-winter)", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.c2 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "forest (post-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                        "forest (post-winter)", c("NMDS1", "NMDS2")]), ]
grp.d1 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "jhum (pre-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                     "jhum (pre-winter)", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.d2 <- nmds_scores_metadata_season[nmds_scores_metadata_season$grp == "jhum (post-winter)", ][chull(nmds_scores_metadata_season[nmds_scores_metadata_season$grp == 
                                                                                                                     "jhum (post-winter)", c("NMDS1", "NMDS2")]), ]

hull.data_season <- rbind(grp.a1, grp.a2, grp.b1, grp.b2, grp.c1, grp.c2, grp.d1, grp.d2)  #combine grp.a and grp.b
hull.data_season$grp<- factor(hull.data_season$grp, levels= c("forest (pre-winter)", "forest (post-winter)", "bamboo (pre-winter)", "bamboo (post-winter)",
                                                              "jhum (pre-winter)", "jhum (post-winter)","abandoned (pre-winter)", "abandoned (post-winter)"))

nmds_scores_metadata_season$grp<- factor(nmds_scores_metadata_season$grp, levels= c("forest (pre-winter)", "forest (post-winter)", "bamboo (pre-winter)", "bamboo (post-winter)",
                                                                                    "jhum (pre-winter)", "jhum (post-winter)","abandoned (pre-winter)", "abandoned (post-winter)"))

ggplot() + 
  geom_point(data = nmds_scores_metadata_season, aes(x = NMDS1, y = NMDS2, shape = grp, colour = grp), size = 3, alpha = 0.5) +
  scale_colour_manual(values = c("#E9967A", "#E9967A","#B0E57C","#B0E57C","#87CEEB", "#87CEEB","#B19CD9","#B19CD9"), name="Land-use land-cover types")+
  scale_shape_manual(values = c("forest (pre-winter)" = 19, "forest (post-winter)"=17, "bamboo (pre-winter)"=19, "bamboo (post-winter)"=17,
                                "jhum (pre-winter)"=19, "jhum (post-winter)"=17, "abandoned (pre-winter)" = 19, "abandoned (post-winter)" = 17)) +
  geom_polygon(data=hull.data_season, aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30)+
  scale_fill_manual(values = c("#E9967A", "#E9967A","#B0E57C","#B0E57C","#87CEEB", "#87CEEB","#B19CD9","#B19CD9"), name = "Land-use land-cover types") +
  coord_equal() +
  theme_bw()

beta_2 <- adonis2(metadata_dist_mat1_season ~ nmds_scores_metadata_season$grp) # significantly different R2= 0.40435 P<0.001
print(beta_2)

# Split dissimilarity matrix into subsets based on group labels
dissimilarity_subsets_season <- split(metadata_dist_mat1_season, nmds_scores_metadata_season$grp)

# Calculate mean dissimilarity within each subset
mean_dissimilarity_within_habitat_season <- sapply(dissimilarity_subsets_season, function(x) mean(x))

# Print mean dissimilarity within each habitat
print(mean_dissimilarity_within_habitat_season)






### nmds per year#### 

metadata_yr <- metadata %>%
  mutate(proxycode = ifelse(grepl("[1-4]$", plotcode), 
                             paste0(substr(plotcode,1, nchar(plotcode) -3), "_yr1"), 
                            ifelse(grepl("[5-8]$", plotcode),
                                   paste0(substr(plotcode,1, nchar(plotcode) -3), "_yr2"),
                            plotcode)))

metadata_dist_yr<- metadata_yr %>%
  group_by(scientific_name, proxycode) %>%
  summarise(n=n())

metadata_dist_wide_yr<- metadata_dist_yr %>%
  pivot_wider(names_from = scientific_name, values_from = n, values_fill=0) %>%
  column_to_rownames("proxycode")

metadata_dist_mat_yr<- vegdist(metadata_dist_wide_yr, method="jaccard") ## calculate distance matrix - dissimilarity ##
metadata_dist_mat1_yr <- as.matrix(metadata_dist_mat_yr, labels =T) ## create matrix

nmds_metadata_dist_yr<- metaMDS(metadata_dist_mat1_yr, distance= "jaccard", k=2, maxit= 999, trymax=500, wascores= TRUE, autotransform = FALSE)
goodness(nmds_metadata_dist_yr) ### goodness of fit ###
stressplot(nmds_metadata_dist_yr)

nmds_scores_metadata_yr<- as.data.frame(scores(nmds_metadata_dist_yr)) # extract scores of nmds
nmds_scores_metadata_yr$sites_yr<- rownames(nmds_scores_metadata_yr)

plot(nmds_metadata_dist_yr, type= "t") # display empty ordination space
orditorp(nmds_metadata_dist_yr, display = "sites", cex=1)

nmds_scores_metadata_yr <- nmds_scores_metadata_yr %>%
  mutate(grp = case_when(
    grepl("^A.*yr1$", sites_yr) ~ "Abandoned 2021-2022",
    grepl("^A.*yr2$", sites_yr) ~ "Abandoned 2022-2023",
    grepl("^B.*yr1$", sites_yr) ~ "Bamboo 2021-2022",
    grepl("^B.*yr2$", sites_yr) ~ "Bamboo 2022-2023",
    grepl("^F.*yr1$", sites_yr) ~ "Forests 2021-2022",
    grepl("^F.*yr2$", sites_yr) ~ "Forests 2022-2023",
    grepl("^J.*yr1$", sites_yr) ~ "Jhum 2021-2022",
    grepl("^J.*yr2$", sites_yr) ~ "Jhum 2022-2023",
    TRUE ~ NA_character_
  ))

grp.a1 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Abandoned 2021-2022", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                  "Abandoned 2021-2022", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.a2 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Abandoned 2022-2023", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                                              "Abandoned 2022-2023", c("NMDS1", "NMDS2")]), ]
grp.b1 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Bamboo 2021-2022", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                             "Bamboo 2021-2022", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b2 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Bamboo 2022-2023", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                                                       "Bamboo 2022-2023", c("NMDS1", "NMDS2")]), ]
grp.c1 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Forests 2021-2022", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                                                       "Forests 2021-2022", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.c2 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Forests 2022-2023", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                                                       "Forests 2022-2023", c("NMDS1", "NMDS2")]), ]
grp.d1 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Jhum 2021-2022", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                                                       "Jhum 2021-2022", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.d2 <- nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == "Jhum 2022-2023", ][chull(nmds_scores_metadata_yr[nmds_scores_metadata_yr$grp == 
                                                                                                                       "Jhum 2022-2023", c("NMDS1", "NMDS2")]), ]

hull.data_yr <- rbind(grp.a1, grp.a2, grp.b1, grp.b2, grp.c1, grp.c2, grp.d1, grp.d2)  #combine grp.a and grp.b
hull.data_yr

#colorblind_palette <- viridis(8) can also try 

ggplot() + 
  geom_point(data = nmds_scores_metadata_yr, aes(x = NMDS1, y = NMDS2, shape = grp, colour = grp), size = 3, alpha = 0.5) +
  scale_colour_manual(values = colorblind_palette, name="Land-use land-cover types")+
  scale_shape_manual(values = c("Abandoned 2021-2022" = 19, "Abandoned 2022-2023" = 19, "Bamboo 2021-2022" = 19, "Bamboo 2022-2023"=19,
                                "Forests 2021-2022"= 19, "Forests 2022-2023"=19, "Jhum 2021-2022"=19, "Jhum 2022-2023"=19)) +
  #geom_polygon(data=hull.data_yr,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30)+
  #geom_circle(data = nmds_scores_metadata_yr %>% 
   #             group_by(grp) %>% 
    #            summarise(cx = mean(NMDS1), cy = mean(NMDS2), r = sqrt((max(NMDS1) - min(NMDS1))^2 + (max(NMDS2) - min(NMDS2))^2) / 2),
     #         aes(x0 = cx, y0 = cy, r = r, colour= grp), fill = NA, size = 0.5) +
  scale_fill_manual(values = colorblind_palette, name = "Land-use land-cover types") +
  coord_equal() +
  theme_bw()


beta_2 <- adonis2(metadata_dist_mat1_yr ~ nmds_scores_metadata_yr$grp) # significantly different R2= 0.40435 P<0.001
print(beta_2)




#### traits###====
traits_all<- read.csv("traits_community.csv")
traits_all<- na.omit(traits_all)
traits_all$lulc<- factor(traits_all$lulc, levels=c("forest", "bamboo", "jhum", "abandoned"))
traits_all$season<- factor(traits_all$season, levels=c("pre-winter", "post-winter"))
traits_all$year<- factor(traits_all$year, levels= c("2021-2022", "2022-2023"))
traits_all$mass<- log(traits_all$Mass)

## foraging traits##
traits_all$beak_length<- log(traits_all$Beak.Length_Culmen)
traits_all$beak_width<- log(traits_all$Beak.Width)
traits_all$beak_depth<- log(traits_all$Beak.Depth)

## locomotory traits ##
traits_all$tarsus_length<- log(traits_all$Tarsus.Length)
traits_all$wing_length<- log(traits_all$Wing.Length)
traits_all$tail_length<- log(traits_all$Tail.Length)

library(FD)

### bamboo phase 1 functional diversity ####


library(funspace)
## group (a) on foraging traits
traits_foraging<- subset(traits_all, select=c("beak_length", "beak_width", "beak_depth"))
traits_foraging_pca<- princomp(traits_foraging, score=TRUE, cor=TRUE)
summary(traits_foraging_pca)

PC_standard_deviations <- traits_foraging_pca$sdev
total_variance <- sum(PC_standard_deviations^2)
variance_proportion <- (PC_standard_deviations^2) / total_variance
print(variance_proportion)

library(ggbiplot)
ggbiplot(traits_foraging_pca)
colnames(traits_foraging_pca$scores)<- c("PC1_bill", "PC2_bill", "PC3_bill")

## group (b) on body shape

traits_loco<- subset(traits_all, select=c("tarsus_length", "wing_length", "tail_length"))
traits_loco_pca<- princomp(traits_loco, score=TRUE, cor=TRUE)
summary(traits_loco_pca)

ggbiplot(traits_loco_pca)
colnames(traits_loco_pca$scores)<- c("PC1_loco", "PC2_loco", "PC3_loco")

traits_all<- cbind(traits_all, traits_foraging_pca$scores, traits_loco_pca$scores) # combined traits

## descriptive trait summary ##

mean(forest_traits$Mass)
sd(forest_traits$Mass)
mean(forest_traits$Beak.Depth)
sd(forest_traits$Beak.Depth)
mean(forest_traits$Wing.Length)
sd(forest_traits$Wing.Length)

mean(bamboo_traits$Mass)
sd(bamboo_traits$Mass)
mean(bamboo_traits$Beak.Depth)
sd(bamboo_traits$Beak.Depth)
mean(bamboo_traits$Wing.Length)
sd(bamboo_traits$Wing.Length)

mean(jhum_traits$Mass)
sd(jhum_traits$Mass)
mean(jhum_traits$Beak.Depth)
sd(jhum_traits$Beak.Depth)
mean(jhum_traits$Wing.Length)
sd(jhum_traits$Wing.Length)

mean(abn_traits$Mass)
sd(abn_traits$Mass)
mean(abn_traits$Beak.Depth)
sd(abn_traits$Beak.Depth)
mean(abn_traits$Wing.Length)
sd(abn_traits$Wing.Length)

### functional diversity ### using only trait data and not presence/absence or abundances
### BAMBOO ### ====
## phase 1 pre-winter ###==== using bamboo_1_traits

colnames(traits_all)
# 
# [1] "lulc"               "year"               "season"             "common_name"        "scientific_name"    "family"             "order"             
# [8] "Beak.Length_Culmen" "Beak.Length_Nares"  "Beak.Width"         "Beak.Depth"         "Tarsus.Length"      "Wing.Length"        "Kipps.Distance"    
# [15] "Secondary1"         "Hand.Wing.Index"    "Tail.Length"        "Mass"               "Trophic.Level"      "Trophic.Niche"      "Primary.Lifestyle" 
# [22] "mass"               "beak_length"        "beak_width"         "beak_depth"         "tarsus_length"      "wing_length"        "tail_length"
# 

## functional space ##
bamboo_1<- traits_all %>%
  filter(lulc== "bamboo", year=="2021-2022", season== "pre-winter")
bamboo_1_traits<- subset(bamboo_1, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(bamboo_1_traits)<- bamboo_1_traits$scientific_name
colnames(bamboo_1_traits) <- gsub("_", " ", colnames(bamboo_1_traits))

bamboo_2<- traits_all %>%
  filter(lulc== "bamboo", year=="2021-2022", season== "post-winter")
bamboo_2_traits<- subset(bamboo_2, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(bamboo_2_traits)<- bamboo_2$scientific_name
colnames(bamboo_2_traits) <- gsub("_", " ", colnames(bamboo_2_traits))

bamboo_3<- traits_all %>%
  filter(lulc== "bamboo", year=="2022-2023", season== "pre-winter")
bamboo_3_traits<- subset(bamboo_3, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(bamboo_3_traits)<- bamboo_3_traits$scientific_name
colnames(bamboo_3_traits) <- gsub("_", " ", colnames(bamboo_3_traits))

bamboo_4<- traits_all %>%
  filter(lulc== "bamboo", year=="2022-2023", season== "post-winter")
bamboo_4_traits<- subset(bamboo_4, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(bamboo_4_traits)<- bamboo_4_traits$scientific_name
colnames(bamboo_4_traits) <- gsub("_", " ", colnames(bamboo_4_traits))


bamboo_all<- traits_all %>%
  filter(lulc== "bamboo")
unique_rows_bam <- bamboo_all[!duplicated(bamboo_all$scientific_name), ]
unique_rows_bam_traits<- subset(unique_rows_bam, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_bam_traits)<- unique_rows_bam_traits$scientific_name
colnames(unique_rows_bam_traits) <- gsub("_", " ", colnames(unique_rows_bam_traits))
unique(unique_rows_bam_traits)

####
bam_1_matrix <- model.matrix(~ . - 1, data = bamboo_1_traits) # for continuous and categorical data
bam_1_matrix_dissimilarity<- vegdist(bam_1_matrix, method = "gower") # Calculate distance matrix using Gower method
bam_1_nmds_result <- metaMDS(bam_1_matrix_dissimilarity, k = 2) # dissimilarity matrix for nmds test
#bam_1_nmds_result_scores <- scores(bam_1_nmds_result, display = "sites") # nmds
bam_1_funtest <- funspace(x = bam_1_nmds_result, threshold = 0.95) # calculating funspace
#bam_1_abs<- abs(bam_1_nmds_result_scores[, 1]*bam_1_nmds_result_scores[, 2])+rnorm(nrow(bamboo_1_traits), 0, 1) # absolute values
#bam_1_gam <- funspaceGAM(y=bam_1_abs, funspace=bam_1_funtest) #gam
summary(bam_1_funtest)
plot(bam_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo pre-winter year 1", cex.main= 1.5)
correlation_bam_1_matrix <- cor(bam_1_nmds_result_scores)
correlation_bam_1_matrix

     # axis.title.x = "Mass",
     # axis.title.y = "Beak Depth",
     # axis.title.cex = 1.5,
     # axis.title.line = 2.5,
     # axis.cex = 2.0)


trial1_abs<- abs(trial1_funtest[, 1]*trial1_funtest[, 2])+rnorm(nrow(bamboo_1_traits), 0, 1) # absolute values

bam_1_gam <- funspaceGAM(y=bam_1_abs, funspace=bam_1_funtest)

summary(bam_1_funtest)
plot(bam_1_funtest, quant.plot=TRUE)

funtest <- funspace(x = x, PCs = c(1, 2), threshold = 0.95)
summary(funtest)
plot(funtest)

### using  Principal Coordinates Analysis 
# bam_1_matrix <- model.matrix(~ . - 1, data = bamboo_1_traits)
# bam_1_mat_dis<- vegdist(bam_1_matrix, method = "gower") # Calculate distance matrix using Gower method
# bam_1_scale<- cmdscale(bam_1_mat_dis, k=2) 
# bam_1_funtest <- funspace(x = bam_1_scale, threshold = 0.95)
# bam_1_abs <- abs(bam_1_scale[, 1]*bam_1_scale[, 2])+rnorm(nrow(bam_1_matrix), 0, 1)
# bam_1_gam<- funspaceGAM(y=bam_1_abs, funspace=bam_1_funtest)
# summary(bam_1_funtest)
# plot(bam_1_funtest)
# 
# plot(x=bam_1_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(bamboo_1_traits)
x1<- princomp(bamboo_1_traits)
funtest1 <- funspace(x = x1, PCs = c(1, 2), threshold = 0.95)
summary(funtest1)

my_palette <- brewer.pal(n = 3, name = "Set2")
plot(x=funtest1,
          type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Bamboo phase 1", cex.main = 1.5)

## phase 2 post-winter ###= using bamboo_2_traits

bam_2_matrix <- model.matrix(~ . - 1, data = bamboo_2_traits)
bam_2_matrix_dissimilarity<- vegdist(bam_2_matrix, method = "gower")
bam_2_nmds_result <- metaMDS(bam_2_matrix_dissimilarity, k = 2)
#bam_2_nmds_result_scores <- scores(bam_2_nmds_result, display = "sites")
bam_2_funtest <- funspace(x = bam_2_nmds_result, threshold = 0.95)
# bam_2_abs<- abs(bam_2_nmds_result_scores[, 1]*bam_2_nmds_result_scores[, 2])+rnorm(nrow(bamboo_2_traits), 0, 1) 
# bam_2_gam <- funspaceGAM(y=bam_2_abs, funspace=bam_2_funtest)
summary(bam_2_funtest)
plot(bam_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo post-winter year 1", cex.main = 1.5)

plot(bam_2_funtest,
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 0.5)
 
 
# ### using  Principal Coordinates Analysis 
# bam_2_matrix <- model.matrix(~ . - 1, data = bamboo_2_traits)
# bam_2_mat_dis<- vegdist(bam_2_matrix, method = "gower") # Calculate distance matrix using Gower method
# bam_2_scale<- cmdscale(bam_2_mat_dis, k=2) 
# bam_2_funtest <- funspace(x = bam_2_scale, threshold = 0.95)
# bam_2_abs <- abs(bam_2_scale[, 1]*bam_2_scale[, 2])+rnorm(nrow(bam_2_matrix), mean=0, sd=1)
# bam_2_gam<- funspaceGAM(y=bam_2_abs, funspace=bam_2_funtest)
# summary(bam_2_funtest)
# plot(bam_2_funtest)
# 
# plot(x=bam_2_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(bamboo_2_traits)
x2<- princomp(bamboo_2_traits)
funtest2 <- funspace(x = x2, PCs = c(1, 2), threshold = 0.95)
summary(funtest2)
plot(x=funtest2,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Bamboo phase 2", cex.main = 1.5)

## phase 3 pre-winter ###==== using bamboo_3_traits

bam_3_matrix <- model.matrix(~ . - 1, data = bamboo_3_traits)
bam_3_matrix_dissimilarity<- vegdist(bam_3_matrix, method = "gower")
bam_3_nmds_result <- metaMDS(bam_3_matrix_dissimilarity, k = 2)
#bam_3_nmds_result_scores <- scores(bam_3_nmds_result, display = "sites")
bam_3_funtest <- funspace(x = bam_3_nmds_result, threshold = 0.95)
# bam_3_abs<- abs(bam_3_nmds_result_scores[, 1]*bam_3_nmds_result_scores[, 2])+rnorm(nrow(bamboo_3_traits), 0, 1) 
# bam_3_gam <- funspaceGAM(y=bam_3_abs, funspace=bam_3_funtest)
summary(bam_3_funtest)
plot(bam_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo pre-winter year 2", cex.main = 1.5)
     
     ,
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)
# 
# 
# bam_3_matrix <- model.matrix(~ . - 1, data = bamboo_3_traits)
# bam_3_mat_dis<- vegdist(bam_3_matrix, method = "gower") # Calculate distance matrix using Gower method
# bam_3_scale<- cmdscale(bam_3_mat_dis, k=2) 
# bam_3_funtest <- funspace(x = bam_3_scale, threshold = 0.95)
# bam_3_abs <- abs(bam_3_scale[, 1]*bam_3_scale[, 2])+rnorm(nrow(bam_3_matrix), 0, 1)
# bam_3_gam<- funspaceGAM(y=bam_3_abs, funspace=bam_3_funtest)
# summary(bam_3_funtest)
# plot(bam_3_funtest)
# 
# plot(x=bam_3_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(bamboo_3_traits)
x3<- princomp(bamboo_3_traits)
funtest3 <- funspace(x = x3, PCs = c(1, 2), threshold = 0.95)
summary(funtest3)
plot(x=funtest3,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Bamboo phase 3", cex.main = 1.5)

## phase 4 post-winter ###==== using bamboo_4_traits

bam_4_matrix <- model.matrix(~ . - 1, data = bamboo_4_traits)
bam_4_matrix_dissimilarity<- vegdist(bam_4_matrix, method = "gower")
bam_4_nmds_result <- metaMDS(bam_4_matrix_dissimilarity, k = 2)
#bam_4_nmds_result_scores <- scores(bam_4_nmds_result, display = "sites")
bam_4_funtest <- funspace(x = bam_4_nmds_result, threshold = 0.95)
# bam_4_abs<- abs(bam_4_nmds_result_scores[, 1]*bam_4_nmds_result_scores[, 2])+rnorm(nrow(bamboo_4_traits), 0, 1)
# bam_4_gam <- funspaceGAM(y=bam_4_abs, funspace=bam_4_funtest)
summary(bam_4_funtest)
plot(bam_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo post-winter year 2", cex.main = 1.5)
     ,
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)
# 
# bam_4_matrix <- model.matrix(~ . - 1, data = bamboo_4_traits)
# bam_4_mat_dis<- vegdist(bam_4_matrix, method = "gower") # Calculate distance matrix using Gower method
# bam_4_scale<- cmdscale(bam_4_mat_dis, k=2) 
# bam_4_funtest <- funspace(x = bam_4_scale, threshold = 0.95)
# bam_4_abs <- abs(bam_4_scale[, 1]*bam_4_scale[, 2])+rnorm(nrow(bam_4_matrix), 0, 1)
# bam_4_gam<- funspaceGAM(y=bam_4_abs, funspace=bam_4_funtest)
# summary(bam_4_funtest)
# plot(bam_4_funtest)
# 
# plot(x=bam_4_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(bamboo_4_traits)
x4<- princomp(bamboo_4_traits)
funtest4 <- funspace(x = x4, PCs = c(1, 2), threshold = 0.95)
summary(funtest4)
plot(x=funtest4,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Bamboo phase 4", cex.main = 1.5)


### ALL BAMBOO

bam_all_matrix <- model.matrix(~ . - 1, data = unique_rows_bam_traits)
bam_all_matrix_dissimilarity<- vegdist(bam_all_matrix, method = "gower")
bam_all_nmds_result <- metaMDS(bam_all_matrix_dissimilarity, k = 2)
#for_4_nmds_result_scores <- scores(for_4_nmds_result, display = "sites")
bam_all_funtest <- funspace(x = bam_all_nmds_result, threshold = 0.95)
# for_4_abs<- abs(for_4_nmds_result_scores[, 1]*for_4_nmds_result_scores[, 2])+rnorm(nrow(forest_4_traits), 0, 1) 
# for_4_gam <- funspaceGAM(y=for_4_abs, funspace=for_4_funtest)
summary(bam_all_funtest)
plot(bam_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo functional space", cex.main = 1.5)



### FOREST ### ====
forest_1<- traits_all %>%
  filter(lulc== "forest", year=="2021-2022", season== "pre-winter")
forest_1_traits<- subset(forest_1, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(forest_1_traits)<- forest_1_traits$scientific_name
colnames(forest_1_traits) <- gsub("_", " ", colnames(forest_1_traits))

forest_2<- traits_all %>%
  filter(lulc== "forest", year=="2021-2022", season== "post-winter")
forest_2_traits<- subset(forest_2, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(forest_2_traits)<- forest_2_traits$scientific_name
colnames(forest_2_traits) <- gsub("_", " ", colnames(forest_2_traits))

forest_3<- traits_all %>%
  filter(lulc== "forest", year=="2022-2023", season== "pre-winter")
forest_3_traits<- subset(forest_3, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(forest_3_traits)<- forest_3_traits$scientific_name
colnames(forest_3_traits) <- gsub("_", " ", colnames(forest_3_traits))

forest_4<- traits_all %>%
  filter(lulc== "forest", year=="2022-2023", season== "post-winter")
forest_4_traits<- subset(forest_4, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(forest_4_traits)<- forest_4_traits$scientific_name
colnames(forest_4_traits) <- gsub("_", " ", colnames(forest_4_traits))


forest_all<- traits_all %>%
  filter(lulc== "forest")
unique_rows_for <- forest_all[!duplicated(forest_all$scientific_name), ]
unique_rows_for_traits<- subset(unique_rows_for, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_for_traits)<- unique_rows_for_traits$scientific_name
colnames(unique_rows_for_traits) <- gsub("_", " ", colnames(unique_rows_for_traits))
unique(unique_rows_for_traits)

## phase 1 pre-winter ### using forest_1_traits

for_1_matrix <- model.matrix(~ . - 1, data = forest_1_traits)
for_1_matrix_dissimilarity<- vegdist(for_1_matrix, method = "gower")
for_1_nmds_result <- metaMDS(for_1_matrix_dissimilarity, k = 2)
#for_1_nmds_result_scores <- scores(for_1_nmds_result, display = "sites")
for_1_funtest <- funspace(x = for_1_nmds_result, threshold = 0.95)
# for_1_abs<- abs(for_1_nmds_result_scores[, 1]*for_1_nmds_result_scores[, 2])+rnorm(nrow(forest_1_traits), 0, 1) 
# for_1_gam <- funspaceGAM(y=for_1_abs, funspace=for_1_funtest)
summary(for_1_funtest)
plot(for_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
  title(main = "Forest pre-winter year 1", cex.main = 1.5)
plot(for_1_funtest,
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)
# 
# 
# for_1_matrix <- model.matrix(~ . - 1, data = forest_1_traits)
# for_1_mat_dis<- vegdist(for_1_matrix, method = "gower") # Calculate distance matrix using Gower method
# for_1_scale<- cmdscale(for_1_mat_dis, k=2) 
# for_1_funtest <- funspace(x = for_1_scale, threshold = 0.95)
# for_1_abs <- abs(for_1_scale[, 1]*for_1_scale[, 2])+rnorm(nrow(for_1_matrix), 0, 1)
# for_1_gam<- funspaceGAM(y=for_1_abs, funspace=for_1_funtest)
# summary(for_1_funtest)
# plot(for_1_funtest)
# 
# plot(x=for_1_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(forest_1_traits)
x5<- princomp(forest_1_traits)
funtest5 <- funspace(x = x5, PCs = c(1, 2), threshold = 0.95)
summary(funtest5)
plot(x=funtest5,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Forest phase 1", cex.main = 1.5)

### phase 2 post-winter ### using forest_2_traits

for_2_matrix <- model.matrix(~ . - 1, data = forest_2_traits)
for_2_matrix_dissimilarity<- vegdist(for_2_matrix, method = "gower")
for_2_nmds_result <- metaMDS(for_2_matrix_dissimilarity, k = 2)
#for_2_nmds_result_scores <- scores(for_2_nmds_result, display = "sites")
for_2_funtest <- funspace(x = for_2_nmds_result, threshold = 0.95)
# for_2_abs<- abs(for_2_nmds_result_scores[, 1]*for_2_nmds_result_scores[, 2])+rnorm(nrow(forest_2_traits), 0, 1) 
# for_2_gam <- funspaceGAM(y=for_2_abs, funspace=for_2_funtest)
summary(for_2_funtest)
plot(for_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest post-winter year 1", cex.main = 1.5)
plot(for_2_funtest,
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)

# 
# for_2_matrix <- model.matrix(~ . - 1, data = forest_2_traits)
# for_2_mat_dis<- vegdist(for_2_matrix, method = "gower") # Calculate distance matrix using Gower method
# for_2_scale<- cmdscale(for_2_mat_dis, k=2) 
# for_2_funtest <- funspace(x = for_2_scale, threshold = 0.95)
# for_2_abs <- abs(for_2_scale[, 1]*for_2_scale[, 2])+rnorm(nrow(for_2_matrix), 0, 1)
# for_2_gam<- funspaceGAM(y=for_2_abs, funspace=for_2_funtest)
# summary(for_2_funtest)
# plot(for_2_funtest)
# 
# plot(x=for_2_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(forest_2_traits)
x6<- princomp(forest_2_traits)
funtest6 <- funspace(x = x6, PCs = c(1, 2), threshold = 0.95)
summary(funtest6)
plot(x=funtest6,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Forest phase 2", cex.main = 1.5)

### phase 3 pre-winter ### using forest_3_traits

for_3_matrix <- model.matrix(~ . - 1, data = forest_3_traits)
for_3_matrix_dissimilarity<- vegdist(for_3_matrix, method = "gower")
for_3_nmds_result <- metaMDS(for_3_matrix_dissimilarity, k = 2)
#for_3_nmds_result_scores <- scores(for_3_nmds_result, display = "sites")
for_3_funtest <- funspace(x = for_3_nmds_result, threshold = 0.95)
# for_3_abs<- abs(for_3_nmds_result_scores[, 1]*for_3_nmds_result_scores[, 2])+rnorm(nrow(forest_3_traits), 0, 1) 
# for_3_gam <- funspaceGAM(y=for_3_abs, funspace=for_3_funtest)
summary(for_3_funtest)
plot(for_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest pre-winter year 2", cex.main = 1.5)

plot(for_3_funtest,
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)

# for_3_matrix <- model.matrix(~ . - 1, data = forest_3_traits)
# for_3_mat_dis<- vegdist(for_3_matrix, method = "gower") # Calculate distance matrix using Gower method
# for_3_scale<- cmdscale(for_3_mat_dis, k=2) 
# for_3_funtest <- funspace(x = for_3_scale, threshold = 0.95)
# for_3_abs <- abs(for_3_scale[, 1]*for_3_scale[, 2])+rnorm(nrow(for_3_matrix), 0, 1)
# for_3_gam<- funspaceGAM(y=for_3_abs, funspace=for_3_funtest)
# summary(for_3_funtest)
# plot(for_3_funtest)
# 
# plot(x=for_3_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(forest_3_traits)
x7<- princomp(forest_3_traits)
funtest7 <- funspace(x = x7, PCs = c(1, 2), threshold = 0.95)
summary(funtest7)
plot(x=funtest7,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Forest phase 3", cex.main = 1.5)


### phase 4 post-winter ### using forest_4_traits

for_4_matrix <- model.matrix(~ . - 1, data = forest_4_traits)
for_4_matrix_dissimilarity<- vegdist(for_4_matrix, method = "gower")
for_4_nmds_result <- metaMDS(for_4_matrix_dissimilarity, k = 2)
#for_4_nmds_result_scores <- scores(for_4_nmds_result, display = "sites")
for_4_funtest <- funspace(x = for_4_nmds_result, threshold = 0.95)
# for_4_abs<- abs(for_4_nmds_result_scores[, 1]*for_4_nmds_result_scores[, 2])+rnorm(nrow(forest_4_traits), 0, 1) 
# for_4_gam <- funspaceGAM(y=for_4_abs, funspace=for_4_funtest)
summary(for_4_funtest)
plot(for_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest post-winter year 2", cex.main = 1.5)

# plot(for_4_funtest,
#      axis.title.x = "Mass",
#      axis.title.y = "Beak Depth",
#      axis.title.cex = 1.5,
#      axis.title.line = 2.5,
#      axis.cex = 2.0)

# 
# for_4_matrix <- model.matrix(~ . - 1, data = forest_4_traits)
# for_4_mat_dis<- vegdist(for_4_matrix, method = "gower") # Calculate distance matrix using Gower method
# for_4_scale<- cmdscale(for_4_mat_dis, k=2) 
# for_4_funtest <- funspace(x = for_4_scale, threshold = 0.95)
# for_4_abs <- abs(for_4_scale[, 1]*for_4_scale[, 2])+rnorm(nrow(for_4_matrix), 0, 1)
# for_4_gam<- funspaceGAM(y=for_4_abs, funspace=for_4_funtest)
# summary(for_4_funtest)
# plot(for_4_funtest)
# 
# plot(x=for_4_gam,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(forest_4_traits)
x8<- princomp(forest_4_traits)
funtest8 <- funspace(x = x8, PCs = c(1, 2), threshold = 0.95)
summary(funtest8)
plot(x=funtest8,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Forest phase 4", cex.main = 1.5)


### ALL forest

for_all_matrix <- model.matrix(~ . - 1, data = unique_rows_for_traits)
for_all_matrix_dissimilarity<- vegdist(for_all_matrix, method = "gower")
for_all_nmds_result <- metaMDS(for_all_matrix_dissimilarity, k = 2)
#for_4_nmds_result_scores <- scores(for_4_nmds_result, display = "sites")
for_all_funtest <- funspace(x = for_all_nmds_result, threshold = 0.95)
# for_4_abs<- abs(for_4_nmds_result_scores[, 1]*for_4_nmds_result_scores[, 2])+rnorm(nrow(forest_4_traits), 0, 1) 
# for_4_gam <- funspaceGAM(y=for_4_abs, funspace=for_4_funtest)
summary(for_all_funtest)
plot(for_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest functional space", cex.main = 1.5)


### JHUM ### ====

jhum_1<- traits_all %>%
  filter(lulc== "jhum", year=="2021-2022", season== "pre-winter")
jhum_1_traits<- subset(jhum_1, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(jhum_1_traits)<- jhum_1_traits$scientific_name
colnames(jhum_1_traits) <- gsub("_", " ", colnames(jhum_1_traits))

jhum_2<- traits_all %>%
  filter(lulc== "jhum", year=="2021-2022", season== "post-winter")
jhum_2_traits<- subset(jhum_2, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(jhum_2_traits)<- jhum_2_traits$scientific_name
colnames(jhum_2_traits) <- gsub("_", " ", colnames(jhum_2_traits))

jhum_3<- traits_all %>%
  filter(lulc== "jhum", year=="2022-2023", season== "pre-winter")
jhum_3_traits<- subset(jhum_3, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(jhum_3_traits)<- jhum_3_traits$scientific_name
colnames(jhum_3_traits) <- gsub("_", " ", colnames(jhum_3_traits))

jhum_4<- traits_all %>%
  filter(lulc== "jhum", year=="2022-2023", season== "post-winter")
jhum_4_traits<- subset(jhum_4, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(jhum_4_traits)<- jhum_4_traits$scientific_name
colnames(jhum_4_traits) <- gsub("_", " ", colnames(jhum_4_traits))


jhum_all<- traits_all %>%
  filter(lulc== "jhum")
unique_rows_jhum <- jhum_all[!duplicated(jhum_all$scientific_name), ]
unique_rows_jhum_traits<- subset(unique_rows_jhum, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_jhum_traits)<- unique_rows_jhum_traits$scientific_name
colnames(unique_rows_jhum_traits) <- gsub("_", " ", colnames(unique_rows_jhum_traits))
unique(unique_rows_jhum_traits)


## phase 1 pre-winter ### using forest_1_traits

jhum_1_matrix <- model.matrix(~ . - 1, data = jhum_1_traits)
jhum_1_matrix_dissimilarity<- vegdist(jhum_1_matrix, method = "gower")
jhum_1_nmds_result <- metaMDS(jhum_1_matrix_dissimilarity, k = 2)
#jhum_1_nmds_result_scores <- scores(jhum_1_nmds_result, display = "sites")
jhum_1_funtest <- funspace(x = jhum_1_nmds_result, threshold = 0.95)
#jhum_1_abs<- abs(jhum_1_nmds_result_scores[, 1]*jhum_1_nmds_result_scores[, 2])+rnorm(nrow(jhum_1_traits), 0, 1) 
#jhum_1_gam <- funspaceGAM(y=jhum_1_abs, funspace=jhum_1_funtest)
summary(jhum_1_funtest)
plot(jhum_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum pre-winter year 1", cex.main = 1.5)


jhum_1_matrix <- model.matrix(~ . - 1, data = jhum_1_traits)
jhum_1_mat_dis<- vegdist(jhum_1_matrix, method = "gower") # Calculate distance matrix using Gower method
#jhum_1_scale<- cmdscale(jhum_1_mat_dis, k=2)
jhum_1_funtest <- funspace(x = jhum_1_scale, threshold = 0.95)
jhum_1_abs <- abs(jhum_1_scale[, 1]*jhum_1_scale[, 2])+rnorm(nrow(jhum_1_matrix), 0, 1)
#jhum_1_gam<- funspaceGAM(y=jhum_1_abs, funspace=jhum_1_funtest, minObs = 15)
summary(jhum_1_funtest)
plot(jhum_1_funtest)

# plot(x=jhum_1_funtest,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

#funspaceDim(jhum_1_traits)
x9<- princomp(jhum_1_traits)
funtest9 <- funspace(x = x9, PCs = c(1, 2), threshold = 0.95)
summary(funtest9)
plot(x=funtest9,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Jhum phase 1", cex.main = 1.5)

### phase 2 post-winter ### using jhum_2_traits

jhum_2_matrix <- model.matrix(~ . - 1, data = jhum_2_traits)
jhum_2_matrix_dissimilarity<- vegdist(jhum_2_matrix, method = "gower")
jhum_2_nmds_result <- metaMDS(jhum_2_matrix_dissimilarity, k = 2)
#jhum_2_nmds_result_scores <- scores(jhum_2_nmds_result, display = "sites")
jhum_2_funtest <- funspace(x = jhum_2_nmds_result, threshold = 0.95)
#jhum_2_abs<- abs(jhum_2_nmds_result_scores[, 1]*jhum_2_nmds_result_scores[, 2])+rnorm(nrow(jhum_2_traits), 0, 1)
#jhum_2_gam <- funspaceGAM(y=jhum_2_abs, funspace=jhum_2_funtest)
summary(jhum_2_funtest)
plot(jhum_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum post-winter year 1", cex.main = 1.5)
     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)

 
# jhum_2_matrix <- model.matrix(~ . - 1, data = jhum_2_traits)
# jhum_2_mat_dis<- vegdist(jhum_2_matrix, method = "gower") # Calculate distance matrix using Gower method
# jhum_2_scale<- cmdscale(jhum_2_mat_dis, k=2)
# jhum_2_funtest <- funspace(x = jhum_2_scale, threshold = 0.95)
# jhum_1_abs <- abs(jhum_1_scale[, 1]*jhum_1_scale[, 2])+rnorm(nrow(jhum_1_matrix), 0, 1)
# jhum_1_gam<- funspaceGAM(y=jhum_1_abs, funspace=jhum_1_funtest, minObs = 15)
# summary(jhum_2_funtest)
# plot(jhum_2_funtest)
# 
# plot(x=jhum_2_funtest,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

funspaceDim(jhum_2_traits)
x10<- princomp(jhum_2_traits)
funtest10 <- funspace(x = x10, PCs = c(1, 2), threshold = 0.95)
summary(funtest10)
plot(x=funtest10,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Jhum phase 2", cex.main = 1.5)


### phase 3 pre-winter ### using jhum_3_traits

jhum_3_matrix <- model.matrix(~ . - 1, data = jhum_3_traits)
jhum_3_matrix_dissimilarity<- vegdist(jhum_3_matrix, method = "gower") 
jhum_3_nmds_result <- metaMDS(jhum_3_matrix_dissimilarity, k = 2)
#jhum_3_nmds_result_scores <- scores(jhum_3_nmds_result, display = "sites")
jhum_3_funtest <- funspace(x = jhum_3_nmds_result, threshold = 0.95)
#jhum_3_abs<- abs(jhum_3_nmds_result_scores[, 1]*jhum_3_nmds_result_scores[, 2])+rnorm(nrow(jhum_3_traits), 0, 1) 
#jhum_3_gam <- funspaceGAM(y=jhum_3_abs, funspace=jhum_3_funtest)
summary(jhum_3_funtest)
plot(jhum_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum pre-winter year 2", cex.main = 1.5)

     axis.title.x = "Mass",
     axis.title.y = "Beak Depth",
     axis.title.cex = 1.5,
     axis.title.line = 2.5,
     axis.cex = 2.0)


# jhum_3_matrix <- model.matrix(~ . - 1, data = jhum_3_traits)
# jhum_3_mat_dis<- vegdist(jhum_3_matrix, method = "gower") # Calculate distance matrix using Gower method
# jhum_3_scale<- cmdscale(jhum_3_mat_dis, k=2) 
# jhum_3_funtest <- funspace(x = jhum_3_scale, threshold = 0.95)
# #jhum_1_abs <- abs(jhum_1_scale[, 1]*jhum_1_scale[, 2])+rnorm(nrow(jhum_1_matrix), 0, 1)
# #jhum_1_gam<- funspaceGAM(y=jhum_1_abs, funspace=jhum_1_funtest, minObs = 15)
# summary(jhum_3_funtest)
# plot(jhum_3_funtest)
# 
# plot(x=jhum_3_funtest,
#      type="global",
#      quant.plot=TRUE,
#      quant.col="grey80",
#      globalContour=T,
#      pnt=T,
#      pnt.cex=1.5,
#      pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # colour for points
#      axis.title.line=1,
#      arrows=TRUE,
#      arrows.length=0.9)

funspaceDim(jhum_3_traits)
x11<- princomp(jhum_3_traits)
funtest11 <- funspace(x = x11, PCs = c(1, 2), threshold = 0.95)
summary(funtest11)
plot(x=funtest11,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Jhum phase 3", cex.main = 1.5)

### phase 4 post-winter ### using forest_4_traits

jhum_4_matrix <- model.matrix(~ . - 1, data = jhum_4_traits)
jhum_4_matrix_dissimilarity<- vegdist(jhum_4_matrix, method = "gower")
jhum_4_nmds_result <- metaMDS(jhum_4_matrix_dissimilarity, k = 2)
#jhum_4_nmds_result_scores <- scores(jhum_4_nmds_result, display = "sites")
jhum_4_funtest <- funspace(x = jhum_4_nmds_result, threshold = 0.95)
#jhum_4_abs<- abs(jhum_4_nmds_result_scores[, 1]*jhum_4_nmds_result_scores[, 2])+rnorm(nrow(jhum_4_traits), 0, 1)
#jhum_4_gam <- funspaceGAM(y=jhum_4_abs, funspace=jhum_4_funtest)
summary(jhum_4_funtest)
plot(jhum_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum post-winter year 2", cex.main = 1.5)

plot(jhum_4_funtest, quant.plot=TRUE,quant.lwd = 2, pnt = TRUE, pnt.cex = 0.1,
     pnt.col = rgb(0.1, 0.8, 0.2, alpha = 0.2), arrows = TRUE, arrows.length = 0.7)

#funspaceDim(jhum_4_traits)
x12<- princomp(jhum_4_traits)
funtest12 <- funspace(x = x12, PCs = c(1, 2), threshold = 0.95)
summary(funtest12)
plot(x=funtest12,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Jhum phase 4", cex.main = 1.5)


### ALL jhum

jhum_all_matrix <- model.matrix(~ . - 1, data = unique_rows_jhum_traits)
jhum_all_matrix_dissimilarity<- vegdist(jhum_all_matrix, method = "gower")
jhum_all_nmds_result <- metaMDS(jhum_all_matrix_dissimilarity, k = 2)
#for_4_nmds_result_scores <- scores(for_4_nmds_result, display = "sites")
jhum_all_funtest <- funspace(x = jhum_all_nmds_result, threshold = 0.95)
# for_4_abs<- abs(for_4_nmds_result_scores[, 1]*for_4_nmds_result_scores[, 2])+rnorm(nrow(forest_4_traits), 0, 1) 
# for_4_gam <- funspaceGAM(y=for_4_abs, funspace=for_4_funtest)
summary(jhum_all_funtest)
plot(jhum_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum functional space", cex.main = 1.5)



### ABANDONED ### ====

abn_1<- traits_all %>%
  filter(lulc== "abandoned", year=="2021-2022", season== "pre-winter")
abn_1_traits<- subset(abn_1, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(abn_1_traits)<- abn_1_traits$scientific_name
colnames(abn_1_traits) <- gsub("_", " ", colnames(abn_1_traits))

abn_2<- traits_all %>%
  filter(lulc== "abandoned", year=="2021-2022", season== "post-winter")
abn_2_traits<- subset(abn_2, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(abn_2_traits)<- abn_2_traits$scientific_name
colnames(abn_2_traits) <- gsub("_", " ", colnames(abn_2_traits))

abn_3<- traits_all %>%
  filter(lulc== "abandoned", year=="2022-2023", season== "pre-winter")
abn_3_traits<- subset(abn_3, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(abn_3_traits)<- abn_3_traits$scientific_name
colnames(abn_3_traits) <- gsub("_", " ", colnames(abn_3_traits))

abn_4<- traits_all %>%
  filter(lulc== "abandoned", year=="2022-2023", season== "post-winter")
abn_4_traits<- subset(abn_4, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(abn_4_traits)<- abn_4_traits$scientific_name
colnames(abn_4_traits) <- gsub("_", " ", colnames(abn_4_traits))


abn_all<- traits_all %>%
  filter(lulc== "abandoned")
unique_rows_abn <- abn_all[!duplicated(abn_all$scientific_name), ]
unique_rows_abn_traits<- subset(unique_rows_abn, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_abn_traits)<- unique_rows_abn_traits$scientific_name
colnames(unique_rows_abn_traits) <- gsub("_", " ", colnames(unique_rows_abn_traits))
unique(unique_rows_abn_traits)

## phase 1 pre-winter ### using abn_1_traits

abn_1_matrix <- model.matrix(~ . - 1, data = abn_1_traits)
abn_1_matrix_dissimilarity<- vegdist(abn_1_matrix, method = "gower")
abn_1_nmds_result <- cmdscale(abn_1_matrix_dissimilarity, k=2)
#abn_1_nmds_result_scores <- scores(abn_1_nmds_result, display = "sites")
abn_1_funtest <- funspace(x = abn_1_nmds_result, threshold = 0.95)
#abn_1_abs<- abs(abn_1_nmds_result_scores[, 1]*abn_1_nmds_result_scores[, 2])+rnorm(nrow(abn_1_traits), 0, 1)
#abn_1_gam <- funspaceGAM(y=abn_1_abs, funspace=abn_1_funtest)
summary(abn_1_funtest)
plot(abn_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum pre-winter year 1", cex.main = 1.5)

plot(abn_1_funtest, quant.plot=TRUE)

#funspaceDim(abn_1_traits)
x13<- princomp(abn_1_traits)
funtest13 <- funspace(x = x13, PCs = c(1, 2), threshold = 0.95)
summary(funtest13)
plot(x=funtest13,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Abandoned jhum phase 1", cex.main = 1.5)


### phase 2 post-winter ### using jhum_2_traits

abn_2_matrix <- model.matrix(~ . - 1, data = abn_2_traits)
abn_2_matrix_dissimilarity<- vegdist(abn_2_matrix, method = "gower")
abn_2_nmds_result <- metaMDS(abn_2_matrix_dissimilarity, k = 2)
#abn_2_nmds_result_scores <- scores(abn_2_nmds_result, display = "sites")
abn_2_funtest <- funspace(x = abn_2_nmds_result, threshold = 0.95)
#abn_2_abs<- abs(abn_2_nmds_result_scores[, 1]*abn_2_nmds_result_scores[, 2])+rnorm(nrow(abn_2_traits), 0, 1)
#abn_2_gam <- funspaceGAM(y=abn_2_abs, funspace=abn_2_funtest)

summary(abn_2_funtest)
plot(abn_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum post-winter year 1", cex.main = 1.5)

plot(abn_2_funtest, quant.plot=TRUE)

funspaceDim(abn_2_traits)
x14<- princomp(abn_2_traits)
funtest14 <- funspace(x = x14, PCs = c(1, 2), threshold = 0.95)
summary(funtest14)
plot(x=funtest14,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Abandoned jhum phase 2", cex.main = 1.5)


### phase 3 pre-winter ### using jhum_3_traits

abn_3_matrix <- model.matrix(~ . - 1, data = abn_3_traits)
abn_3_matrix_dissimilarity<- vegdist(abn_3_matrix, method = "gower")
abn_3_nmds_result <- metaMDS(abn_3_matrix_dissimilarity, k = 2)
#abn_3_nmds_result_scores <- scores(abn_3_nmds_result, display = "sites")
abn_3_funtest <- funspace(x = abn_3_nmds_result, threshold = 0.95)
#abn_3_abs<- abs(abn_3_nmds_result_scores[, 1]*abn_3_nmds_result_scores[, 2])+rnorm(nrow(abn_3_traits), 0, 1)
#abn_3_gam <- funspaceGAM(y=abn_3_abs, funspace=abn_3_funtest)
summary(abn_3_funtest)
plot(abn_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum pre-winter year 2", cex.main = 1.5)

plot(abn_3_funtest, quant.plot=TRUE)

funspaceDim(abn_3_traits)
x15<- princomp(abn_3_traits)
funtest15 <- funspace(x = x15, PCs = c(1, 2), threshold = 0.95)
summary(funtest15)
plot(x=funtest15,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Abandoned jhum phase 3", cex.main = 1.5)

### phase 4 post-winter ### using forest_4_traits

abn_4_matrix <- model.matrix(~ . - 1, data = abn_4_traits)
abn_4_matrix_dissimilarity<- vegdist(abn_4_matrix, method = "gower")
abn_4_nmds_result <- metaMDS(abn_4_matrix_dissimilarity, k = 2)
#abn_4_nmds_result_scores <- scores(abn_4_nmds_result, display = "sites")
abn_4_funtest <- funspace(x = abn_4_nmds_result, threshold = 0.95)
#abn_4_abs<- abs(abn_4_nmds_result_scores[, 1]*abn_4_nmds_result_scores[, 2])+rnorm(nrow(abn_4_traits), 0, 1)
#abn_4_gam <- funspaceGAM(y=abn_4_abs, funspace=abn_4_funtest)
summary(abn_4_funtest)
plot(abn_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum post-winter year 2", cex.main = 1.5)

plot(abn_4_funtest, quant.plot=TRUE,quant.lwd = 2, pnt = TRUE, pnt.cex = 0.1,
     pnt.col = rgb(0.1, 0.8, 0.2, alpha = 0.2), arrows = TRUE, arrows.length = 0.7)

#funspaceDim(abn_4_traits)
x16<- princomp(abn_4_traits)
funtest16 <- funspace(x = x16, PCs = c(1, 2), threshold = 0.95)
summary(funtest16)
plot(x=funtest16,
     type="global",
     globalContour=T,
     pnt=F,
     axis.title.line=2.5,
     arrows=TRUE,
     axis.title.cex=1.5,
     arrows.length=1.2,
     arrows.head =0.2,
     arrows.label.pos=1.2)
title(main = "Abandoned jhum phase 4", cex.main = 1.5)


### ALL abandoned

abn_all_matrix <- model.matrix(~ . - 1, data = unique_rows_abn_traits)
abn_all_matrix_dissimilarity<- vegdist(abn_all_matrix, method = "gower")
abn_all_nmds_result <- metaMDS(abn_all_matrix_dissimilarity, k = 2)
#for_4_nmds_result_scores <- scores(for_4_nmds_result, display = "sites")
abn_all_funtest <- funspace(x = abn_all_nmds_result, threshold = 0.95)
# for_4_abs<- abs(for_4_nmds_result_scores[, 1]*for_4_nmds_result_scores[, 2])+rnorm(nrow(forest_4_traits), 0, 1) 
# for_4_gam <- funspaceGAM(y=for_4_abs, funspace=for_4_funtest)
summary(abn_all_funtest)
plot(abn_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum functional space", cex.main = 1.5)




#install.packages("ecospat.nichePOSNB",repos = NULL,type = "source")

#### more plots for functional traits ##########
library(gghalves)
library(ggridges)

traits_all$lulc<- factor(traits_all$lulc, levels=c("forest", "bamboo", "jhum", "abandoned"))
traits_all$season<- factor(traits_all$season, levels=c("pre-winter", "post-winter"))
traits_all$year<- factor(traits_all$year, levels= c("2021-2022", "2022-2023"))

traits_pre_winter<- traits_all %>% 
  filter(season== "pre-winter")

traits_post_winter<- traits_all %>% 
  filter(season== "post-winter")

### mass######

my_comparisons <- list( c("forest", "bamboo"), c("bamboo", "jhum"), c("bamboo","abandoned"), c("forest", "jhum"), c("forest", "abandoned"), c("jhum", "abandoned"))
my_comparisons_seasons <- list(c("forest-pre-winter", "forest-post-winter"), c("bamboo-pre-winter", "bamboo-post-winter"), c("jhum-pre-winter", "jhum-post-winter"), c("abandoned-pre-winter", "abandoned-post-winter"))
microclimate_all$lulc_season<- factor(microclimate_all$lulc_season, levels= c("Forest-pre-winter", "Forest-post-winter", "Bamboo-pre-winter", "Bamboo-post-winter", "Jhum-pre-winter", "Jhum-post-winter", "Abandoned-pre-winter", "Abandoned-post-winter"))

### overall 


forest_mass <- traits_all$mass[traits_all$lulc == "forest"]
bamboo_mass <- traits_all$mass[traits_all$lulc == "bamboo"]
jhum_mass <- traits_all$mass[traits_all$lulc == "jhum"]
abandoned_mass <- traits_all$mass[traits_all$lulc == "abandoned"]


# Perform pairwise t-tests
pairwise_results_mass <- list(
  t_test_forest_bamboo = t.test(forest_mass, bamboo_mass),
  t_test_bamboo_jhum = t.test(bamboo_mass, jhum_mass),
  t_test_bamboo_abandoned = t.test(bamboo_mass, abandoned_mass),
  t_test_forest_jhum = t.test(forest_mass, jhum_mass),
  t_test_forest_abandoned = t.test(forest_mass, abandoned_mass),
  t_test_jhum_abandoned = t.test(jhum_mass, abandoned_mass)
)

# Print the results
pairwise_results_mass


ggplot(traits_all, aes(x = lulc, y = mass, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
    geom_point(alpha = 2.0, size=3) +
    scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
    scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
    theme_bw()+
  labs(x = NULL, y = "log(mass)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' body mass")


traits_all$lulc_season<- paste(traits_all$lulc, traits_all$season, sep = "-")
traits_all$lulc_season<- factor(traits_all$lulc_season, levels= c("forest-pre-winter", "forest-post-winter", "bamboo-pre-winter", "bamboo-post-winter",
                                                                  "jhum-pre-winter", "jhum-post-winter", "abandoned-pre-winter", "abandoned-post-winter"))

### pre-winter##

forest_mass_pre <- traits_pre_winter$mass[traits_pre_winter$lulc == "forest"]
bamboo_mass_pre <- traits_pre_winter$mass[traits_pre_winter$lulc == "bamboo"]
jhum_mass_pre <- traits_pre_winter$mass[traits_pre_winter$lulc == "jhum"]
abandoned_mass_pre <- traits_pre_winter$mass[traits_pre_winter$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_mass_pre <- list(
  t_test_forest_bamboo = t.test(forest_mass_pre, bamboo_mass_pre),
  t_test_bamboo_jhum = t.test(bamboo_mass_pre, jhum_mass_pre),
  t_test_bamboo_abandoned = t.test(bamboo_mass_pre, abandoned_mass_pre),
  t_test_forest_jhum = t.test(forest_mass_pre, jhum_mass_pre),
  t_test_forest_abandoned = t.test(forest_mass_pre, abandoned_mass_pre),
  t_test_jhum_abandoned = t.test(jhum_mass_pre, abandoned_mass_pre)
)

# Print the results
pairwise_results_mass_pre


ggplot(traits_pre_winter, aes(x = lulc, y = mass, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 2.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(mass)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' body mass")



### post-winter ##

forest_mass_post <- traits_post_winter$mass[traits_post_winter$lulc == "forest"]
bamboo_mass_post <- traits_post_winter$mass[traits_post_winter$lulc == "bamboo"]
jhum_mass_post <- traits_post_winter$mass[traits_post_winter$lulc == "jhum"]
abandoned_mass_post <- traits_post_winter$mass[traits_post_winter$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_mass_post <- list(
  t_test_forest_bamboo = t.test(forest_mass_post, bamboo_mass_post),
  t_test_bamboo_jhum = t.test(bamboo_mass_post, jhum_mass_post),
  t_test_bamboo_abandoned = t.test(bamboo_mass_post, abandoned_mass_post),
  t_test_forest_jhum = t.test(forest_mass_post, jhum_mass_post),
  t_test_forest_abandoned = t.test(forest_mass_post, abandoned_mass_post),
  t_test_jhum_abandoned = t.test(jhum_mass_post, abandoned_mass_post)
)

# Print the results
pairwise_results_mass_post


ggplot(traits_post_winter, aes(x = lulc, y = mass, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 2.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(mass)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' body mass")


### beak depth#####

### overall 

forest_beak <- traits_all$beak_width[traits_all$lulc == "forest"]
bamboo_beak <- traits_all$beak_width[traits_all$lulc == "bamboo"]
jhum_beak <- traits_all$beak_width[traits_all$lulc == "jhum"]
abandoned_beak <- traits_all$beak_width[traits_all$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_beak <- list(
  t_test_forest_bamboo = t.test(forest_beak, bamboo_beak),
  t_test_bamboo_jhum = t.test(bamboo_beak, jhum_beak),
  t_test_bamboo_abandoned = t.test(bamboo_beak, abandoned_beak),
  t_test_forest_jhum = t.test(forest_beak, jhum_beak),
  t_test_forest_abandoned = t.test(forest_beak, abandoned_beak),
  t_test_jhum_abandoned = t.test(jhum_beak, abandoned_beak)
)

# Print the results
pairwise_results_beak


ggplot(traits_all, aes(x = lulc, y = beak_depth, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 3.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(beak depth)", color= "LULC types")+
  theme(axis.text.x = element_blank(),  # Adjust the size of x-axis labels
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),  # Adjust the legend text size
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' beak depth")


## pre-winter ##

forest_beak_pre <- traits_pre_winter$beak_width[traits_pre_winter$lulc == "forest"]
bamboo_beak_pre <- traits_pre_winter$beak_width[traits_pre_winter$lulc == "bamboo"]
jhum_beak_pre <- traits_pre_winter$beak_width[traits_pre_winter$lulc == "jhum"]
abandoned_beak_pre <- traits_pre_winter$beak_width[traits_pre_winter$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_beak_pre <- list(
  t_test_forest_bamboo = t.test(forest_beak_pre, bamboo_beak_pre),
  t_test_bamboo_jhum = t.test(bamboo_beak_pre, jhum_beak_pre),
  t_test_bamboo_abandoned = t.test(bamboo_beak_pre, abandoned_beak_pre),
  t_test_forest_jhum = t.test(forest_beak_pre, jhum_beak_pre),
  t_test_forest_abandoned = t.test(forest_beak_pre, abandoned_beak_pre),
  t_test_jhum_abandoned = t.test(jhum_beak_pre, abandoned_beak_pre)
)

# Print the results
pairwise_results_beak_pre


ggplot(traits_pre_winter, aes(x = lulc, y = beak_depth, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 2.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(beak depth)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' beak depth")


### post-winter ##

forest_beak_post <- traits_post_winter$beak_width[traits_post_winter$lulc == "forest"]
bamboo_beak_post <- traits_post_winter$beak_width[traits_post_winter$lulc == "bamboo"]
jhum_beak_post <- traits_post_winter$beak_width[traits_post_winter$lulc == "jhum"]
abandoned_beak_post <- traits_post_winter$beak_width[traits_post_winter$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_beak_post <- list(
  t_test_forest_bamboo = t.test(forest_beak_post, bamboo_beak_post),
  t_test_bamboo_jhum = t.test(bamboo_beak_post, jhum_beak_post),
  t_test_bamboo_abandoned = t.test(bamboo_beak_post, abandoned_beak_post),
  t_test_forest_jhum = t.test(forest_beak_post, jhum_beak_post),
  t_test_forest_abandoned = t.test(forest_beak_post, abandoned_beak_post),
  t_test_jhum_abandoned = t.test(jhum_beak_post, abandoned_beak_post)
)

# Print the results
pairwise_results_beak_post


ggplot(traits_post_winter, aes(x = lulc, y = beak_depth, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 2.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(beak depth)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' beak depth")



### wing length ####

## overall
forest_wing <- traits_all$wing_length[traits_all$lulc == "forest"]
bamboo_wing <- traits_all$wing_length[traits_all$lulc == "bamboo"]
jhum_wing <- traits_all$wing_length[traits_all$lulc == "jhum"]
abandoned_wing <- traits_all$wing_length[traits_all$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_wing<- list(
  t_test_forest_bamboo = t.test(forest_wing, bamboo_wing),
  t_test_bamboo_jhum_wing = t.test(bamboo_wing, jhum_wing),
  t_test_bamboo_abandoned = t.test(bamboo_wing, abandoned_wing),
  t_test_forest_jhum = t.test(forest_wing, jhum_wing),
  t_test_forest_abandoned = t.test(forest_wing, abandoned_wing),
  t_test_jhum_abandoned = t.test(jhum_wing, abandoned_wing)
)

# Print the results
pairwise_results_wing

ggplot(traits_all, aes(x = lulc, y = wing_length, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 3.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(wing length)", color= "LULC types")+
  theme(axis.text.x = element_blank(),  # Adjust the size of x-axis labels
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),  # Adjust the legend text size
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' wing length")


## pre-winter

forest_wing_pre <- traits_pre_winter$wing_length[traits_pre_winter$lulc == "forest"]
bamboo_wing_pre <- traits_pre_winter$wing_length[traits_pre_winter$lulc == "bamboo"]
jhum_wing_pre <- traits_pre_winter$wing_length[traits_pre_winter$lulc == "jhum"]
abandoned_wing_pre <- traits_pre_winter$wing_length[traits_pre_winter$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_wing_pre <- list(
  t_test_forest_bamboo = t.test(forest_wing_pre, bamboo_wing_pre),
  t_test_bamboo_jhum = t.test(bamboo_wing_pre, jhum_wing_pre),
  t_test_bamboo_abandoned = t.test(bamboo_wing_pre, abandoned_wing_pre),
  t_test_forest_jhum = t.test(forest_wing_pre, jhum_wing_pre),
  t_test_forest_abandoned = t.test(forest_wing_pre, abandoned_wing_pre),
  t_test_jhum_abandoned = t.test(jhum_wing_pre, abandoned_wing_pre)
)

# Print the results
pairwise_results_wing_pre


ggplot(traits_pre_winter, aes(x = lulc, y = wing_length, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 2.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(wing length)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' wing length")


## post-winter

forest_wing_post <- traits_post_winter$wing_length[traits_post_winter$lulc == "forest"]
bamboo_wing_post <- traits_post_winter$wing_length[traits_post_winter$lulc == "bamboo"]
jhum_wing_post <- traits_post_winter$wing_length[traits_post_winter$lulc == "jhum"]
abandoned_wing_post <- traits_post_winter$wing_length[traits_post_winter$lulc == "abandoned"]

# Perform pairwise t-tests
pairwise_results_wing_post <- list(
  t_test_forest_bamboo = t.test(forest_wing_post, bamboo_wing_post),
  t_test_bamboo_jhum = t.test(bamboo_wing_post, jhum_wing_post),
  t_test_bamboo_abandoned = t.test(bamboo_wing_post, abandoned_wing_post),
  t_test_forest_jhum = t.test(forest_wing_post, jhum_wing_post),
  t_test_forest_abandoned = t.test(forest_wing_post, abandoned_wing_post),
  t_test_jhum_abandoned = t.test(jhum_wing_post, abandoned_wing_post)
)

# Print the results
pairwise_results_wing_post

ggplot(traits_post_winter, aes(x = lulc, y = wing_length, color = lulc)) +
  geom_boxplot(alpha = 3.0, width=0.5)+
  geom_point(alpha = 2.0, size=3) +
  scale_color_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  scale_fill_manual(values = c("#E9967A", "#B0E57C","#87CEEB", "#B19CD9"))+
  stat_compare_means(comparisons = my_comparisons, size = 6.0, method= "t.test") +
  theme_bw()+
  labs(x = NULL, y = "log(wing length)", color= "LULC types")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20))+
  ggtitle("Species' wing length")


## dietary traits ##

trophic_all<- table(traits_all$Trophic.Niche, traits_all$lulc)
library(RColorBrewer)
display.brewer.all()
colors <- brewer.pal(12, "Set3")

barplot(prop.table(trophic_all, 2),
        legend.text = FALSE,
        args.legend = list(cex=0.80,x="topright"),
        #col = pastel_palette,
       ylab = "Proportion of Trophic niches",
        xlab = "LULC types",
        cex.axis=1.2,
        cex.lab = 1.2,
       col = colors)


trophic_life<- table(traits_all$Primary.Lifestyle, traits_all$lulc)
library(RColorBrewer)
display.brewer.all()
colors <- brewer.pal(4, "YlGnBu")

barplot(prop.table(trophic_life, 2),
        legend.text = FALSE,
        args.legend = list(cex=0.80,x="topright"),
        #col = pastel_palette,
        ylab = "Proportion of Trophic lifestyle",
        xlab = "LULC types",
        cex.axis=1.2,
        cex.lab = 1.2,
        col = colors)


########

library(lme4)
library(glmmTMB)
library(DHARMa)

fun_div<- read.csv("funspace_fd.csv")
fun_div$lulc<- factor(fun_div$lulc)
fun_div$season<- factor(fun_div$season)
fun_div$type<- factor(fun_div$type)
fun_div$type <- relevel(fun_div$type, ref = "natural")

model0<- lmer(Frich_p ~ 1 + (1|type), data = fun_div)
summary(model0)


model1 <-lmer(Frich_p ~ mean_temp*lulc + (1|phase),
              data = fun_div)

summary(model1)

fixed_effect_coefficients1 <- coef(model1)
fixed_effect_p_values1 <- summary(model1)$fixed_effect_coefficients1[, "Pr(>|t|)"]
print(fixed_effect_p_values1)


model2 <-lmer(Frich_p ~ var_temp*lulc + (1|phase),
              data = fun_div)

summary(model2)

fixed_effect_coefficients2 <- coef(model2)
fixed_effect_p_values2 <- summary(model2)$fixed_effect_coefficients2[, "Pr(>|t|)"]
print(fixed_effect_p_values2)

model3 <-lmer(Frich_p ~ mean_rh*lulc + (1|phase),
              data = fun_div)

summary(model3)

fixed_effect_coefficients3 <- coef(model3)
fixed_effect_p_values3 <- summary(model3)$fixed_effect_coefficients3[, "Pr(>|t|)"]
print(fixed_effect_p_values3)



model4 <-lmer(Frich_p ~ var_rh*lulc + (1|phase),
              data = fun_div)

summary(model4)

fixed_effect_coefficients4 <- coef(model4)
fixed_effect_p_values4 <- summary(model4)$fixed_effect_coefficients4[, "Pr(>|t|)"]
print(fixed_effect_p_values4)

#### Phylogenetic diversity analysis ##### 
## using code from https://pedrohbraga.github.io/ ### 
required.libraries <- c("ape", "picante", 
                        "pez", "phytools",
                        "vegan", "adephylo", 
                        "phylobase", "geiger", 
                        "mvMORPH", "OUwie", 
                        "hisse", "BAMMtools",
                        "phylosignal", "Biostrings",
                        "devtools","ggplot2", 
                        "kableExtra", "betapart", "gridExtra",
                        "reshape2")

needed.libraries <- required.libraries[!(required.libraries %in% installed.packages()[,"Package"])]
if(length(needed.libraries)) install.packages(needed.libraries)

### Install ggtree from BiocManager

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

set.seed(1)
# Download files from Marc Cadotte's and Jonathan Davies' book 
# "Phylogenies in Ecology: A guide to concepts and methods":

dir.create("data/Jasper")

download.file("https://raw.githubusercontent.com/pedrohbraga/PhyloCompMethods-in-R-workshop/master/data/Jasper/resources/data/Jasper/jasper_data.csv", 
              "data/Jasper/jasper_data.csv")

download.file("https://raw.githubusercontent.com/pedrohbraga/PhyloCompMethods-in-R-workshop/master/data/Jasper/resources/data/Jasper/jasper_tree.phy", 
              "data/Jasper/jasper_tree.phy")

# Download the tree files from this paper

download.file("https://onlinelibrary.wiley.com/action/
downloadSupplement?doi=10.1111%2Fj.1461-0248.2009.01307.x&attachmentId=179085359", 
              destfile = "data/ele_1307_sm_sa1.tre")


JasperPlants.tree <- read.tree("data/Jasper/jasper_tree.phy")

##### phylo ####
library(picante)
library(ape)
## first remove these species from the main dataset which are not there in the Vertlife phylogeny ##
metadata_phylo<- metadata[!metadata$scientific_name %in% c("Prinia superciliaris", "Phylloscopus tephrocephalus", "Cettia castaneocoronata",
                                                           "Horornis brunnescens", "Procarduelis nipalensis", "Horornis fortipes", "Grammatoptila striata",
                                                           "Phylloscopus poliogenys", "Phyllergates cucullatus", "Anthipes monileger", "Chelidorhynx hypoxanthus",
                                                           "Tarsiger rufilatus","Trochalopteron affine", "Cyanoderma chrysaeum" , "Pterorhinus ruficollis", 
                                                           "Cyanoderma ruficeps", "Elachura formosa", "Machlolophus spilonotus", "Schoeniparus castaneceps",
                                                           "Phylloscopus intermedius", "Trochalopteron squamatum",  "Ixos mcclellandii", "Trochalopteron subunicolor", 
                                                           "Phylloscopus castaniceps", "Stachyris humei", "Horornis flavolivaceus", "Hemitesia pallidipes",
                                                           "Brachypteryx leucophris", "Psilopogon virens", "Pomatorhinus superciliaris", "Pterorhinus caerulatus",
                                                           "Staphida castaniceps", "Actinodura strigula", "Dryobates cathpharius", "Psilopogon franklinii"), ]

metadata_phylo$lulcproxy <- ifelse(metadata_phylo$phase == 1, paste0(metadata_phylo$lulc, "_1") , 
                    ifelse(metadata_phylo$phase == 2, paste0(metadata_phylo$lulc, "_2"),
                           ifelse(metadata_phylo$phase == 3, paste0(metadata_phylo$lulc, "_3"),
                                  ifelse(metadata_phylo$phase == 4, paste0(metadata_phylo$lulc, "_4"), metadata_phylo$lulc))))

metadata_phylo_matrix<- metadata_phylo %>%
  group_by(lulcproxy, scientific_name)%>%
  summarise(n=n())

metadata_phylo_matrix_wide<- metadata_phylo_matrix %>%
  pivot_wider(names_from = scientific_name, values_from = n, values_fill=0) %>%
  column_to_rownames("lulcproxy")

metadata_phylo_matrix_wide <- metadata_phylo_matrix_wide[, order(colnames(metadata_phylo_matrix_wide))]

habitat_names<- rownames(metadata_phylo_matrix_wide)

tree_all<- read.nexus("tree2/tree2.nex") ## read the tree 

# Sort tree tips alphabetically
tree_all <- lapply(tree_all, function(tree) {
  tree$tip.label <- sort(tree$tip.label)
  return(tree)
})

#metadata_phylo_matrix_wide <- metadata_phylo_matrix_wide[order(rownames(metadata_phylo_matrix_wide)), , drop = FALSE]


# Initialize empty lists to store results
pd_results <- list()
psr_results <- list()

# Loop through each tree
for (i in seq_along(tree_all)) {
  # Extract the current tree
  tree <- tree_all[[i]]
  tree$tip.label <- sort(tree$tip.label)
  tree$tip.label <- gsub("_", " ", tree$tip.label)
  
  # Loop through each habitat
  for (habitat in habitat_names) {
    # Subset the metadata and phylogenetic matrix for the current habitat
    subset_metadata <- metadata_phylo_matrix_wide[habitat, , drop = FALSE]
    
    # Calculate PD for the current habitat and tree
    pd_result <- pd(subset_metadata, tree, include.root = TRUE)
    
    # Calculate PSR for the current habitat and tree
    psr_result <- psr(subset_metadata, tree, compute.var = TRUE, scale.vcv = TRUE)
    
    # Store the results in the respective lists
    pd_results[[paste0(habitat, "_", i)]] <- pd_result$PD
    psr_results[[paste0(habitat, "_", i)]] <- psr_result$SR
  }
}

# Calculate the average PD and PSR across trees for each habitat
average_pd <- sapply(habitat_names, function(habitat) mean(unlist(pd_results[grep(habitat, names(pd_results))])))
average_psr <- sapply(habitat_names, function(habitat) mean(unlist(psr_results[grep(habitat, names(psr_results))])))

# Combine the results into a data frame
results_df <- data.frame(habitat= habitat_names, Average_PD = average_pd, Average_PSR = average_psr)

#write.csv(results_df, "results_df.csv") # write and export
# Create a data frame to store the averaged results

# Convert row names to a column
results_df$habitat_names <- rownames(results_df)

## plot PD ##

pd<- read.csv("results_pd.csv")
pd$lulc<- factor(pd$lulc, levels=c("forest", "bamboo", "jhum", "abandoned"))

corr2 <- cor.test(pd$mean_temp, pd$mean_rh)
print(corr2)

# Plotting
ggplot(pd, aes(x = lulc, y = Average_PD, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Average Phylogenetic Diversity by Habitat",
       x = "LULC types",
       y = "Average PD") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


### 

ggplot(pd, aes(x = lulc, y = Average_PSR, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Average Phylogenetic Species Richness by Habitat",
       x = "LULC types",
       y = "Average PSR") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


### plot trees for each of the habitat ###

tree<- tree_all[[1]]
species_names<- tree$tip.label
tree$tip.label <- sort(tree$tip.label)
tree$tip.label <- gsub("_", " ", tree$tip.label)

meta_phylo<- unique(metadata_phylo[c("lulc", "scientific_name")])

# Create separate dataframes for each habitat category based on 'lulc' column
jhum_species <- unique(meta_phylo[meta_phylo$lulc == "jhum", "scientific_name", drop = FALSE])
bamboo_species <- unique(meta_phylo[meta_phylo$lulc == "bamboo", "scientific_name", drop = FALSE])
forest_species <- unique(meta_phylo[meta_phylo$lulc == "forest", "scientific_name", drop = FALSE])
abandoned_species <- unique(meta_phylo[meta_phylo$lulc == "abandoned", "scientific_name", drop = FALSE])


match_species_with_habitat <- function(species_names, habitat_category, meta_phylo) {
  # Subset the meta_phylo dataframe for the specified habitat category
  habitat_species <- meta_phylo[meta_phylo$lulc == habitat_category, "scientific_name"]
  
  # Filter species names that are present in the habitat_species
  matched_species <- species_names[species_names %in% habitat_species]
  
  return(matched_species)
}


jhum_matched_species <- match_species_with_habitat(tree$tip.label, "jhum", meta_phylo)
bamboo_matched_species <- match_species_with_habitat(tree$tip.label, "bamboo", meta_phylo)
forest_matched_species <- match_species_with_habitat(tree$tip.label, "forest", meta_phylo)
abandoned_matched_species <- match_species_with_habitat(tree$tip.label, "abandoned", meta_phylo)


# Create separate trees for each habitat category
jhum_tree <- keep.tip(tree, jhum_matched_species)
bamboo_tree <- keep.tip(tree, bamboo_matched_species)
forest_tree <- keep.tip(tree, forest_matched_species)
abandoned_tree <- keep.tip(tree, abandoned_matched_species)

# Plot the trees
plot(jhum_tree, main = "Jhum Habitat Tree")
plot(bamboo_tree, main = "Bamboo Habitat Tree")
plot(forest_tree, main = "Forest Habitat Tree")
plot(abandoned_tree, main = "Abandoned Habitat Tree")



#### pre-winter phylogenetic diversity ####


metadata_phylo_pre<- metadata_phylo %>%
  filter(season== "pre-winter")

metadata_phylo_matrix_pre<- metadata_phylo_pre %>%
  group_by(lulcproxy, scientific_name)%>%
  summarise(n=n())

metadata_phylo_matrix_wide_pre<- metadata_phylo_matrix_pre %>%
  pivot_wider(names_from = scientific_name, values_from = n, values_fill=0) %>%
  column_to_rownames("lulcproxy")

metadata_phylo_matrix_wide_pre <- metadata_phylo_matrix_wide_pre[, order(colnames(metadata_phylo_matrix_wide_pre))]
habitat_pre<- rownames(metadata_phylo_matrix_wide_pre)


# Initialize empty lists to store results
pd_results_pre <- list()
psr_results_pre <- list()

# Loop through each habitat
for (habitat_name in rownames(metadata_phylo_matrix_wide_pre)) {
  # Subset the metadata and phylogenetic matrix for the current habitat
  subset_metadata <- metadata_phylo_matrix_wide_pre[habitat_name, , drop = FALSE]
  
  # Calculate PD for the current habitat and tree
  pd_result_pre <- pd(subset_metadata, tree, include.root = TRUE)
  
  # Calculate PSR for the current habitat and tree
  psr_result_pre <- psr(subset_metadata, tree, compute.var = TRUE, scale.vcv = TRUE)
  
  # Store the results in the respective lists
  pd_results_pre[[habitat_name]] <- pd_result_pre$PD
  psr_results_pre[[habitat_name]] <- psr_result_pre$SR
}

# Calculate the average PD and PSR across trees for each habitat
average_pd_pre <- sapply(habitat_pre, function(habitat_pre) mean(unlist(pd_results_pre[grep(habitat_pre, names(pd_results_pre))])))
average_psr_pre <- sapply(habitat_pre, function(habitat_pre) mean(unlist(psr_results_pre[grep(habitat_pre, names(psr_results_pre))])))

# Combine the results into a data frame
results_df_pre <- data.frame(habitat = habitat_pre, Average_PD = average_pd_pre, Average_PSR = average_psr_pre)

write.csv(results_df_pre, "results_df_pre.csv") # write and export
# Create a data frame to store the averaged results

# Convert row names to a column
#results_df$habitat_names <- rownames(results_df)

## plot PD ##

pd_pre<- read.csv("results_df_pre.csv")
pd_pre$lulc<- factor(pd_pre$lulc, levels=c("forest", "bamboo", "jhum", "abandoned"))


# Plotting
ggplot(pd_pre, aes(x = lulc, y = Average_PD, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Average Phylogenetic Diversity by LULC in pre-winter",
       x = "LULC types",
       y = "Average PD") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


ggplot(pd_pre, aes(x = lulc, y = Average_PSR, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Average Phylogenetic Species Richness by LULC in pre-winter",
       x = "LULC types",
       y = "Average PSR") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


#### post-winter phylogenetic diversity ####


metadata_phylo_post<- metadata_phylo %>%
  filter(season== "post-winter")

metadata_phylo_matrix_post<- metadata_phylo_post %>%
  group_by(lulcproxy, scientific_name)%>%
  summarise(n=n())

metadata_phylo_matrix_wide_post<- metadata_phylo_matrix_post %>%
  pivot_wider(names_from = scientific_name, values_from = n, values_fill=0) %>%
  column_to_rownames("lulcproxy")

metadata_phylo_matrix_wide_post <- metadata_phylo_matrix_wide_post[, order(colnames(metadata_phylo_matrix_wide_post))]
habitat_post<- rownames(metadata_phylo_matrix_wide_post)


# Initialize empty lists to store results
pd_results_post <- list()
psr_results_post <- list()

# Loop through each habitat
for (habitat_name in rownames(metadata_phylo_matrix_wide_post)) {
  # Subset the metadata and phylogenetic matrix for the current habitat
  subset_metadata <- metadata_phylo_matrix_wide_post[habitat_name, , drop = FALSE]
  
  # Calculate PD for the current habitat and tree
  pd_result_post <- pd(subset_metadata, tree, include.root = TRUE)
  
  # Calculate PSR for the current habitat and tree
  psr_result_post <- psr(subset_metadata, tree, compute.var = TRUE, scale.vcv = TRUE)
  
  # Store the results in the respective lists
  pd_results_post[[habitat_name]] <- pd_result_post$PD
  psr_results_post[[habitat_name]] <- psr_result_post$SR
}

# Calculate the average PD and PSR across trees for each habitat
average_pd_post <- sapply(habitat_post, function(habitat_post) mean(unlist(pd_results_post[grep(habitat_post, names(pd_results_post))])))
average_psr_post <- sapply(habitat_post, function(habitat_post) mean(unlist(psr_results_post[grep(habitat_post, names(psr_results_post))])))

# Combine the results into a data frame
results_df_post <- data.frame(habitat = habitat_post, Average_PD = average_pd_post, Average_PSR = average_psr_post)

write.csv(results_df_post, "results_df_post.csv") # write and export
# Create a data frame to store the averaged results

# Convert row names to a column
#results_df$habitat_names <- rownames(results_df)

## plot PD ##

pd_post<- read.csv("results_df_post.csv")
pd_post$lulc<- factor(pd_post$lulc, levels=c("forest", "bamboo", "jhum", "abandoned"))


# Plotting
ggplot(pd_post, aes(x = lulc, y = Average_PD, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Average Phylogenetic Diversity by LULC in post-winter",
       x = "LULC types",
       y = "Average PD") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


ggplot(pd_post, aes(x = lulc, y = Average_PSR, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Average Phylogenetic Species Richness by LULC in post-winter",
       x = "LULC types",
       y = "Average PSR") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))

