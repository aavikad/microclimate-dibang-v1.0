### Aavika Dhanda ##

## dibang community data ##

library(dplyr)
library(tidyverse)
library(ggplot2)

metadata<- read.csv("metadata.csv")
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

#rarefied_richness_coverage_post<- rich_w$iNextEst$coverage_based # To Extract asymptotic richness estimates (order = 0)
#lapply(rich_post_w, function(x) write.table( data.frame(x), 'richness_rarefied.csv'  , append= T, sep=',' )) # to export


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

#### traits###====
library(FD)

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

## PCA on traits by groups ##

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

### functional diversity ### using only trait data and not presence/absence or abundances
### BAMBOO ### ====
## phase 1 pre-winter ###==== using bamboo_1_traits

## functional space ##

bamboo_all<- traits_all %>%
  filter(lulc== "bamboo")
unique_rows_bam <- bamboo_all[!duplicated(bamboo_all$scientific_name), ]
unique_rows_bam_traits<- subset(unique_rows_bam, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_bam_traits)<- unique_rows_bam_traits$scientific_name
colnames(unique_rows_bam_traits) <- gsub("_", " ", colnames(unique_rows_bam_traits))
unique(unique_rows_bam_traits)

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


### ALL BAMBOO

bam_all_matrix <- model.matrix(~ . - 1, data = unique_rows_bam_traits)
bam_all_matrix_dissimilarity<- vegdist(bam_all_matrix, method = "gower")
bam_all_nmds_result <- metaMDS(bam_all_matrix_dissimilarity, k = 2)
bam_all_funtest <- funspace(x = bam_all_nmds_result, threshold = 0.95)
summary(bam_all_funtest)
plot(bam_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo functional space", cex.main = 1.5)

####
bam_1_matrix <- model.matrix(~ . - 1, data = bamboo_1_traits) # for continuous and categorical data
bam_1_matrix_dissimilarity<- vegdist(bam_1_matrix, method = "gower") # Calculate distance matrix using Gower method
bam_1_nmds_result <- metaMDS(bam_1_matrix_dissimilarity, k = 2) # dissimilarity matrix for nmds test
bam_1_funtest <- funspace(x = bam_1_nmds_result, threshold = 0.95) # calculating funspace
summary(bam_1_funtest)
plot(bam_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo pre-winter year 1", cex.main= 1.5)


## phase 2 post-winter ###= using bamboo_2_traits

bam_2_matrix <- model.matrix(~ . - 1, data = bamboo_2_traits)
bam_2_matrix_dissimilarity<- vegdist(bam_2_matrix, method = "gower")
bam_2_nmds_result <- metaMDS(bam_2_matrix_dissimilarity, k = 2)
bam_2_funtest <- funspace(x = bam_2_nmds_result, threshold = 0.95)
summary(bam_2_funtest)
plot(bam_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo post-winter year 1", cex.main = 1.5)


## phase 3 pre-winter ###==== using bamboo_3_traits

bam_3_matrix <- model.matrix(~ . - 1, data = bamboo_3_traits)
bam_3_matrix_dissimilarity<- vegdist(bam_3_matrix, method = "gower")
bam_3_nmds_result <- metaMDS(bam_3_matrix_dissimilarity, k = 2)
bam_3_funtest <- funspace(x = bam_3_nmds_result, threshold = 0.95)
summary(bam_3_funtest)
plot(bam_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Bamboo pre-winter year 2", cex.main = 1.5)


## phase 4 post-winter ###==== using bamboo_4_traits

bam_4_matrix <- model.matrix(~ . - 1, data = bamboo_4_traits)
bam_4_matrix_dissimilarity<- vegdist(bam_4_matrix, method = "gower")
bam_4_nmds_result <- metaMDS(bam_4_matrix_dissimilarity, k = 2)
bam_4_funtest <- funspace(x = bam_4_nmds_result, threshold = 0.95)
summary(bam_4_funtest)
plot(bam_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")


### FOREST ### ====
forest_all<- traits_all %>%
  filter(lulc== "forest")
unique_rows_for <- forest_all[!duplicated(forest_all$scientific_name), ]
unique_rows_for_traits<- subset(unique_rows_for, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_for_traits)<- unique_rows_for_traits$scientific_name
colnames(unique_rows_for_traits) <- gsub("_", " ", colnames(unique_rows_for_traits))
unique(unique_rows_for_traits)


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


### ALL forest

for_all_matrix <- model.matrix(~ . - 1, data = unique_rows_for_traits)
for_all_matrix_dissimilarity<- vegdist(for_all_matrix, method = "gower")
for_all_nmds_result <- metaMDS(for_all_matrix_dissimilarity, k = 2)
for_all_funtest <- funspace(x = for_all_nmds_result, threshold = 0.95)
summary(for_all_funtest)
plot(for_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest functional space", cex.main = 1.5)

## phase 1 pre-winter ### using forest_1_traits

for_1_matrix <- model.matrix(~ . - 1, data = forest_1_traits)
for_1_matrix_dissimilarity<- vegdist(for_1_matrix, method = "gower")
for_1_nmds_result <- metaMDS(for_1_matrix_dissimilarity, k = 2)
for_1_funtest <- funspace(x = for_1_nmds_result, threshold = 0.95)
summary(for_1_funtest)
plot(for_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest pre-winter year 1", cex.main = 1.5)


### phase 2 post-winter ### using forest_2_traits

for_2_matrix <- model.matrix(~ . - 1, data = forest_2_traits)
for_2_matrix_dissimilarity<- vegdist(for_2_matrix, method = "gower")
for_2_nmds_result <- metaMDS(for_2_matrix_dissimilarity, k = 2)
for_2_funtest <- funspace(x = for_2_nmds_result, threshold = 0.95)
summary(for_2_funtest)
plot(for_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest post-winter year 1", cex.main = 1.5)

### phase 3 pre-winter ### using forest_3_traits

for_3_matrix <- model.matrix(~ . - 1, data = forest_3_traits)
for_3_matrix_dissimilarity<- vegdist(for_3_matrix, method = "gower")
for_3_nmds_result <- metaMDS(for_3_matrix_dissimilarity, k = 2)
for_3_funtest <- funspace(x = for_3_nmds_result, threshold = 0.95)
summary(for_3_funtest)
plot(for_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest post-winter year 1", cex.main = 1.5)


### phase 4 post-winter ### using forest_4_traits

for_4_matrix <- model.matrix(~ . - 1, data = forest_4_traits)
for_4_matrix_dissimilarity<- vegdist(for_4_matrix, method = "gower")
for_4_nmds_result <- metaMDS(for_4_matrix_dissimilarity, k = 2)
for_4_funtest <- funspace(x = for_4_nmds_result, threshold = 0.95)
summary(for_4_funtest)
plot(for_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Forest post-winter year 2", cex.main = 1.5)



### JHUM ### ====

jhum_all<- traits_all %>%
  filter(lulc== "jhum")
unique_rows_jhum <- jhum_all[!duplicated(jhum_all$scientific_name), ]
unique_rows_jhum_traits<- subset(unique_rows_jhum, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_jhum_traits)<- unique_rows_jhum_traits$scientific_name
colnames(unique_rows_jhum_traits) <- gsub("_", " ", colnames(unique_rows_jhum_traits))
unique(unique_rows_jhum_traits)

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

### ALL jhum

jhum_all_matrix <- model.matrix(~ . - 1, data = unique_rows_jhum_traits)
jhum_all_matrix_dissimilarity<- vegdist(jhum_all_matrix, method = "gower")
jhum_all_nmds_result <- metaMDS(jhum_all_matrix_dissimilarity, k = 2)
jhum_all_funtest <- funspace(x = jhum_all_nmds_result, threshold = 0.95)
summary(jhum_all_funtest)
plot(jhum_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum functional space", cex.main = 1.5)


## phase 1 pre-winter ### using forest_1_traits

jhum_1_matrix <- model.matrix(~ . - 1, data = jhum_1_traits)
jhum_1_matrix_dissimilarity<- vegdist(jhum_1_matrix, method = "gower")
jhum_1_nmds_result <- metaMDS(jhum_1_matrix_dissimilarity, k = 2)
jhum_1_funtest <- funspace(x = jhum_1_nmds_result, threshold = 0.95)
summary(jhum_1_funtest)
plot(jhum_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum pre-winter year 1", cex.main = 1.5)


### phase 2 post-winter ### using jhum_2_traits

jhum_2_matrix <- model.matrix(~ . - 1, data = jhum_2_traits)
jhum_2_matrix_dissimilarity<- vegdist(jhum_2_matrix, method = "gower")
jhum_2_nmds_result <- metaMDS(jhum_2_matrix_dissimilarity, k = 2)
jhum_2_funtest <- funspace(x = jhum_2_nmds_result, threshold = 0.95)
summary(jhum_2_funtest)
plot(jhum_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum post-winter year 1", cex.main = 1.5)


### phase 3 pre-winter ### using jhum_3_traits

jhum_3_matrix <- model.matrix(~ . - 1, data = jhum_3_traits)
jhum_3_matrix_dissimilarity<- vegdist(jhum_3_matrix, method = "gower") 
jhum_3_nmds_result <- metaMDS(jhum_3_matrix_dissimilarity, k = 2)
jhum_3_funtest <- funspace(x = jhum_3_nmds_result, threshold = 0.95)
summary(jhum_3_funtest)
plot(jhum_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum pre-winter year 2", cex.main = 1.5)


### phase 4 post-winter ### using forest_4_traits

jhum_4_matrix <- model.matrix(~ . - 1, data = jhum_4_traits)
jhum_4_matrix_dissimilarity<- vegdist(jhum_4_matrix, method = "gower")
jhum_4_nmds_result <- metaMDS(jhum_4_matrix_dissimilarity, k = 2)
jhum_4_funtest <- funspace(x = jhum_4_nmds_result, threshold = 0.95)
summary(jhum_4_funtest)
plot(jhum_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Jhum post-winter year 2", cex.main = 1.5)




### ABANDONED ### ====

abn_all<- traits_all %>%
  filter(lulc== "abandoned")
unique_rows_abn <- abn_all[!duplicated(abn_all$scientific_name), ]
unique_rows_abn_traits<- subset(unique_rows_abn, select=c("mass", "PC1_bill", "PC1_loco", "Trophic.Niche", "Primary.Lifestyle"))
rownames(unique_rows_abn_traits)<- unique_rows_abn_traits$scientific_name
colnames(unique_rows_abn_traits) <- gsub("_", " ", colnames(unique_rows_abn_traits))
unique(unique_rows_abn_traits)


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


### ALL abandoned

abn_all_matrix <- model.matrix(~ . - 1, data = unique_rows_abn_traits)
abn_all_matrix_dissimilarity<- vegdist(abn_all_matrix, method = "gower")
abn_all_nmds_result <- metaMDS(abn_all_matrix_dissimilarity, k = 2)
abn_all_funtest <- funspace(x = abn_all_nmds_result, threshold = 0.95)
summary(abn_all_funtest)
plot(abn_all_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum functional space", cex.main = 1.5)

## phase 1 pre-winter ### using abn_1_traits

abn_1_matrix <- model.matrix(~ . - 1, data = abn_1_traits)
abn_1_matrix_dissimilarity<- vegdist(abn_1_matrix, method = "gower")
abn_1_nmds_result <- cmdscale(abn_1_matrix_dissimilarity, k=2)
abn_1_funtest <- funspace(x = abn_1_nmds_result, threshold = 0.95)
summary(abn_1_funtest)
plot(abn_1_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum pre-winter year 1", cex.main = 1.5)


### phase 2 post-winter ### using jhum_2_traits

abn_2_matrix <- model.matrix(~ . - 1, data = abn_2_traits)
abn_2_matrix_dissimilarity<- vegdist(abn_2_matrix, method = "gower")
abn_2_nmds_result <- metaMDS(abn_2_matrix_dissimilarity, k = 2)
abn_2_funtest <- funspace(x = abn_2_nmds_result, threshold = 0.95)
summary(abn_2_funtest)
plot(abn_2_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum post-winter year 1", cex.main = 1.5)


### phase 3 pre-winter ### using jhum_3_traits

abn_3_matrix <- model.matrix(~ . - 1, data = abn_3_traits)
abn_3_matrix_dissimilarity<- vegdist(abn_3_matrix, method = "gower")
abn_3_nmds_result <- metaMDS(abn_3_matrix_dissimilarity, k = 2)
abn_3_funtest <- funspace(x = abn_3_nmds_result, threshold = 0.95)
summary(abn_3_funtest)
plot(abn_3_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum pre-winter year 2", cex.main = 1.5)

### phase 4 post-winter ### using forest_4_traits

abn_4_matrix <- model.matrix(~ . - 1, data = abn_4_traits)
abn_4_matrix_dissimilarity<- vegdist(abn_4_matrix, method = "gower")
abn_4_nmds_result <- metaMDS(abn_4_matrix_dissimilarity, k = 2)
abn_4_funtest <- funspace(x = abn_4_nmds_result, threshold = 0.95)
summary(abn_4_funtest)
plot(abn_4_funtest, axis.title.x = "NMDS1", axis.title.y = "NMDS2")
title(main = "Abandoned jhum post-winter year 2", cex.main = 1.5)


#### Phylogenetic diversity analysis ##### 
library(picante)
library(ape)
library(BiocManager)
library(ggtree)
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

#needed.libraries <- required.libraries[!(required.libraries %in% installed.packages()[,"Package"])]
#if(length(needed.libraries)) install.packages(needed.libraries)

##### phylo ####

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

#write.csv(results_df_pre, "results_df_pre.csv") # write and export
# Create a data frame to store the averaged results

# Convert row names to a column
#results_df$habitat_names <- rownames(results_df)

## plot PD ##

pd_pre<- pd %>%
  filter(season== "pre-winter")

pd_pre$lulc<- factor(pd_pre$lulc, levels=c("forest", "bamboo", "jhum", "abandoned"))


# Plotting seasonal 
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

#write.csv(results_df_post, "results_df_post.csv") # write and export
# Create a data frame to store the averaged results

# Convert row names to a column
#results_df$habitat_names <- rownames(results_df)

## plot PD ##

pd_post<- pd %>%
  filter(season =="post-winter")
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

##### GLMMS using rarefied richness ####
library(Matrix)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(AICcmodavg)

richness<- read.csv("rarefied_rich_seasons.csv")
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
                         data=richness) # full model (mean temp + season + lulc)
summary(model1)
AIC(model1)
res1<- residuals(model1)
qqnorm(res1)
qqline(res1)

residuals1 <- simulateResiduals(model1) #null hypothesis is that there is no overdispersion
plot(residuals1)
testDispersion(residuals1) #dispersion = 0.91977, p-value = 0.76

model2<- lmerTest::lmer(coverage_based.qD ~ mean_temp+ lulc + (1|site),
                        data=richness) # mean temp + lulc
summary(model2)
AIC(model2)


model3<- lmerTest::lmer(coverage_based.qD ~ mean_temp+ season + (1|site),
                        data=richness) # mean temp + season
summary(model3)
AIC(model3)


model_set1 <- list(mod0, model1, model2, model3)
mod.names1<- c('null', 'temp+season+lulc','mean_temp+ lulc', 'mean_temp+season')
aictab(cand.set = model_set1, modnames = mod.names1) # full model performs the best


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


### mean RH ##

model1a<- lmerTest::lmer(coverage_based.qD ~ mean_rh + season +lulc + (1|site),
                        data=richness) # full model (mean rh + season + lulc)
summary(model1a)
AIC(model1a)
res1a<- residuals(model1a)
qqnorm(res1a)
qqline(res1a)

residuals1a <- simulateResiduals(model1a) #null hypothesis is that there is no overdispersion
plot(residuals1a)
testDispersion(residuals1a) #dispersion = 0.91848, p-value = 0.76


model2a<- lmerTest::lmer(coverage_based.qD ~ mean_rh+ season + (1|site),
                        data=richness) # mean rh + season
summary(model2a)
AIC(model2a)

model3a<- lmerTest::lmer(coverage_based.qD ~ mean_rh+ lulc + (1|site),
                        data=richness) # mean rh + lulc
summary(model3a)
AIC(model3a)

model_set2 <- list(mod0, model1a, model2a, model3a)
mod.names2<- c('null', 'rh+season+lulc','mean_rh+ season', 'mean_rh+lulc')
aictab(cand.set = model_set2, modnames = mod.names2) # full model performs the best


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
library("optimx")

phylo<- read.csv("results_pd.csv")
phylo$lulc<- factor(phylo$lulc)
phylo$lulc<- relevel(phylo$lulc, ref = "forest")
phylo$season<- factor(phylo$season)
phylo$season<- relevel(phylo$season, ref = "pre-winter")

## null model ## PD

model_0<- glmmTMB(log(Average_PD) ~ 1+ (1|year), data = phylo)
summary(model_0)


model4<- glmmTMB(log(Average_PD) ~  mean_temp + (1|year),
                  data=phylo,
                  family = gaussian) # mean temp
summary(model4)
res4<- residuals(model4)
qqnorm(res4)
qqline(res4)

residuals4 <- simulateResiduals(model4) #null hypothesis is that there is no overdispersion
plot(residuals4)
testDispersion(residuals4) #dispersion = 1.0808, p-value = 0.776


model5<- glmmTMB(log(Average_PD) ~ mean_rh + (1|year),
                  data=phylo,
                  family = gaussian) # rh
summary(model5)


model6<- glmmTMB(log(Average_PD) ~ season + (1|year),
                  data=phylo,
                  family = gaussian) # season
summary(model6)

model7<- glmmTMB(log(Average_PD) ~ lulc + (1|year),
                  data=phylo,
                  family = gaussian) # lulc
summary(model7)

model_set3 <- list(model_0, model4, model5, model6, model7)
mod.names3<- c('null', 'temp', 'rh', 'season', 'lulc')
aictab(cand.set = model_set3, modnames = mod.names3) # temp model performs the best


## null model ## PSR

mod_PSR_0<- glmmTMB(Average_PSR ~ 1 + (1|year), 
                    data=phylo,
                    family = poisson())

summary(mod_PSR_0)


model4a<- glmmTMB(Average_PSR ~  mean_temp + (1|year),
                  data=phylo,
                  family = poisson()) # mean temp
summary(model4a)

model5a<- glmmTMB(Average_PSR ~ mean_rh + (1|year),
                  data=phylo,
                  family = poisson()) # lulc
summary(model5a)


model6a<- glmmTMB(Average_PSR ~ season + (1|year),
                  data=phylo,
                  family = poisson()) # season
summary(model6a)

model7a<- glmmTMB(Average_PSR ~ lulc + (1|year),
                  data=phylo,
                  family = poisson()) # lulc
summary(model7a)
res7a<- residuals(model7a)
qqnorm(res7a)
qqline(res7a)

residuals7a <- simulateResiduals(model7a) #null hypothesis is that there is no overdispersion
plot(residuals7a)
testDispersion(residuals7a) #dispersion = 0.40449, p-value = 0.064


model_set4 <- list(mod_PSR_0, model4a, model5a, model6a, model7a)
mod.names4<- c('null', 'temp', 'rh', 'season', 'lulc')
aictab(cand.set = model_set4, modnames = mod.names4) # model with lulc is the best performing


### GLMMS using FRich####
library(optimx)

fun<- read.csv("funspace_fd.csv")
fun$lulc<- factor(fun$lulc)
fun$lulc<- relevel(fun$lulc, ref= "forest")
fun$season<- factor(fun$season)
fun$season<- relevel(fun$season, ref= "pre-winter")

## null model 
mod_frich0<- glmmTMB(FRich1 ~ 1+ (1|year), data= fun)

model8<- glmmTMB(FRich1 ~  mean_temp + (1|year), data= fun) # mean temp
summary(model8)

model9<- glmmTMB(FRich1 ~ mean_rh + (1|year), data= fun) # mean rh
summary(model9)

res9<- residuals(model9)
qqnorm(res9)
qqline(res9)

residuals9 <- simulateResiduals(model9) #null hypothesis is that there is no overdispersion
plot(residuals9)
testDispersion(residuals9) #dispersion = 1.0808, p-value = 0.776


model10<- glmmTMB(FRich1 ~ season + (1|year), data= fun) # season
summary(model10)

model11<- glmmTMB(FRich1 ~ lulc + (1|year), data= fun) # lulc
summary(model11)

model_set5 <- list(mod_frich0, model8, model9, model10, model11)
mod.names5<- c('null', 'mean_temp', 'mean_rh', 'season', 'lulc')
aictab(cand.set = model_set5, modnames = mod.names5) # model with mean rh performs the best


# Plotting
ggplot(fun, aes(x = lulc, y = FRich1, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Functional richness by LULC",
       x = "LULC types",
       y = "Functional richness") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))

fun_pre<- fun %>%
  filter(season== "pre-winter")


# Plotting
ggplot(fun_pre, aes(x = lulc, y = FRich1, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Functional richness by LULC in pre-winter",
       x = "LULC types",
       y = "Functional richness") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


fun_post<- fun %>%
  filter(season== "post-winter")


# Plotting
ggplot(fun_post, aes(x = lulc, y = FRich1, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Functional richness by LULC in post-winter",
       x = "LULC types",
       y = "Functional richness") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


### GLMMS using FDiv####

### null model ##
mod_fdiv0<- glmmTMB(log(FDiv1) ~ 1 + (1|year), data= fun)
summary(mod_fdiv0)

model8a<- glmmTMB(log(FDiv1)  ~  mean_temp + (1|year), data= fun) # mean temp
summary(model8a)

model9a<- glmmTMB(log(FDiv1) ~ mean_rh + (1 | year), data = fun) # mean rh
summary(model9a)


model10a<- glmmTMB(log(FDiv1)  ~ season + (1|year), data= fun) # season
summary(model10a)

model11a<- glmmTMB(log(FDiv1)  ~ lulc + (1|year), data= fun) # lulc
summary(model11a)

model_set6 <- list(mod_fdiv0, model9a, model9b, model9c, model9d)
mod.names6<- c('null', 'mean_temp', 'mean_rh', 'season', 'lulc')
aictab(cand.set = model_set6, modnames = mod.names6) # none of the models performed better than the null model


ggplot(fun, aes(x = lulc, y = FDiv1, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Functional diversity by LULC",
       x = "LULC types",
       y = "Functional diversity") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


# Plotting
ggplot(fun_pre, aes(x = lulc, y = FDiv1, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Functional diversity by LULC in pre-winter",
       x = "LULC types",
       y = "Functional diversity") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))


# Plotting
ggplot(fun_post, aes(x = lulc, y = FDiv1, fill = lulc)) +
  geom_boxplot() +
  labs(title = "Functional diversity by LULC in post-winter",
       x = "LULC types",
       y = "Functional diversity") +
  scale_fill_manual(values = c("#E9967A", "#B0E57C", "#87CEEB", "#B19CD9")) +
  theme_minimal() +
  theme(axis.text= element_text(size = 16), 
        axis.title= element_text(size = 16),
        plot.title= element_text(size=16))

