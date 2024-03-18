
######################################################################################################

###############    Phytosanitary threats of non-native crickets under temperature change    ##########

######################################################################################################



df = read_xlsx("FR with both AG.xlsx")
str(df)

df$`#Prey eaten` = as.numeric(df$`#Prey eaten`)
df$`#Prey [bitten]` = as.numeric(df$`#Prey [bitten]`)
df$`#Prey` = as.numeric(df$`#Prey`)

df$Total_eaten = df$`#Prey eaten` + df$`#Prey [bitten]`

names(df)

df= df[,-c(10:11)]


suppressMessages({
  library(dplyr, quiet = TRUE, warn.conflicts = FALSE)
  library(reshape, quiet = TRUE, warn.conflicts = FALSE)
  library(ggplot2)
  library(stringr)
  library(tidyr)  
  library(stringr)
  library(readxl)
  library(frair)
  library(patchwork)
  library(glmmTMB)
})

str(df)

df$Species <- as.factor(df$Species)
df$Sex <- as.factor(df$Sex)
df$Temperature <- as.factor(df$Temperature)
df$Seed <- as.factor(df$Seed)


df$Temperature <- as.factor(gsub("°C", "", df$Temperature))
df<- df %>% filter(!Seed =="control")

df$Damaged_Seeds <- (df$`#Prey eaten` + df$`#Prey [bitten]`)
df$Undamaged_Seeds <- df$`#Prey` - df$Damaged_Seeds

df$Species <- as.character(df$Species)
df <- df %>% filter(!Species =="Acheta & Gryllus")
df$Species <- as.factor(df$Species)

response_matrix <- cbind(df$Damaged_Seeds, df$Undamaged_Seeds)


df_clean <- df %>% mutate(Proportion_eaten = Total_eaten / `#Prey`)
max(df_clean$Proportion_eaten)

unique(df$Species)
levels(df$Species)

# Fit the GLM model ( I need to run a GLM for each type of consumption)
model <- glm(response_matrix  ~ Species * Temperature + `#Prey`, family = binomial, data = df)

summary(model)

#Coefficients:
#(Intercept)                                 4.229766   0.220870  19.150  < 2e-16 ***
#  SpeciesAcheta domesticus                   -1.639414   0.239541  -6.844  7.7e-12 ***
#  SpeciesGryllus bimaculatus                 -1.945660   0.182806 -10.643  < 2e-16 ***
#  Temperature25°C                             1.256966   0.332844   3.776 0.000159 ***
#  Temperature30°C                             0.852228   0.288074   2.958 0.003093 ** 
#  `#Prey`                                    -0.045059   0.004411 -10.214  < 2e-16 ***
#  SpeciesAcheta domesticus:Temperature25°C    0.616873   0.513896   1.200 0.229990    
#SpeciesGryllus bimaculatus:Temperature25°C -0.769969   0.357120  -2.156 0.031080 *  
#  SpeciesAcheta domesticus:Temperature30°C    1.306408   0.527238   2.478 0.013218 *  
#  SpeciesGryllus bimaculatus:Temperature30°C  0.085804   0.321066   0.267 0.789279  


response_matrix <- cbind(df$`#Prey eaten`, df$Undamaged_Seeds)
model2 <- glm(response_matrix  ~ Species * Temperature + `#Prey`, family = binomial, data = df)
summary(model2)
#Estimate Std. Error z value             Pr(>|z|)    
#(Intercept)                                 4.463161   0.245376  18.189  < 2e-16 ***
#  SpeciesAcheta domesticus                   -2.523421   0.271503  -9.294  < 2e-16 ***
#  SpeciesGryllus bimaculatus                 -2.260803   0.213734 -10.578  < 2e-16 ***
#  Temperature25°C                             1.526458   0.344414   4.432 9.33e-06 ***
#  Temperature30°C                             1.345185   0.299892   4.486 7.27e-06 ***
#  `#Prey`                                    -0.084182   0.005081 -16.567  < 2e-16 ***
#  SpeciesAcheta domesticus:Temperature25°C    0.554218   0.534878   1.036   0.3001    
#SpeciesGryllus bimaculatus:Temperature25°C -0.884222   0.383130  -2.308   0.0210 *  
#  SpeciesAcheta domesticus:Temperature30°C    1.234805   0.545389   2.264   0.0236 *  
#  SpeciesGryllus bimaculatus:Temperature30°C  0.224588   0.346565   0.648   0.5170  
  
  
  
response_matrix <- cbind(df$`#Prey [bitten]`, df$Undamaged_Seeds)
model3 <- glm(response_matrix  ~ Species * Temperature + `#Prey`, family = binomial, data = df)
summary(model3)
#Coefficients:
#(Intercept)                                 2.60968    0.24163  10.801  < 2e-16 ***
#  SpeciesAcheta domesticus                   -0.91365    0.24870  -3.674 0.000239 ***
#  SpeciesGryllus bimaculatus                 -1.76038    0.18801  -9.363  < 2e-16 ***
#  Temperature25°C                             1.04509    0.33675   3.103 0.001913 ** 
#  Temperature30°C                             0.45804    0.29378   1.559 0.118970    
#`#Prey`                                    -0.01210    0.00508  -2.382 0.017198 *  
#  SpeciesAcheta domesticus:Temperature25°C    0.67641    0.51963   1.302 0.193019    
#SpeciesGryllus bimaculatus:Temperature25°C -0.75617    0.36337  -2.081 0.037436 *  
#  SpeciesAcheta domesticus:Temperature30°C    1.38327    0.53435   2.589 0.009634 ** 
#  SpeciesGryllus bimaculatus:Temperature30°C  0.09222    0.33001   0.279 0.779916   
  

anova <- car::Anova(model, test = "LR")
anova2 <- car::Anova(model2, test = "LR")
anova3 <- car::Anova(model3, test = "LR")



emm <- emmeans(model3, ~ Species | Temperature)
emm_interaction <- pairs(emm, simple = "each")
summary(emm_interaction, adjust = "tukey")



emm_plot <- emmip(model, Species ~ Temperature, CIs = TRUE)
emm_plot + 
  theme_bw() + xlab("Temperature (°C)")+ ylab("Linear prediction") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), # Customize x-axis text
        legend.position = "top") + 
  scale_color_manual(values = c("Acheta domesticus" = "green", "Gryllus bimaculatus" = "purple")) # Customize colors




##############    Mortality plot      ##########

df1 <- read_xlsx("Mortality data (G&A)(2).xlsx")

library(dplyr)
library(pscl)

df1$Species <- as.factor(df1$Species)
df1$Temp <- as.factor(gsub("°C", "", df1$Temp))

extract_counts <- function(str) {
  counts <- as.numeric(gsub("\"|/.*", "", str))
  replicates <- as.numeric(gsub(".*?/", "", gsub("\"", "", str)))
  return(list(counts = counts, replicates = replicates))
}
df1 <- df1 %>%
  mutate(Dead_Acheta = sapply(`Dead Acheta/Replicates`, function(x) extract_counts(x)$counts),
         Replicates_Acheta = sapply(`Dead Acheta/Replicates`, function(x) extract_counts(x)$replicates),
         Dead_Gryllus = sapply(`Dead Gryllus/Replicates`, function(x) extract_counts(x)$counts),
         Replicates_Gryllus = sapply(`Dead Gryllus/Replicates`, function(x) extract_counts(x)$replicates))

df1 <- df1 %>%
  mutate(Mortality_Acheta = Dead_Acheta / Replicates_Acheta,
         Mortality_Gryllus = Dead_Gryllus / Replicates_Gryllus)

mortality_summary <- df1 %>%
  group_by(Species, Temp) %>%
  summarize(Mean_Mortality = mean(Mortality_Gryllus, na.rm = TRUE),
            SE = sd(Mortality_Gryllus, na.rm = TRUE) / sqrt(n())) %>%
  ungroup()

ggplot(mortality_summary, aes(x = Species, y = Mean_Mortality, fill = Temp)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black", size =0.2) +
  geom_errorbar(aes(ymin = Mean_Mortality - SE, ymax = Mean_Mortality + SE), 
                position = position_dodge(0.9), width = 0.25) +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic")) +
  labs(x = "Species",
       y = "Mean mortality rate (± SE)",
       fill = "Temperature") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text = element_text(size=12, color="black"),
        axis.title = element_text(size=14, color="black"))


### Binomial models 
str(df1)

df1$Species <- as.character(df1$Species)
df1$Temp <- as.numeric(as.character(df1$Temp)) 

acheta_rows <- df1 %>% 
  filter(Species == "Acheta & Gryllus") %>%
  mutate(Species = "Acheta domesticus")
acheta_rows= acheta_rows[,c(1:3,6,7,10)]
colnames(acheta_rows) = c("Species","Temp","Prey","Dead","Replicates","Mortality")
acheta_rows$ID =1:nrow(acheta_rows)
  
gryllus_rows <- df1 %>%
  filter(Species == "Acheta & Gryllus") %>%
  mutate(Species = "Gryllus bimaculatus")
gryllus_rows= gryllus_rows[,c(1:3,8,9,11)]
colnames(gryllus_rows) = c("Species","Temp","Prey","Dead","Replicates","Mortality")
gryllus_rows$ID =1:nrow(gryllus_rows)


single_sp = df1 %>%  filter(Species == "Acheta domesticus")
single_sp= single_sp[,c(1:3,6,7,10)]
colnames(single_sp) = c("Species","Temp","Prey","Dead","Replicates","Mortality")
single_sp$ID =NA

single_sp1 = df1 %>%  filter(Species == "Gryllus bimaculatus")
single_sp1 = single_sp1[,c(1:3,8,9,11)]
colnames(single_sp1) = c("Species","Temp","Prey","Dead","Replicates","Mortality")
single_sp1$ID =NA

df_model= rbind(acheta_rows,gryllus_rows,single_sp1,single_sp )
df_model <- df_model[c(31:60),]
df_model <- df_model[c(1:30),]

# Model 1
levels(df1$Prey)

df_model$Species <- as.factor(df_model$Species)

levels(df_model$Species)
glm_1 <- glm(cbind(Dead, Replicates - Dead) ~ Temp + Prey +Species+  Temp:Species, 
                  family = binomial, data = df_model)

summary(glm_1)

# Model 2 (here I use the full database)
library(lmerTest)
df_model$ID <- as.factor(df_model$ID)

glmm_mixed <- glmer(cbind(Dead, Replicates - Dead) ~ Species + Temp + Prey +Temp:Species +
                      (1|ID), 
                    family = binomial, data = df_model)

summary(glmm_mixed)





##############    Figure 1:      ##########

str(df)
df_summary <- df %>%
  mutate(Total_Prey = `#Prey eaten` + `#Prey [bitten]` + as.numeric(`#Prey left`)) %>%
  group_by(Species, Temperature) %>%
  summarize(
    Proportion_Bitten = mean(`#Prey [bitten]` / Total_Prey),
    SE_Bitten = sd(`#Prey [bitten]` / Total_Prey) / sqrt(n()),
    Proportion_Eaten = mean(`#Prey eaten` / Total_Prey),
    SE_Eaten = sd(`#Prey eaten` / Total_Prey) / sqrt(n()),
    Proportion_Total_Eaten = mean((`#Prey eaten` + `#Prey [bitten]`) / Total_Prey),
    SE_Total_Eaten = sd((`#Prey eaten` + `#Prey [bitten]`) / Total_Prey) / sqrt(n()),
    .groups = 'drop')

df_summary <- df_summary %>% as.data.frame()
# Reshape data for plotting
df_long <- df_summary %>%
  pivot_longer(
    cols = starts_with("Proportion_"),
    names_to = "Consumption_Type",
    values_to = "Proportion"
  ) %>%
  pivot_longer(
    cols = starts_with("SE_"),
    names_to = "SE_Type",
    values_to = "SE"
  ) %>%
  mutate(Consumption_Type = sub("Proportion_", "", Consumption_Type),
         SE_Type = sub("SE_", "", SE_Type),
         Consumption_Type = factor(Consumption_Type, levels = c("Bitten", "Eaten", "Total_Eaten"))) %>%
  filter(Consumption_Type == SE_Type) %>%
  dplyr::select(-SE_Type)


unique(df_long$Consumption_Type)
df_long$Consumption_Type <- as.character(df_long$Consumption_Type)
df_long$Consumption_Type[df_long$Consumption_Type =="Total_Eaten"] <- "Total"

ggplot(df_long, aes(x = Species, y = Proportion, fill = Temperature)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), position = position_dodge(width = 0.7), width = 0.25) +
  facet_wrap(~Consumption_Type, scales = 'fixed', ncol = 3) +  
  scale_fill_brewer(palette = "Set1", name = "Temperature") +
  labs(x = "Species", y = "Proportion consumed", fill = "Temperature") +
  theme_bw() + xlab("") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black", face = "italic"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    legend.position = "top",
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(reverse = FALSE))




##############    Functional response frair      ##########

library(purrr)

results <- df %>%
  group_by(Species, Temperature, Seed, Sex) %>%
  nest() %>%
  mutate(test = map(data, ~ frair_test(Total_eaten ~ `#Prey`, data = .x)))


results$test[[2]]
results$test[[3]]

df2 <- df %>% filter(Species=="Acheta domesticus" & Temperature=="25°C") 
df3 <- df %>% filter(Species=="Acheta domesticus" & Temperature=="30°C") 

frair_responses(show=T) 
type_II_model2 <- frair_fit(formula = Total_eaten ~ `#Prey`, data = df2,
                           response = "rogersII",
                           start = list(a = 1, h = 0.1),
                           fixed = list(T = 1)) 

type_III_model <- frair_fit(Total_eaten ~ `#Prey`, data = df3, response = "flexpnr",
                            start = list(h = 0.1, b = 0.5, q = 1, T = 1))

start_values <- list(
  h = 0.1,  # Handling time
  b = 0.5,  # Parameter related to the shape of the response curve
  q = 1,  # Another shape parameter, often related to the inflection point of the curve
  T = 1 )
flexible_model <- frair_fit(Total_eaten ~ `#Prey`, data = df3, response = "flexpnr",start = start_values)

aic_values <- c(type_II = AIC(type_II_model$fit), Type_III = AIC(type_III_model$fit),
                Flexible = AIC(flexible_model$fit))
best_model_name <- names(aic_values)[which.min(aic_values)]


setwd("C:/Users/Propietario/Desktop/grillus")
df = read_xlsx('FR with both A&G.xlsx')
head(df)
str(df)


df$`#Prey eaten` = as.numeric(df$`#Prey eaten`)
df$`#Prey [bitten]` = as.numeric(df$`#Prey [bitten]`)
df$`#Prey` = as.numeric(df$`#Prey`)

df$Total_eaten = df$`#Prey eaten` + df$`#Prey [bitten]`

names(df)

df= df[,-c(7,8,9,10,11)]

df= df %>% filter(!Seed== 'control')
df= df %>% filter(!Species== 'Acheta & Gryllus')


df$Species <- as.factor(df$Species)
df$Sex <- as.factor(df$Sex)
df$Temperature <- as.factor(df$Temperature)
df$Seed <- as.factor(df$Seed)



df %>%
  summarise_at(vars(Total_eaten, `#Prey`), 
               list(~sum(is.na(.))))

df_clean <- df %>%
  filter(!is.na(Total_eaten))

str(df_clean)

df_clean <- df_clean %>%
  mutate(Proportion_eaten = Total_eaten / `#Prey`)


results <- df_clean %>%
  group_by(Species, Temperature, Seed) %>%
  nest() %>%
  mutate(test = map(data, ~ frair_test(Total_eaten ~ `#Prey`, data = .x))) 


res_list <- list()

for (i in 1:nrow(results)) {
  results1 = results[i,]
  results2 = results1$data %>% as.data.frame()
  
  z =  frair_fit(formula =  Total_eaten ~ X.Prey, data = results2,
                 response = "rogersII",
                 start = list(a = 1, h = 0.1),
                 fixed = list(T = 1)) 
  
  assign(paste("Fit", i, sep="_"), z)
  
  res = z[["coefficients"]] %>% as.data.frame()
  
  res$FitID <- paste("Fit", i, sep="_")
  
  # Add the additional information
  res$Species <- results1$Species[1]    
  res$Temperature <- results1$Temperature[1]    
  res$Seed <- results1$Seed[1]   
  res$Terms = c('a', 'h','T')
  
  res_list[[i]] <- res
}




result_list <- list()

for(i in 1:6) {
  
  fit <- get(paste("Fit", i, sep="_"))
  
  
  result <- frair_boot(fit)
  confint(result , citypes ='bca')
  
  assign(paste("Fit_boot", i, sep="_"), result)
  
  
  result_list[[i]] <- result
  cat('We hate Paride ==', i, '\n')
}



plot(x = 1,
     type = "n",
     xlim = c(0, 40), 
     ylim = c(0, 40),
     font.lab = 2,  
     mgp = c(2.5,1,0),
     cex.axis = 1.5,  
     xaxt = "n",
     cex.lab = 1.5,
     xlab="Prey Density", ylab="No. of seed eaten")
axis(1, at = c(0,  5, 10,20, 40), cex.axis = 1.5)

color1 <- col2rgb("red")
color2 <- col2rgb("orange")
color3 <- col2rgb("yellow")

# Create new colors with alpha
color_alpha1 <- rgb(color1[1]/255, color1[2]/255, color1[3]/255, alpha=0.5)
color_alpha2 <- rgb(color2[1]/255, color2[2]/255, color2[3]/255, alpha=0.5)
color_alpha3 <- rgb(color3[1]/255, color3[2]/255, color3[3]/255, alpha=0.5)


drawpoly(Fit_boot_1, col=color_alpha3, border=NA, tozero=TRUE)
drawpoly(Fit_boot_2, col=color_alpha2, border=NA, tozero=TRUE)
drawpoly(Fit_boot_3, col=color_alpha1, border=NA, tozero=TRUE)


lines(Fit_boot_1, col = color_alpha3, lty=1, lwd=3) 
lines( Fit_boot_2, col = color_alpha2,lty=2,  lwd=3 ) 
lines(Fit_boot_3, col = color_alpha1 ,lty=3,  lwd=3 ) 


text(x = max(par("usr")[1:2]), y = max(par("usr")[3:4]), 
     labels = "Acheta & Gryllus", adj = c(1, 1), cex = 1.5, font = 3)


a <- df_clean %>% 
  filter(Species == "Acheta & Gryllus", Sex=='f',Temperature == '20°C', Seed == "millet")

points(x = a$`#Prey`, y = a$`#Prey eaten`, pch="*", cex=0.9, col = 'yellow')

b <- df_clean %>% 
  filter(Species == "Acheta domesticus", Sex=='f',Temperature == '25°C', Seed == "millet")

points(x = b$`#Prey`, y = b$`#Prey eaten`, pch="x", cex=0.9, col = 'orange')

c <- df_clean %>% 
  filter(Species == "Acheta domesticus", Sex=='f',Temperature == '30°C', Seed == "millet")

points(x = c$`#Prey`, y = c$`#Prey eaten`, pch="+", cex=0.9, col = 'red')

legend(0, 40, legend=c("20°C", "25°C","30°C"),
       col=c('yellow', 'orange','red'), lty=1:3, cex=1.2, lwd=2)







plot(x = 1,
     type = "n",
     xlim = c(0, 40), 
     ylim = c(0, 40),
     font.lab = 2,  
     mgp = c(2.5,1,0),
     cex.axis = 1.5,  
     xaxt = "n",
     cex.lab = 1.5,
     xlab="Prey Density", ylab="No. of seed eaten")
axis(1, at = c(0,  5, 10,20,40), cex.axis = 1.5)



drawpoly(Fit_boot_4, col=color_alpha3, border=NA, tozero=TRUE)
#Coefficients (original data):
#  a     h     T 
#1.166 0.134 1.000 
drawpoly(Fit_boot_5, col=color_alpha2, border=NA, tozero=TRUE)
#Coefficients (original data):
#  a     h     T 
#3.173 0.090 1.000 
drawpoly(Fit_boot_6, col=color_alpha1, border=NA, tozero=TRUE)
#Coefficients (original data):
#  a     h     T 
#2.588 0.030 1.000

lines(Fit_boot_4, col = color_alpha3, lty=1, lwd=3) 
lines( Fit_boot_5, col = color_alpha2,lty=2,  lwd=3 ) 
lines(Fit_boot_6, col = color_alpha1 ,lty=3,  lwd=3 ) 


text(x = max(par("usr")[1:2]), y = max(par("usr")[3:4]), 
     labels = "Acheta domesticus", adj = c(1, 1), cex = 1.5, font = 3)

text(x = 17, y = 15, labels = "Temperature: 20°C", adj = c(0, 1), cex = 0.8, font = 2)
text(x = 17, y = 18, labels = "Temperature: 25°C", adj = c(0, 1), cex = 0.8, font = 2)
text(x = 17, y = 19, labels = "Temperature: 30°C", adj = c(0, 1), cex = 0.8, font = 2)


a <- df_clean %>% 
  filter(Species == "Acheta domesticus", Sex=='f',Temperature == '20°C', Seed == "millet")

points(x = a$`#Prey`, y = a$`#Prey eaten`, pch="*", cex=0.9, col = 'yellow')

b <- df_clean %>% 
  filter(Species == "Acheta domesticus", Sex=='f',Temperature == '25°C', Seed == "millet")

points(x = b$`#Prey`, y = b$`#Prey eaten`, pch="x", cex=0.9, col = 'orange')

c <- df_clean %>% 
  filter(Species == "Acheta domesticus", Sex=='f',Temperature == '30°C', Seed == "millet")

points(x = c$`#Prey`, y = c$`#Prey eaten`, pch="+", cex=0.9, col = 'red')

legend(0, 40, legend=c("Temperature: 20°C", "Temperature: 25°C","Temperature: 30°C"),
       col=c('yellow', 'orange','red'), lty=1:3, cex=1.2, lwd=2)






plot(x = 1,
     type = "n",
     xlim = c(0, 40), 
     ylim = c(0, 40),
     font.lab = 2,  
     mgp = c(2.5,1,0),
     cex.axis = 1.5,  
     xaxt = "n",
     cex.lab = 1.5,
     xlab="Prey Density", ylab="No. of seed eaten")
axis(1, at = c(0,  5, 10,20,40), cex.axis = 1.5)



drawpoly(Fit_boot_7, col=color_alpha3, border=NA, tozero=TRUE)
#Coefficients (original data):
#  a     h     T 
#1.166 0.134 1.000 
drawpoly(Fit_boot_8, col=color_alpha2, border=NA, tozero=TRUE)
#Coefficients (original data):
#  a     h     T 
#3.173 0.090 1.000 
drawpoly(Fit_boot_9, col=color_alpha1, border=NA, tozero=TRUE)
#Coefficients (original data):
#  a     h     T 
#2.588 0.030 1.000

lines(Fit_boot_7, col = color_alpha3, lty=1, lwd=3) 
lines( Fit_boot_8, col = color_alpha2,lty=2,  lwd=3 ) 
lines(Fit_boot_9, col = color_alpha1 ,lty=3,  lwd=3 ) 


text(x = max(par("usr")[1:2]), y = max(par("usr")[3:4]), 
     labels = "Grillus", adj = c(1, 1), cex = 1.5, font = 3)

text(x = 17, y = 15, labels = "Temperature: 20°C", adj = c(0, 1), cex = 0.8, font = 2)
text(x = 17, y = 18, labels = "Temperature: 25°C", adj = c(0, 1), cex = 0.8, font = 2)
text(x = 17, y = 19, labels = "Temperature: 30°C", adj = c(0, 1), cex = 0.8, font = 2)


a <- df_clean %>% 
  filter(Species == "Gryllus bimaculatus", Sex=='f',Temperature == '20°C', Seed == "millet")

points(x = a$`#Prey`, y = a$`#Prey eaten`, pch="*", cex=0.9, col = 'yellow')

b <- df_clean %>% 
  filter(Species == "Gryllus bimaculatus", Sex=='f',Temperature == '25°C', Seed == "millet")

points(x = b$`#Prey`, y = b$`#Prey eaten`, pch="x", cex=0.9, col = 'orange')

c <- df_clean %>% 
  filter(Species == "Gryllus bimaculatus", Sex=='f',Temperature == '30°C', Seed == "millet")

points(x = c$`#Prey`, y = c$`#Prey eaten`, pch="+", cex=0.9, col = 'red')

legend(0, 20, legend=c("Temperature: 20°C", "Temperature: 25°C","Temperature: 30°C"),
       col=c('yellow', 'orange','red'), lty=1:3, cex=1.2, lwd=2)






##############    Non-trophic interaction     ##########

rm(list=ls())

library(frair)
library(readxl)
library(plyr)
library(MuMIn)
library(emmeans)
library(car)
library(bbmle)
library(emdbook)
library(dataframes2xls)
library(AICcmodavg) 
library(ggplot2)
library(reshape2)
library(deSolve) 
library(FME)

params<-read.csv("params.csv")
params<-read.csv2("Ross_claws.csv")
params<-read_xlsx("params_orthop.xlsx")

# you should alter this file with the parameters for each claw treatment
# you could just run to the code three times, with all pairwise combinations of the three groups
# then you have to change less below (but just repeat the scripts over and over)

params$Std_Error <-as.numeric(params$Std_Error)
params$CI<-(params$Std..Error*1.96)

n<-params
n$Estimate <- as.numeric(n$Estimate)
n$CI<- as.numeric(n$CI)

Pars16 <- c(
  nilea = n$Estimate[n$Predator == "Acheta_30" & n$parameters == "a"],
  nileh = n$Estimate[n$Predator == "Acheta_30" & n$parameters == "h"],
  omoa = n$Estimate[n$Predator == "Grillus_30" & n$parameters == "a"],
  omoh = n$Estimate[n$Predator == "Grillus_30" & n$parameters == "h"]
)  

nilea_indices <- which(n$Predator == "Acheta_30" & n$parameters == "a")
nileh_indices <- which(n$Predator == "Acheta_30" & n$parameters == "h")
omoa_indices <- which(n$Predator == "Grillus_30" & n$parameters == "a")
omoh_indices <- which(n$Predator == "Grillus_30" & n$parameters == "h")

parRanges16_IC <- data.frame(
  min = c(
    n$Estimate[nilea_indices] - n$CI[nilea_indices],
    n$Estimate[nileh_indices] - n$CI[nileh_indices],
    n$Estimate[omoa_indices] - n$CI[omoa_indices],
    n$Estimate[omoh_indices] - n$CI[omoh_indices]
  ),
  max = c(
    n$Estimate[nilea_indices] + n$CI[nilea_indices],
    n$Estimate[nileh_indices] + n$CI[nileh_indices],
    n$Estimate[omoa_indices] + n$CI[omoa_indices],
    n$Estimate[omoh_indices] + n$CI[omoh_indices]
  )
)

rownames(parRanges16_IC) <- c("nilea", "nileh", "omoa", "omoh")


parCovar16 =diag(x = 1, nrow=4, ncol=4)

MPE <- function (Pars,Nnile,Nomo,prey_density){
  model <- function(Time, State, Pars){ 
    with (as.list(c(State,Pars)), { 
      dNd <- -nilea*Nd*Nnile / (1 + nilea*nileh*Nd) - omoa*Nd*Nomo / (1 + omoa*omoh*Nd)  
      return(list(c(dNd))) 
    }) 
  }
  
  Time <- seq(0, 1, by = 0.01) ## e.g. 6 hours. must fit to your units of the handling times and attack rates (e.g. hours, seconds) 
  #Time<-24
  State <- c(Nd = prey_density) ## just some number, must be replaced with your experimental starting densities 
  
  results <- as.data.frame(ode(func = model, y = State, parms = Pars,method = rkMethod("rk45dp7"), times = Time))
  results$eaten<-(prey_density-results$Nd)
  return(results)
}

summary.sensRange<-function(object, ...) {
  
  npar <- attr(object, "npar")
  sens <- as.matrix(object[, -(1:npar)])
  x    <- attr(object, "x")
  names(x) <- NULL
  nx   <- attr(object, "nx")
  varnames  <- attr(object, "var")
  
  if (ncol(sens)>1)
    SumSens <- data.frame(
      x    = x,
      Mean = apply(sens, 2, FUN = mean),
      Sd   = apply(sens, 2, FUN = sd),
      Min  = apply(sens, 2, FUN = min),
      Max  = apply(sens, 2, FUN = max),
      q025  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.025)),
      q05  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.05)),
      q25  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.25)),
      q50  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.5)),
      q75  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.75)),
      q95  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.95)),
      q975  = apply(sens, 2, FUN=function(x)quantile(x, probs = 0.975))
    ) else
      SumSens <- data.frame(
        x    = x,
        Mean = mean(sens, na.rm = TRUE),
        Sd   = NA,
        Min  = min(sens, na.rm = TRUE),
        Max  = max(sens, na.rm = TRUE),
        q025  = quantile(sens, probs = 0.025, na.rm = TRUE),
        q05  = quantile(sens, probs = 0.05, na.rm = TRUE),
        q25  = quantile(sens, probs = 0.25, na.rm = TRUE),
        q50  = quantile(sens, probs = 0.5, na.rm = TRUE),
        q75  = quantile(sens, probs = 0.75, na.rm = TRUE),
        q95  = quantile(sens, probs = 0.95, na.rm = TRUE),
        q975  = quantile(sens, probs = 0.975, na.rm = TRUE)
      )
  
  rownames(SumSens) <- colnames(sens)
  attr(SumSens, "var") <- attr(object, "var")
  attr(SumSens, "nx")  <- attr(object, "nx")
  class(SumSens)<-c("summary.sensRange", "data.frame")
  
  return(SumSens)
}

conf_int<-function(Pars,parRanges,parCovar,Nnile,Nomo,prey_density){
  Pars2=Pars
  parRanges2=parRanges
  parCovar2=parCovar
  ##assume a normal distribution but I do not have the full variance coriance matrix for the parameters
  #SensR <- sensRange(func = MPE, parms = Pars, NC=Nb_C,NL=Nb_L,NS=Nb_S,prey_density= Daphnia, dist = "norm",
  #                   sensvar = "IS", parMean = Pars2, parCovar =parCovar2,  
  #                 parRange = parRanges2, num = 100)
  
  ##latin hypercube sampling algorithm
  SensR <- sensRange(func = MPE, parms = Pars, Nnile=Nnile, Nomo=Nomo, prey_density=prey_density, 
                     dist = "latin",
                     sensvar = "eaten",   
                     parRange = parRanges2, num = 100)
  Sens <- summary.sensRange(SensR, na.rm= TRUE)
  colnames(Sens)[1]<- c("Time")
  colnames(Sens)[2]<- c("eaten_predicted")
  res<-Sens[101,]
  
  res$Prey_density <-prey_density
  res$nile <- Nnile
  res$omo <- Nomo
  
  return(res)
}

# Then fit the model below to get predictions for your prey densities with each inter/intraspecific predator combination
# You adjust the numbers on the right for "no. pred. treatment1, no pred. treatment2, prey density"
# e.g., "1,1,12" is 1 x predator type 1, 0 x predator type 2, and 12 prey", so it calculates the predicted
# number of prey eaten for those treatments; "2,0,24" is "2 x predator type 1, 0 x predator type 2, and 24 prey"


# Both species together at 20 degrees
k1<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,5)
k2<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,10)
k3<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,20)
k4<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,40)

# Both species together at 25 degrees
k5<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,5)
k6<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,10)
k7<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,20)
k8<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,40)

# Both species together at 30 degrees
k9<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,5)
k10<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,10)
k11<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,20)
k12<-conf_int(Pars16,parRanges16_IC,parCovar16, 1,1,40)

results<-rbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12)
write_xlsx(results, "Non-trophic_ortop.xlsx")


# Fit the model now: 

data = read_xlsx("FR with both AG.xlsx", sheet = "Multi_predator")

names(data)
str(data)
data$Sp_Temp <- paste0(data$Species,"_", data$Temperature)

res1= scheirerRayHare(IS ~ Temperature * Prey_density,
                data = data)

res2= scheirerRayHare(ISNT ~ Temperature * Prey_density,
                data = data)

post_hoc_temp <- pairwise.wilcox.test(data$IS, data$Temperature, p.adjust.method = "bonferroni")
post_hoc_prey <- pairwise.wilcox.test(data$IS, data$Prey_density, p.adjust.method = "bonferroni")

post_hoc_temp <- pairwise.wilcox.test(data$ISNT, data$Temperature, p.adjust.method = "bonferroni")
post_hoc_prey <- pairwise.wilcox.test(data$ISNT, data$Prey_density, p.adjust.method = "bonferroni")

data$Temp_Prey_interaction <- interaction(data$Temperature, data$Prey_density)
pairwise_results_interaction <- pairwise.wilcox.test(data$ISNT, data$Temp_Prey_interaction, method="bonferroni")

# option b 
DT = dunnTest(IS ~ Temperature * Temperature,
              data=data,
              method="bh")
# make the plot now:

data$Temperature <- factor(data$Temperature, levels = c("20°C", "25°C", "30°C", "35°C"))

data_summary <- data %>%
  group_by(Prey_density, Temperature) %>%
  summarise(
    ISNT_mean = mean(IS, na.rm = TRUE),
    ISNT_se = sd(IS, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup() # Ungroup for plotting

p1<- ggplot(data_summary, aes(x = Prey_density, y = ISNT_mean, color = Temperature, group = Temperature)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey65", size=1)+
  geom_errorbar(aes(ymin = ISNT_mean - ISNT_se, ymax = ISNT_mean + ISNT_se), 
                width = 0.2, 
                size = 1, 
                position = position_dodge(1.2)) +
  geom_point(position = position_dodge(1.2), size = 3) +
  theme_bw() +
  labs(x = "Prey density",
       y = "Non-trophic interaction strength (ISNT)") +
  theme(legend.title = element_blank()) + 
  scale_x_continuous(breaks = c(0, 5, 10, 20, 40)) + 
  theme(axis.text = element_text(size=12, color="black"),
        axis.title = element_text(size=14, color="black")) 
  

p1 +p2










