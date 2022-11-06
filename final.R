library(dplyr)
library(tidyverse)
library(lme4)
library(lattice)
library(lmerTest)
library(magrittr)
library(sjPlot) 
library(sjmisc)
library(sjstats)
library(arm)
install.packages("glmmTMB")

dat <- read.csv("please.csv")

#calculating the percentage of non-responses per category
percent_NR_imdfetn <- nrow(subset(give, imdfetn==c(7, 8, 9)))/nrow(give)
percent_NR_imwbcnt <- nrow(subset(give, imwbcnt==c(77,88,99)))/nrow(give)
percent_NR_lrscale <- nrow(subset(give, lrscale==c(77,88,99)))/nrow(give)
percent_NR_ipudrst <- nrow(subset(give, ipudrst==c(7,8,9)))/nrow(give)
#finding all percentages to be <5%
percent_NR_imdfetn
percent_NR_imwbcnt
percent_NR_ipudrst
percent_NR_lrscale

#MICE IMPUTATION

install.packages("naniar")
library(magrittr)
library(naniar)
install.packages("mice")
library(mice)
library(ordinal)
library(tidyverse)
library(VIM)
library(MASS)
dat <- read.csv("please.csv")
bridget1 <- subset(dat, brncntr == 1 & blgetmg == 2) #removed immigrants and ethnic minorities
bridget2 <- bridget1[, c("imdfetn", "imwbcnt", "ipudrst", "lrscale", "cntry")] #loaded in countries, attitudes, feelings, beliefs, and intentions
summary(bridget2) #ensured successful loading in of data
bridget <- subset(bridget1, imdfetn != 7 & imdfetn !=8 &imdfetn !=9 & imwbcnt != 77 & imwbcnt!=88 & imwbcnt!=99 & lrscale != 77 & lrscale != 88 & lrscale != 99 & ipudrst != 7 & ipudrst != 8 & ipudrst != 9) #creating a complete dataset for comparison with MICE-imputed results
bridget2 <- bridget2 %>% replace_with_na(replace = list(imdfetn = c(7,8,9), imwbcnt = c(77,88,99), lrscale = c(77,88,99), ipudrst = c(7,8,9))) #replacing non-responses with NAs
bridget2$imdfetn <- as.factor(bridget2$imdfetn)#converting response to factor
bridget2$imwbcnt <- as.factor(bridget2$imwbcnt)#converting affective explanation to factor
bridget2$ipudrst <- as.factor(bridget2$ipudrst)#converting cognitive explanation to factor
bridget2$lrscale <- as.factor(bridget2$lrscale)#converting behavioural explanation to factor 

bridget$imdfetn <- as.factor(bridget$imdfetn)#converting response to factor
bridget$imwbcnt <- as.factor(bridget$imwbcnt)#converting affective explanation to factor
bridget$ipudrst <- as.factor(bridget$ipudrst)#converting cognitive explanation to factor
bridget$lrscale <- as.factor(bridget$lrscale)#converting behavioural explanation to factor 

#missing data visualisations - spine plots
spineMiss(bridget2[,c("ipudrst", "imdfetn")], 
          xlab = "Cognitive information", 
          ylab = "Attitude", 
          main = "Spine plot for attitude and cognitive information", 
          cex = 5)
spineMiss(bridget2[,c("imwbcnt", "imdfetn")], 
          xlab = "Affective information", 
          ylab = "Attitude", 
          main = "Spine plot for attitude and affective information")
spineMiss(bridget2[,c("lrscale", "imdfetn")], 
          xlab = "Behavioural information", 
          ylab = "Attitude", 
          main = "Spine plot for attitude and behavioural information")

#missing data visualisations - margin plots
marginplot(bridget2[c(2,3)], col = c("blue", "red", "orange"),
           xlab = "Affective information",
           ylab = "Cognitive information",
           main = "Margin plot for affective and cognitive information")
marginplot(bridget2[c(3,4)], col = c("blue", "red", "orange"), 
           xlab = "Cognitive information", 
           ylab = "Behavioural information",
           main = "Margin plot for cognitive and behavioural information")
marginplot(bridget2[c(2,4)], col = c("blue", "red", "orange"),
           xlab = "Affective information", 
           ylab = "Behavioural information", 
           main = "Margin plot for affective and behavioural information")

#Conducting MICE imputation and noting coefficient results - upon comparison found to give little improvement 
tempData <- mice(bridget2,m=2,meth='polr', seed=200)
class(bridget2$imdfetn)
summary(tempData)                           
tempData$imp$imdfetn
modelFit1 <- with(tempData, polr(imdfetn ~ imwbcnt, Hess=TRUE))
summary(pool(modelFit1))#MICE-computed coefficients and standard errors
modelFit2 <- polr(imdfetn ~ imwbcnt + ipudrst + lrscale, data=bridget, Hess=TRUE)
summary(modelFit2) #Complete dataset coefficients and standard errors


#creating a new dataset, renaming columns, reverse scoring, converting discrete numerics to factors
gives <- subset(dat, brncntr == 1 & blgetmg == 2) #remove immigrants and ethnic minorities
give <- gives[, c("imdfetn", "imwbcnt", "ipudrst", "lrscale", "cntry")] #loaded in countries, attitudes, feelings, beliefs, and intentions
summary(give) #checking to ensure data loaded in correctly
ava <- subset(give, imdfetn != 7 & imdfetn !=8 &imdfetn !=9 & imwbcnt != 77 & imwbcnt!=88 & imwbcnt!=99 & lrscale != 77 & lrscale != 88 & lrscale != 99 & ipudrst != 7 & ipudrst != 8 & ipudrst != 9)#removed non-responses from attitudes,feelings, beliefs, and intentions
summary(ava) #check to see if data is cleaned, i.e non-responses have been removed
ava2 <- data.frame(attitude=ava$imdfetn, feelings=sapply(ava$imwbcnt, function(x) 10-x), country=ava$cntry, intentions=ava$ipudrst, beliefs=ava$lrscale) #reverse scoring feelings to move in the same direction as attitudes

#keeping ava2 as original dataset, and creating a new dataset for transforming and performing computations
ava3 <- ava2
a <- as.factor(ava3$attitude)
ava3$feelings <- as.factor(ava3$feelings)
ava3$beliefs <- as.factor(ava3$beliefs)
ava3$intentions <- as.factor(ava3$intentions)
ava3$intentions
ava3$country <- factor(ava3$country, order=FALSE)
class(ava3$attitude)
class(ava3$feelings)
class(ava3$beliefs)
class(ava3$intentions)
class(ava3$country)

ava3$attitude <- a

###############################################################################################
#EXPLORATORY ANALYSES

#descriptive statistics of variables attitude, feelings, and country
lapply(ava3[,c("attitude","feelings", "country","beliefs", "intentions")], table)

#three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ attitude + feelings + intentions, data=ava3))

boxplot(split(ava2$attitude,factor(ava2$feelings)), 
        main="Relationship between attitude and affective information", 
        xlab="Affective information", 
        ylab="Attitude")
boxplot(split(ava2$attitude, factor(ava2$beliefs)), 
        main="Relationship between attitude and cognitive information", 
        xlab="Cognitive information", 
        ylab="Attitude")
boxplot(split(ava2$attitude, factor(ava2$intentions)), 
        main="Relationship between attitude and behavioural information", 
        xlab="Behavioural information", 
        ylab="Attitude")

#SCATTER PLOT FEELINGS AND ATTITUDES - 
ggplot(ava2)+
  geom_point(aes(x=feelings, y=attitude), position="jitter", alpha=1/5)


#SCATTER PLOT INTENTIONS AND ATTITUDES - 
ggplot(ava2)+
  geom_point(aes(x=factor(intentions), y=attitude), position="jitter", alpha=1/10)

#SCATTER PLOT BELIEFS AND ATTITUDES - 
ggplot(ava2)+
  geom_point(aes(x=beliefs, y=attitude), position="jitter", alpha=1/15)
############################################################################################


###########################################################################################

#USING POLR TO PERFORM ORDINAL LOGISTIC REGRESSION WITHOUT RANDOM EFFECTS

library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
install.packages("relimp")
install.packages("XQuartz")
library(relimp)
library(effects)

#ordered regression - f e e l i n g s
mf <- polr(attitude ~ feelings, data=ava3, Hess=TRUE)
summary(mf)
extractAIC(mf)[2] #extracting AIC for inclusion into the ANOVA table

#plotting the effect of feelings
Effect(focal.predictors = "feelings", mf)
plot(Effect(focal.predictors = "feelings", mf), 
     xlab = "Affective information", 
     ylab = "Attitude (probability)",
     main = "Affective influence on attitude")

#ordered regression - i n t e n t i o n s 
mi <- polr(attitude ~ intentions, data=ava3, Hess=TRUE)
summary(mi)
extractAIC(mi)[2]

#plotting the effect of intentions
Effect(focal.predictors = "intentions", mi)
plot(Effect(focal.predictors = "intentions", mi), 
     xlab = "Behavioural information",
     ylab = "Attitude (probability)", 
     main = "Behavioural influence on attitude")


#ordered regression - b e l i e f s 
mb <- polr(attitude ~ beliefs, data=ava3, Hess=TRUE)
summary(mb)
extractAIC(mb)[2]

#plotting the effect of beliefs
Effect(focal.predictors = "beliefs", mb, residuals=TRUE)
plot(Effect(focal.predictors = "beliefs", mb, residuals=TRUE), 
     xlab = "Cognitive information", 
     ylab = "Attitude (probability)", 
     main = "Cognitive influence on attitude")

#ordered interaction between predictors and country predictor without random effects
mii1 <- clm(attitude ~ feelings + beliefs + intentions, data=ava3, Hess=TRUE) #used clm from ordinal to check p-values which are not visible with polr
summary(mii1)

#Creating a names vector to append to the dataframe for making a table for coefficients for mii1
names <- c("1|2", "2|3", "3|4", "affective1", "affective2", "affective3", "affective4", "affective5", "affective6", "affective7", "affective8", "affective9", "affective10", 
           "cognitive1", "cognitive2", "cognitive3", "cognitive4", "cognitive5", "cognitive6", "cognitive7", "cognitive8", "cognitive9", "cognitive10",
           "behavioural2", "behavioural3", "behavioural4", "behavioural5", "behavioural6")

#making a table of coefficients
extracting <- summary(mii1) 
mydff<- data.frame(extracting$coefficients[,"Estimate"]) #extracting estimates from summary of mii1 model
mydff$names <- names #creating a column for altered category names
mydff <- mydff[-c(1,2,3),] #removing threshold estimates
confint_df <- confint(mii1) #extracting confidence intervals from mii1 
confint_df$estimate <- mydff$extracting.coefficients....Estimate.. #creating column in for previously extracted estimates
confint_df$names <- mydff$names #adding column for previously created category names
confint_df <- confint_df[,c(4,1,3,2)] #re-ordering columns for presentation
confint_table <- confint_df %>% gt() #creating a basic table
confint_table #viewing the table
confint_table%>% #designing the table (aesthetics etc.)
  tab_header(title = "Attitude on affective, cognitive, and behavioural influences", 
             subtitle = "Coefficients and confidence intervals of logistic regression")%>%
  cols_label(
    names = "Variable name", 
    X2.5.. = "Lower limit", 
    estimate = "Estimate", 
    X97.5.. = "Upper limit")%>%
  cols_align(align="center")%>%
  tab_row_group(
    label = "Behavioural information",
    rows = 21:25
  )%>%
  tab_row_group(
    label="Cognitive information", 
    rows = 11:20
  )%>%
  tab_row_group(
    label = "Affective information", 
    rows = 1:10
  )%>%
  tab_style(
    style = list(
      cell_fill(color="#c3dcc3"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = estimate
    ))%>%
  tab_style(
    style = list(
      cell_fill(color = "#c3dcc3")
    ),
    locations = cells_body(
      columns = names
    ))
  

  
##FOR MY OWN INTEREST - plotted and analysed profile likelihoods for mii1 model
#profile likelihood for mii1
props <- profile(mii1)
plot(props)

confint(props)

## plotting the profiles:
par(mfrow = c(2, 2))
plot(props, root = TRUE) ## check for linearity
par(mfrow = c(2, 2))
plot(props)
par(mfrow = c(2, 2))
plot(props, approx = TRUE)
par(mfrow = c(2, 2))
plot(props, Log = TRUE)
par(mfrow = c(2, 2))
plot(props, Log = TRUE, relative = FALSE)
#Allowed for completeness:
par(mfrow = c(2, 2))
plot(props, Log = FALSE, relative = FALSE)

#creating new model for testing country effects. "Country" is a factor, and in clm model will be treated as a dummy variable upon regression
miinew <- clm(attitude ~ feelings + beliefs + intentions + country, data=ava3, Hess=TRUE)
summary(miinew)
extractAIC(miinew)[2]
anova(mii1, miinew) #testing for the significance of country effects in data

#redoing all previous regressions with the clm function, ordinal package for efficient comparison with country interaction model
mfclm <- clm(attitude ~ feelings, data=ava3, Hess=TRUE)
extractAIC(mfclm)[2]
#feelings and beliefs
mfb <- clm(attitude ~ feelings + beliefs, data=ava3, Hess=TRUE)
extractAIC(mfb)[2]
#feelings and intentions
mfi <- clm(attitude ~ feelings + intentions, data=ava3, Hess=TRUE)
extractAIC(mfi)[2]
#feelings and intentions and beliefs
mii1
extractAIC(mii1)[2]
#feelings and intentions and beliefs and country effects
miinew
extractAIC(miinew)[2]

anova(mfclm, mfb) #HYPOTHESIS TEST BETWEEN FEELINGS, FEELINGS + BELIEFS
anova(mfclm, mfb, mfi) #HYPOTHESIS TEST BETWEEN FEELINGS, FEELINGS + BELIEFS, FEELINGS + BELIEFS + INTENTIONS
anova(mii1, miinew)#HYPOTHESIS TEST BETWEEN FEELINGS + BELIEFS + INTENTIONS, FEELINGS + BELIEFS + INTENTIONS + COUNTRY


maaa <- polr(attitude ~ feelings + intentions + beliefs, data=ava3, Hess=TRUE)
relimp(maaa)

###########################################################################################
library(sjPlot)
library(ordinal)
library(effects)
library(MASS)
#USING CLMM TO PERFORM ORDINAL LOGISTIC REGRESSION WITH RANDOM EFFECTS
#The plots created here were not included in the report due to limited space
#Changes were observed mainly in the size of standard errors which got larger

#effect of f e e l i n g s + random intercept with country
modf <- clmm(attitude ~ feelings + (1|country), data=ava3, Hess=T)
summary(modf)

#plotting the effect of feelings + random intercept with country
Effect(focal.predictors = "feelings", modf)
plot(Effect(focal.predictors = "feelings", modf))

#effect of b e l i e f s + random intercept with country
modb <- clmm(attitude ~ beliefs + (1|country), data=ava3)
summary(modb)

#plotting the effect of beliefs + random intercept with country
Effect(focal.predictors = "beliefs", modb)
plot(Effect(focal.predictors = "beliefs", modb))

#effect of i n t e n t i o n s + random intercept with country
modii <- clmm(attitude ~ intentions + (1|country), data=ava3)
summary(modii)

#plotting the effect of intentions + random intercept with country
Effect(focal.predictors = "intentions", modii)
plot(Effect(focal.predictors = "intentions", modii))


#plot for random country effects and their residuals
ci <- anova1$ranef + qnorm(0.975) * sqrt(anova1$condVar) %o% c(-1, 1)
ord.re <- order(anova1$ranef)
dff <- data.frame(ranef=anova1$ranef, c=country)
dff_ordered <- dff[order(dff$ranef),]
ci <- ci[order(anova1$ranef),]
plot(1:28, anova1$ranef[ord.re], axes=FALSE, ylim=range(ci),
     xlab="Country", ylab="Country effect", 
     main = "Country-wise distribution modes and confidence intervals")
axis(1, at=1:28, labels = dff_ordered$c)
axis(2)
for(i in 1:28) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)

#USING LIKELIHOOD RATIO TESTS TO ASSESS THE SIGNIFICANCE AND RELATIVE STRENGTH OF AFFECTIVE (FEELINGS), COGNITIVE (BELIEFS), AND INTENTIONS ON ATTITUDES TO IMMIGRATION 
#RESULTS WERE THE SAME AS THOSE OBTAINED WITH DUMMY VARIABLES, SO NOT INCLUDED IN REPORT
anova1 <- clmm(attitude ~ feelings + intentions + beliefs + (1|country), data=ava3, Hess=TRUE) #base model
anova2 <- clmm(attitude ~ feelings + intentions + (1|country), data=ava3, Hess=TRUE); anova(anova1, anova2) #test for BELIEFS
anova3 <- clmm(attitude ~ feelings + beliefs + (1|country), data=ava3, Hess=TRUE); anova(anova1, anova3) #test for INTENTIONS
anova4 <- clmm(attitude ~ beliefs + intentions + (1|country), data=ava3, Hess=TRUE); anova(anova1, anova4) # test for FEELINGS
anova(anova1, anova2, anova3, anova4) #this tells me that differences across countries is are significant


#PLOT - Influence of affective information on attitudes toward immigration
plot(predictorEffects(modf), 
     axes=list(grid=TRUE, y=list(style="stacked")),
     lattice=list(key.args=list(columns=1)), 
     main = "Affective influence on attitudes toward immigration", 
     ylab= "Attitude (probability)", 
     xlab = "Affective information")

#PLOT - Influence of cognitive information on attitudes toward immigration
plot(predictorEffects(modb), 
     axes=list(grid=TRUE, y=list(style="stacked")),
     lattice=list(key.args=list(columns=1)), 
     main = "Cognitive influence on attitudes toward immigration", 
     ylab= "Attitude (probability)", 
     xlab = "Cognitive information")

#PLOT - Influence of behavioural information on attitudes toward immigration
plot(predictorEffects(modii), 
     axes=list(grid=TRUE, y=list(style="stacked")),
     lattice=list(key.args=list(columns=1)), 
     main = "Behavioural influence on attitudes toward immigration", 
     ylab= "Attitude (probability)", 
     xlab = "Behavioural information")

atp <- polr(attitude ~ country, data=ava3, Hess=TRUE)

#PLOT - proportion of attitude types across countries
plot(predictorEffects(atp),
     axes=list(grid=TRUE, x=list(rug=FALSE), y=list(style="stacked")),
     lattice=list(key.args=list(columns=1)),
     lines=list(multiline=TRUE), 
     ylab = "Attitude (probability", 
     xlab = "Countries", 
     main = "Proportion of attitude types across countries")

############################################################################################

#CREATING MODEL FIT ANOVA TABLES
anova(mfclm, mfb)
anova(mfclm, mfb, mfi) #model 1, testing for significance of each predictor
anova(mii1, miinew) #model 2, testing for significance of country effects

install.packages("gt")
library(gt)
library(tidyverse)
library(glue)

anova_wr <- data.frame("Modelling_attitude_on" = c("Affective information", "Affective and behavioural information", "Affective, cognitive, and behavioural information"), 
                       "No_of_parameters" = c(13, 18, 28), 
                       "AIC"=c(73727.9, 73008.4, 72358.3), 
                       "logLikehood" = c(-36851, -36486, -36151),
                       "LR_Statistic" = c("", 729.5, 670.1),
                       "df" = c("", 5, 10),
                       "Chisq_significance_test" = c("", "< 2.2e-16 ***", "< 2.2e-16 ***"))
anova_wr %>%
  gt()%>%
  tab_header(title = "Likelihood Ratio tests for comparing model fits", 
             subtitle = "Significance of affective, behavioural, and cognitive information tested")%>%
  cols_label(
    Modelling_attitude_on = "Explanatory variables included", 
    No_of_parameters = "No. of parameters", 
    AIC = "AIC", 
    logLikehood = "log likelihood", 
    LR_Statistic = "Likelihood Ratio statistic", 
    df = "Degrees of freedom", 
    Chisq_significance_test = "Chi-squared test (Pr(>Chi)"
  )%>%
  cols_align(align="center")


anova_country_test <- data.frame("Modelling_attitude_on" = c("Affective, cognitive, and behavioural information", "Country dummies and affective, cognitive, and behavioural information"), 
                                 "No_of_parameters" = c(28,55), 
                                 "AIC"=c(72358, 69283), 
                                 "logLikehood" = c(-36151,-34586),
                                 "LR_Statistic" = c("", 3129.4),
                                 "df" = c("", 27),
                                 "Chisq_significance_test" = c("", "< 2.2e-16 ***"))

anova_country_test%>%
  gt()%>%
  tab_header(title = "Likelihood Ratio tests for comparing model fits", 
                   subtitle = "Significance of clustering due to country effects tested")%>%
  cols_label(
    Modelling_attitude_on = "Explanatory variables included", 
    No_of_parameters = "No. of parameters", 
    AIC = "AIC", 
    logLikehood = "log likelihood", 
    LR_Statistic = "Likelihood Ratio statistic", 
    df = "Degrees of freedom", 
    Chisq_significance_test = "Chi-squared test (Pr(>Chi)")%>%
  cols_align(align="center")

#############################################################################################

#SPATIAL ANALYSIS

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
install.packages("rgeos")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

library("sf")
cent_error <- c(12, 71, 99, 184, 188, 189) #removing rows with error-inducing values; they represented countries not part of the analysis
world[cent_error, ]$name
world <- world[-cent_error, ]
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
colnames(world)[which(names(world) == "iso_a2")] <- "country"

library(tidyverse)
install.packages("viridis")
install.packages("paletteer")
library(paletteer)
library(RColorBrewer)

dat <- read.csv("please.csv")

ava2 #calling ava2
df <- data.frame(country=ava2$country, attitude=ava2$attitude)#creating a new dataframe with country and attitude values
agg <- aggregate(x=df, 
                 by=list(df$country),
                 FUN=mean)#aggregating attitude values after grouping by country
aggre <- data.frame(agg) #keeping the old dataframe, creating a new one for further transformations
aggre <- select(aggre, -country) #removing extraneous country variable
names(aggre)[names(aggre) == "Group.1"] <- "country" #renaming existing variable common between geographical and attitude dataframes as "country"

world1 <- merge(world, aggre, by="country") #merging by the now commonly labelled column
str(world1) #checking result

#Plotting this new merged dataframe
ggplot(data=world1)+
  geom_sf(aes(fill=attitude))+
  geom_text(data= world_points,aes(x=X, y=Y, label=name), color = "thistle3", fontface = "bold", check_overlap = TRUE, size = 3) +
  coord_sf(xlim=c(-25,35), ylim=c(30,75), expand=TRUE) +
  scale_fill_viridis_c(option = "rocket") +
  labs(x="Longitudes", y="Latitudes", 
       title="Attitudes towards immigration in Europe")
display.brewer.all()





