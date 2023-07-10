#setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)

#Setting the seed is helpful in removing the "randomness"

##Setting the Seed
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)

##Installing pacman
tryCatch(require(pacman),finally=utils:::install.packages(pkgs='pacman',repos='http://cran.r-project.org'));
require(pacman)

pacman::p_load(Hmisc,
               checkmate,
               corrr,
               conflicted,
               readxl,
               dplyr,
               tidyr,
               ggplot2,
               knitr,
               evaluate,
               iopsych,
               psych,
               quantreg,
               lavaan,
               xtable,
               reshape2,
               GPArotation,
               Amelia,
               # esquisse,
               expss,
               multilevel,
               janitor,
               mice,
               lmtest,
               tidylog
)

##Load Libraries
suppressPackageStartupMessages({
    library(Hmisc) # Contains many functions useful for data analysis
    library(checkmate) # Fast and Versatile Argument Checks
    library(corrr) # Correlations in R
    library(conflicted) # Makes it easier to handle same named functions that are in different packages
    library(readxl) # reading in Excel files
    library(dplyr) # data manipulation
    library(tidyr) # Tidy Messy Data
    library(ggplot2) # data visualization
    library(knitr) # knitting data into HTML, Word, or PDF
    library(evaluate) # Parsing and Evaluation Tools that Provide More Details than the Default
    library(iopsych) # Methods for Industrial/Organizational Psychology
    library(psych) # Procedures for Psychological, Psychometric, and Personality Research
    library(quantreg) # Quantile Regression
    library(lavaan) # confirmatory factor analysis (CFA) and structural equation modeling (SEM)
    library(xtable) # Export Tables to LaTeX or HTML
    library(reshape2) # transforming data between wide and long (tall)
    library(GPArotation) # GPA Factor Rotation
    library(Amelia) # A Program for Missing Data
    # library(esquisse) # Explore and Visualize Your Data Interactively
    library(expss) # Tables, Labels and Some Useful Functions from Spreadsheets and 'SPSS' Statistics
    library(multilevel) # Multilevel Functions
    library(janitor) # 	Simple Tools for Examining and Cleaning Dirty Data
    library(mice) # Multivariate Imputation by Chained Equations
    library(skimr) # Exploratory Data Analysis
    library(lmtest) # A collection of tests, data sets, and examples for diagnostic checking in linear regression models    
    library(tidylog) # Creates a log to tell you what your tidyverse commands are doing to the data. NOTE: MAKE SURE TO ALWAYS LOAD LAST!!!
})

for (f in getNamespaceExports("tidylog")) {
    conflicted::conflict_prefer(f, "tidylog", quiet = TRUE)
}

## Load your data
library(psych)

library(haven)
data <- read_sav("HWK 1/SAQ.sav") #This creates an object in the environment 
View(data)

str(data)

#Glimpse the data
glimpse(data) #from `dplyr`

#Identify column names
dput(colnames(data))

##Exploratory Data Analysis

#This process will consist of analyzing the data to see what information is missing or potentially problematic.

#Missing Data
#Identifiying any missing data
missmap(data)

#Percent missing calculated
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}

missing <- apply(data, 1, percentmissing) # Use an apply function to loop it. 1 indicates rows and 2 indicates columns

table(round(missing, 0))

#There is no missing data which indicates that all respondents answered all items within the survey.

##Outlier Detection

#Outliers can be identified through the Malalanobis calculation.

#First, it is important to ensure the outlier calculation is performed on just the items within the survey. Currently the data has information that is not item related.

#Creating a new data frame with only survey items
data_questions <- data[1:23] # [ ] this selects the columns that contain survey information

#Calculating Malanobis distance
set.seed(123) #removing the randomness

##outliers
cutoff = qchisq(1-.001, ncol(data_questions))
mahal = mahalanobis(data_questions,
                    colMeans(data_questions),
                    cov(data_questions))
cutoff ##cutoff score
ncol(data_questions) ##df
summary(mahal < cutoff)

#FALSE is an indication of outliers within the data set. According to the cut score set within the code there are 97 outliers that require further investigation.

#Creating object within data frame with mahal calculation
data_mahal <- data_questions %>%
    bind_cols(mahal) %>%
    rename(mahal = `...24`) # renaming the new column "mahal"

#Take a look at the observations labeled as FALSE.

#Looking at data outside the Mahal cutscore
mahal_out <- data_mahal %>%
    filter(mahal > cutoff) %>% #filtering mahal scores based upon cutoff
    arrange(desc(mahal)) # sort mahal values from most to least

#There are 97 observations that did not make the cut score for the mahal distance calculation. After assessing these items it appears many of the respondents chose answers that consistently fell on the "extreme" values within the item measures. These items will be removed in the line of code below.

##Outlier Removal

#Excluding Outliers
#data_noout will not contain any outliers 
data_noout <- data_questions %>%
    filter(mahal < cutoff)

##Additivity

#Additivity Calculation
#make sure to use data that does not contain outliers within this calculation
correl = cor(data_noout, use = "pairwise.complete.obs")

symnum(correl)

correl

#The goal is to identify any "1" that falls outside the diagonal. The output from the additivity calculation shows there is no substantial overlap among the questions and that each question does in fact correlate with each other.

#Next, assumptions for EFA should continue to be tested. Running a fake regression is necessary to screen the existing data. Using a chi sq value of 7 is a good rule of thumb. The purpose of this regression is to test for a pattern within the residuals using fake data from a generated random number. If there is a pattern, that would be an indication that something is wrong with the prediction because the residuals are not randomized from a random group of data.

#Fake Regression
random = rchisq(nrow(data_noout), 7) #randomizing data
fake = lm(random~., data = data_noout) #fake regression model
standardized = rstudent(fake) #standardizing values to make them able to be interpreted
fitted = scale(fake$fitted.values) #fit of the regression model
summary(fake)

#Check the residuals from the fake regression using a histogram to analyze the distribution.

#Checking Normality
hist(standardized)

#The histogram in question seems to have a normal distribution. There is a slight skew to the left of the graph but there is still a noticeable bell shaped distribution.

##Heteroscedasticity

#Breusch-Pagan Test
#load lmtest library
library(lmtest)

#perform Breusch-Pagan Test
bptest(fake)

#The test statistic is 18.31 and the corresponding p-value is 0.741. Since the p-value is not less than 0.05, we fail to reject the null hypothesis. We do not have sufficient evidence to say that heteroscedasticity is present in the regression model.

#Q-Q Plot

#Check linearity using qqplot.
##linearity
qqnorm(standardized)
abline(0,1) #inserting a test line within the plot

#The Q-Q Plot above provides sufficient evidence that variables within the data are linearly related with each other. There does appear to be a very slight curve as the standardized values approach -2 and +2. These values still appear to be close enough to the prediction line within the plot. Many of the points that are significantly bent and off the line fall outside the -2 and +2 threshold which indicates they are outside the majority of values within the distribution.

##Homogeneity

#Checking for homogeneity
plot(fitted,standardized)
abline(0,0) #inserts line within the plot
abline(v = 0) #inserts line within the plot

#There seems to be some skew towards the bottom of the plot. More analysis is needed to adequately assess the data.

##Bartlett's Test

#The Bartlett Test checks the correlation adequacy.
cortest.bartlett(correl, n = nrow(data_noout)) #no outliers within the data

#The Bartletts test was statistically significant meaning there are large enough correlations for an EFA.

##Kaiser, Meyer, Olkin Measure of Sampling Adequacy (KMO) Test

#Now the KMO test is needed to assess if the sampling is adequate to run an EFA.

##sampling adequacy KMO test
#need to indicate what variables to include within the correl test by using [ ]
KMO(correl[,1:23])

#The MSA was calculated at 0.93. This is a good result because it is above the general threshold of 0.90 and indicates the sample size is adequate to perform an EFA.

#Both the KMO and Bartlett's test provide statistical evidence to run an EFA. It will be helpful to change "data_noout" to "data" for the remainder of the calculations.

#Change original data to data with no outliers
data <- data_noout

#The code above removed a handful of observations from the original data that were not related to the survey items. This will allow the coding for the EFA to be more simple.

##Exploratory Factor Analysis (EFA)

#Before conducting an EFA the data must be split before being utilized. In this case the data will be split using a 50/50 ratio. This split will place 50% of the data within a training group and 50% of the data within a test group. A 50/50 ratio was used to provide an adequate amount of data in both the training and testing groups. This will provide roughly 1,200 observations for both the EFA and future CFA.

#You must set your seed before splitting your data!
set.seed(2023)

#Next, creating an ID variable for the data set will be beneficial.

#creating ID variable
data <- data %>% 
    mutate(ID = row_number())

#Move ID to front
#moving ID variable to the front of data set
data <- data %>%
    dplyr::select(ID, everything()) #everything() is needed to reference all other variables

#checking to see if ID variable moved to the front of the data set
colnames(data)

#Now that the ID variable has been created the Training and Test data sets can be created.

#Splitting the Data
#creating two data subsets using the 50/50 ratio
training <- sample(data$ID, length(data$ID)*0.5) #0.5 indicates the split ratio

data_training <- subset(data, ID %in% training)
data_test <- subset(data, !(ID %in% training)) #Remember "!" for test data

#Now we can visualize the training data set using histograms.

#creating historgrams of training data
#breaks indicates the range of data within the x axis
hist(data_training$Question_01, breaks = 5) 
hist(data_training$Question_02, breaks = 5)
hist(data_training$Question_03, breaks = 5)
hist(data_training$Question_04, breaks = 5)
hist(data_training$Question_05, breaks = 5)
hist(data_training$Question_06, breaks = 5)
hist(data_training$Question_07, breaks = 5)
hist(data_training$Question_08, breaks = 5)
hist(data_training$Question_09, breaks = 5)
hist(data_training$Question_10, breaks = 5)
hist(data_training$Question_11, breaks = 5)
hist(data_training$Question_12, breaks = 5)
hist(data_training$Question_13, breaks = 5)
hist(data_training$Question_14, breaks = 5)
hist(data_training$Question_15, breaks = 5)
hist(data_training$Question_16, breaks = 5)
hist(data_training$Question_17, breaks = 5)
hist(data_training$Question_18, breaks = 5)
hist(data_training$Question_19, breaks = 5)
hist(data_training$Question_20, breaks = 5)
hist(data_training$Question_21, breaks = 5)
hist(data_training$Question_22, breaks = 5)
hist(data_training$Question_23, breaks = 5)

#Now that the data has been visualized it is important to look at the correlation matrix of the data set.

#Flatten Correlation Matrix Function

flattenCorrMatrix <- function(cormat, pmat, nmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut],
        n = nmat[ut]
    )
}

#You must install "Hmisc" package to make this work.

#Load Hmisc
#install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

#It is necessary to create a new data frame using data_training when creating a flattened correlation matrix. This will ensure the original data is not tampered with.

#As a matrix
data_training_MAT <- as.matrix(data_training)

#Feeding correlation matrix into flatten function
library(Hmisc)
#install.packages("checkmate", dependencies = TRUE)
library(checkmate)
res <- rcorr(data_training_MAT)
print(res)

#Flattened Correlation Matrix w/data
library(dplyr)
Data_Flat_Cor_Mat <- flattenCorrMatrix(res$r, res$P, res$n) #these p values match SPSS

Data_Flat_Cor_Mat[,3:5] <- round(Data_Flat_Cor_Mat[,3:5], 3) 

#Adding * to any correlation with p<0.05
Data_Flat_Cor_Mat <- Data_Flat_Cor_Mat %>%
    mutate(Sig = ifelse(p < 0.05, paste0(p, "*"),
                        p))

Data_Flat_Cor_Mat

#Now that the correlations have been assessed it is time to run an EFA. This EFA will be run using a parallel analysis.

#Parallel Analysis
#make sure your data range is set to just the variables in question. This is removing the ID term from the analysis.
library(psych)
fa.parallel(data_training[c(2:24)])

#According to the scree plot it appears the elbow of the graph begins around 2 factors. 1 factor is where the EFA will begin and factors will be added until simple structure is achieved that adequately represents the data. Additionally, the maximum likelihood method will be used to conduct the EFA. Maximum likelihood will be a good approach due to the relatively normal distribution.

#To adequately interpret each factor analysis to determine how the models fit and how the factors correlate with one another a rotation is needed. For each analysis, a promax rotation will be utilized to allow the factors to correlate with each other and because the data has a large sample size. It is important to note that a promax rotation is an oblique rotation.

#NOTE: The variable naming convention is as follows: * fa = Factor Analysis * ml = Maximum Likelihood (the method of factor analysis we are using) * 1 = the number of factors we think are in the data * trn = the training data

#Factor Analysis using 1 Factor & Maximum Likelihood
#make sure to remove all other varaibles from the efa. This inlcudes demographics and ID variables
#"ml" stands for maximum likelihood
fa_ml_1_trn <- fa(data_training[c(2:24)], nfactors = 1, fm="ml")

print(fa_ml_1_trn)

#This can be made easier to read by running the EFA with a cutoff threshold of .3. This will show factor loadings that are above the .3 threshold.

#Rotating 1 factor model using promax
fa_ml_1_trn <- fa(data_training[c(2:24)], nfactors = 1, fm="ml", rotate = "promax")

print(fa_ml_1_trn$loadings, cutoff = .3)

#Factor Analysis using 2 Factor & Maximum Likelihood
fa_ml_2_trn <- fa(data_training[c(2:24)], nfactors = 2, fm="ml")

print(fa_ml_2_trn)

# 2 Factor EFA with cutoff
print(fa_ml_2_trn$loadings, cutoff = .3)

#Rotating 2 factor model using promax
fa_ml_2_trn <- fa(data_training[c(2:24)], nfactors = 2, fm="ml", rotate = "promax")

print(fa_ml_2_trn$loadings, cutoff = .3)

#There are very few items loading onto the first factor. In fact, one of the loadings is a cross loading from an item loading onto the second factor. Removing cross loaded items and items that do not reach the .3 threshold will provide further insight into the two factor model.

#Listing column names
colnames(data_training)

# Removing problematic items from data_training}
#the select function selects the items to be removed from the data frame
data_training_MOD <- data_training %>%
    dplyr::select(-c(Question_08, Question_23))

#Checking modified data frame
colnames(data_training_MOD)

#2 Factor model with items removed}
#note that the column numbers differ depending on the items being removed
fa_ml_2_trn_MOD <- fa(data_training_MOD[c(2:22)], nfactors = 2, fm="ml", rotate = "promax")

print(fa_ml_2_trn_MOD$loadings, cutoff = .3)

#There still are some problematic items within the model. Items 6, 9, and 22 will be removed to reassess the two factor model.

#Removing additional items from data
data_training_MOD <- data_training %>%
    dplyr::select(-c(Question_06, Question_09, Question_22, Question_08, Question_23))

#Assessing column names
colnames(data_training_MOD)

#Rerunning 2 factor model
fa_ml_2_trn_MOD <- fa(data_training_MOD[c(2:19)], nfactors = 2, fm="ml", rotate = "promax")

print(fa_ml_2_trn_MOD$loadings, cutoff = .3)

#After removing a series of items from the training data the 2 factor model seems to be struggling to adequately place items within factors without producing cross loadings.

#Running a 3 factor model will be beneficial to assess whether a third factor improves factor loadings for the overall model.

#Factor Analysis using 3 Factor & Maximum Likelihood}
fa_ml_3_trn <- fa(data_training[c(2:24)], nfactors = 3, fm="ml")

print(fa_ml_3_trn)

#3 Factor EFA with cutoff
print(fa_ml_3_trn$loadings, cutoff = .3)

#Rotating 3 factor model using promax
fa_ml_3_trn <- fa(data_training[c(2:24)], nfactors = 3, fm="ml", rotate = "promax")

print(fa_ml_3_trn$loadings, cutoff = .3)

#The 3 factor model still has a few cross loadings but does seem to capture a more even spread throughout the factors. Factors that have cross loadings as well as loadings that do not reach the .3 threshold will be dropped to further assess the model. The items being removed are as follows: Question_06, Question_12, Question_22, Question_23.

# Assessing column names
colnames(data_training)

#Removing problematic items from data_training
#the select function selections the items to be removed from the data frame
data_training_MOD <- data_training %>%
    dplyr::select(-c(Question_06, Question_12, Question_22, Question_23))

#Checking modified data frame
colnames(data_training_MOD)

#The new data frame no longer contains the problematic items. The 3 factor model will be rerun to assess the overall model fit and if it has a simple structure.

#3 Factor model with items removed
fa_ml_3_trn_MOD <- fa(data_training_MOD[c(2:20)], nfactors = 3, fm="ml", rotate = "promax")

print(fa_ml_3_trn_MOD$loadings, cutoff = .3)

#After removing 4 items from the overall training data, this model looks pretty good. There are no cross loadings among the factors and the cumulative variance has increased upon each item being removed. This 3 factor model seems to do a better job of describing the data than the 2 factor model. A 4 factor model might parse out the items in a more effective manner while still having a simple structure.

#Factor Analysis using 4 Factor & Maximum Likelihood}
fa_ml_4_trn <- fa(data_training[c(2:24)], nfactors = 4, fm="ml")

print(fa_ml_4_trn)

#4 Factor EFA with cutoff
print(fa_ml_4_trn$loadings, cutoff = .3)

#Rotating 4 factor model using promax}
fa_ml_4_trn <- fa(data_training[c(2:24)], nfactors = 4, fm="ml", rotate = "promax")

print(fa_ml_4_trn$loadings, cutoff = .3)

#There are a handful of cross loading within the 4 factor model. Remove these items and rerun the EFA with the removed items.

#Removing items from 4 factor model
data_training_MOD <- data_training %>%
    dplyr::select(-c(Question_06, Question_12, Question_19))

#Running 4 factor model with modified training data
fa_ml_4_trn_MOD <- fa(data_training_MOD[c(2:21)], nfactors = 4, fm="ml", rotate = "promax")

print(fa_ml_4_trn_MOD$loadings, cutoff = .3)

#There are still two cross loadings occurring within Question_16 and Question_04. The code below will remove those items from the data. 

#Removing additional items
data_training_MOD <- data_training %>%
    dplyr::select(-c(Question_06, Question_12, Question_19, Question_16, Question_04))

#Rerunning 4 factor model with removed items
fa_ml_4_trn_MOD <- fa(data_training_MOD[c(2:19)], nfactors = 4, fm="ml", rotate = "promax")

print(fa_ml_4_trn_MOD$loadings, cutoff = .3)

#This model looks fairly good. There are no cross loadings and both the TLI and RMSEA are at acceptable levels. A 5 factor model will be run to continue to see if it improves upon the 4 factor model.

# Factor Analysis using 5 Factor & Maximum Likelihood}
fa_ml_5_trn <- fa(data_training[c(2:24)], nfactors = 5, fm="ml")

print(fa_ml_5_trn)

#5 Factor EFA with cutoff
print(fa_ml_5_trn$loadings, cutoff = .3)

#Rotating 5 factor model using promax
fa_ml_5_trn <- fa(data_training[c(2:24)], nfactors = 5, fm="ml", rotate = "promax")

print(fa_ml_5_trn$loadings, cutoff = .3)

#The 5 factor model does not have any cross loadings present. The model seems to have adequate TLI and RMSEA statistics. Running a 6 factor model will be beneficial to continue to assess what model fits the data best. 

# Factor Analysis using 6 Factor & Maximum Likelihood}
fa_ml_6_trn <- fa(data_training[c(2:24)], nfactors = 6, fm="ml")

print(fa_ml_6_trn)

#5 Factor EFA with cutoff
print(fa_ml_6_trn$loadings, cutoff = .3)

#Rotating 6 factor model using promax
fa_ml_6_trn <- fa(data_training[c(2:24)], nfactors = 6, fm="ml", rotate = "promax")

print(fa_ml_6_trn$loadings, cutoff = .3)

#The 6 factor model does not have any cross loadings present. Both the 5 factor and 6 factor models have great TLI and RMSEA statistics but their structures seem to get more and more complex. The 4 factor mdoel has very good TLI and RMSEA statistics as well. Due to the simple structure of the modified 3 factor model it has been deemed the best fit for the data. 

##Assessing Exploratory Factor Analysis

#It is helpful to look at a few statistics about the exploratory factor analysis. The goodness of fit of a factor model is assessed by the TLI statistic. The TLI for the final 3 factor model was calculated at 0.914 which is greater than the 0.90 cutoff. This statistic indicates that the 3 factor model is a good fit for the data collected. The RMSEA assesses the residuals of a factor model. The RMSEA for the final 3 factor model was calculated at 0.0598. This falls just below 0.06 which is a common threshold for an acceptable residual statistic. It is important to note the 4 factor model did have better goodness of fit and residual statistics but the 3 factor model was used due its more simple structure. 

#The final 3 factor model is named "fa_ml_3_trn_MOD". To make this more clear this model and the training data that it was run with will be shown below.

#Final 3 factor data
data_training_MOD <- data_training %>%
    dplyr::select(-c(Question_06, Question_12, Question_22, Question_23))

#Final 3 factor model
fa_ml_3_trn_MOD

#This model has been deemed the most simple structure to assess the data. To make the scale building process easier, the factor loadings will be moved to an excel spreadsheet.

#Making Final 3 factor model loadings a dataframe
#the round command tells R to round the loadings, the "3" tells R what decimal place to round
fa_ml_3faclds_final <- as.data.frame(round(unclass(fa_ml_3_trn_MOD$loadings), 3))

fa_ml_3faclds

#Moving Final 3 factor loadings into an excel spreadsheet
#This creates an excel spreadsheet within the working directory on your desktop or drive
openxlsx::write.xlsx(fa_ml_3faclds_final, "~/Desktop/UGA IOMP/Advanced Analytics/UGA IOMP_AA_2023/HWK 1/fa_ml_3faclds_final.xlsx")

##Scale Names

#The 3 factor model with questions 6, 12, 22, and 23 removed produced a final model with 19 questions. The first factor appears to assess individuals' aversion to STEM. The second factor captures individuals' perceived mathematical ability. The third and final factor seems to access respondents' individualized experience with STEM. Now that each factor has been classified, it is necessary to measure each scale's reliability. To do this, data frames should be created that separate each item into their respective scale.

##Scale Building

#The data_training_MOD was the final data used to run the EFA that produce the simple structure 3 factor model. This data frame still contains an ID variable. This should be removed in order to build out the final scales.

#Removing ID from dataframe
STEM_scales <- data_training_MOD %>%
    dplyr::select(-c(ID))

#Reordering dataframe by scale groupings
STEM_scales_ordered <- STEM_scales %>%
    dplyr::select(Question_01, Question_04, Question_05, Question_07, Question_10, Question_13, Question_14, Question_15, Question_16, Question_18, Question_21, Question_08, Question_11, Question_17, Question_02, Question_03, Question_09, Question_19, Question_20, everything())

#The new data frame should be examined using "skim".
skim(STEM_scales_ordered)

#According to the histograms it appears that Question 20 should be reversed scored before creating scales and assessing their reliability.

#The code below will group the items within their overall factors. It is important to include a negative in front of the column number to ensure it is reverse scored if necessary.

#Creating keys list
keys_list <- list(aversion = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                  perceived = c(12, 13, 14),
                  individual = c(15, 16, 17, 18, -19) #this last item is being reverse scored
)

keys <- make.keys(STEM_scales_ordered, keys_list, item.labels = colnames(STEM_scales_ordered))

#Now the items can be scored.

#Creating scores
scores <- scoreItems(keys, STEM_scales_ordered, impute = "none", 
                     min = 1, max = 5, digits = 3) #this creates the scores object for each variable

head(scores$scores) #this reveals the first 6 values of scores calculated

scores_df <- as.data.frame(scores$scores) #this creates a new scores data frame

#Now that scores have been calculated it is necessary to split each factor out from the overall data frame to perform scale analysis on each one. First it will be helpful to rename all variables within the STEM_scales_ordered data frame.

#Renaming variables by scale
rename(STEM_scales_ordered, A1=Question_01, A2=Question_04, A3=Question_05, A4=Question_07, A5=Question_10, A6=Question_13, A7=Question_14, A8=Question_15, A9=Question_16, A10=Question_18, A11=Question_21, P1=Question_08, P2=Question_11, P3=Question_17, I1=Question_02, I2=Question_03, I3=Question_09, I4=Question_19, I5=Question_20)

#Splitting factors by grouping
AV <- STEM_scales_ordered %>%
    dplyr::select(Question_01, Question_04, Question_05, Question_07, Question_10, Question_13, Question_14, Question_15, Question_16, Question_18, Question_21)

PER <- STEM_scales_ordered %>%
    dplyr::select(Question_08, Question_11, Question_17)

IND <- STEM_scales_ordered %>%
    dplyr::select(Question_02, Question_03, Question_09, Question_19, Question_20)

##Aversion Scale Reliability
#Making keys for Aversion factor
keys_list <- list(aversion=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

keys <- make.keys(AV, keys_list, item.labels = colnames(AV))

#Creating alpha object for Aversion scale
AV_ALPHA <- psych::alpha(x = AV[, abs(keys_list$aversion)], keys = keys)

#Calculating additional reliability stats
AV_total <- round(as.data.frame(AV_ALPHA$total), 3)
AV_alpha_drop <- round(as.data.frame(AV_ALPHA$alpha.drop), 3)
AV_item_stat <- round(as.data.frame(AV_ALPHA$item.stats), 3)

AV_ALPHA

#The overall Cronbach's alpha for the Aversion scale is 0.867.  This indicates the scale has an acceptable level of reliability. 

##Perceived Scale Reliability
#Making keys for Perceived scale
keys_list <- list(perceived=c(1, 2, 3))

keys <- make.keys(PER, keys_list, item.labels = colnames(PER))

#Creating alpha object for Percieved scale
PER_ALPHA <- psych::alpha(x = PER[, abs(keys_list$perceived)], keys = keys)

#Calculating additional reliability stats
PER_total <- round(as.data.frame(PER_ALPHA$total), 3)
PER_alpha_drop <- round(as.data.frame(PER_ALPHA$alpha.drop), 3)
PER_item_stat <- round(as.data.frame(PER_ALPHA$item.stats), 3)

PER_ALPHA

#The Perceived scale has a Cronbach's alpha of 0.831. This scale has an adequate level of reliability since it is above 0.80.

##Individualized Scale Reliability
#Making keys for Individualized scale
keys_list <- list(individual=c(1, 2, 3, 4, -5))

keys <- make.keys(IND, keys_list, item.labels = colnames(IND))

#Creating alpha object for Individualized scale
IND_ALPHA <- psych::alpha(x = IND[, abs(keys_list$individual)], keys = keys)

#Calculating additional reliability stats
IND_total <- round(as.data.frame(IND_ALPHA$total), 3)
IND_alpha_drop <- round(as.data.frame(IND_ALPHA$alpha.drop), 3)
IND_item_stat <- round(as.data.frame(IND_ALPHA$item.stats), 3)

IND_ALPHA

#The Individualized scale has a Cronbach's alpha of 0.656. This scale does not have an adequate level of reliability since it falls under 0.70. 

#The scale analysis provides a good look at the 3 factor model that was chosen. After a full analysis, the scales seem to be somewhat difficult to describe and or name. This is a potential drawback from choosing to use a 3 factor model instead of the next most simplistic model, the modified 4 factor model. The 4 factor model would have potentially parsed out the scales in a different manner that would have made the scale building process easier. Additionally, the "individualized experience" scale is not very reliable. This too could have been improved upon had an additional factor been added to the model.

#Overall, the 3 factor model has an acceptable goodness of fit statistic, TLI = 0.914, and a good residuals statistic, RMSEA = 0.0598. This model had the simplest structure of all the models produced within this analysis but did have to removed 4 questions from the survey that were deemed ineffective in measuring the 3 factors. 



