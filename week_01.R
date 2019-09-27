#####################################################################
# Initial Imports and file ingest
####################################################################
# Import Libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

# Load the dataset
ames_df <-
  read_csv("ames_housing_data.csv",
           col_types = list("PoolQC" = col_character()))

# Initial view of the dataframe
head(ames_df)
glimpse(ames_df)
names(ames_df)
summary(ames_df$SalePrice)

# Create some additional columns
ames_df$TotalSF <- ames_df$FirstFlrSF + ames_df$SecondFlrSF
ames_df$HouseAge <- ames_df$YrSold - ames_df$YearBuilt
ames_df$QualityIndex <- ames_df$OverallQual * ames_df$OverallCond
ames_df$logSalePrice <- log(ames_df$SalePrice)
ames_df$price_sqft <- ames_df$SalePrice / ames_df$TotalSF
ames_df$RemodelFlag <-
  if (ames_df$YearRemodel == ames_df$YearBuilt) {
    "No"
  } else {
    "Yes"
  }

# Summary stats about the dataframe
num_obs <- nrow(ames_df)
num_obs
table(ames_df$Zoning)
summary(ames_df$SalePrice)
boxplot(ames_df$SalePrice, horizontal = TRUE)
hist(ames_df$SalePrice)
moments::skewness(ames_df$SalePrice)
moments::kurtosis(ames_df$SalePrice)
SalePrice_Outliers <- boxplot.stats(ames_df$SalePrice, coef = 3.0)

boxplot(ames_df$GrLivArea, horizontal = TRUE)

####################################################################
# Section 1: Sample & Waterfall Drop Conditions
####################################################################
# Subset the data; drop-out waterfall and 20 variables for EDA
subset_df <- ames_df %>%
  filter(
    Zoning %in% c("RH", "RL", "RP", "RM", "FV"),
    BldgType == "1Fam",
    SaleCondition == "Normal",
    GrLivArea < 4000
  ) %>%
  select(
    SalePrice,
    logSalePrice,
    LotArea,
    Neighborhood,
    Condition1,
    HouseStyle,
    YearBuilt,
    TotalSF,
    price_sqft,
    SubClass,
    LotShape,
    QualityIndex,
    TotalBsmtSF,
    CentralAir,
    BedroomAbvGr,
    TotRmsAbvGrd,
    GarageType,
    GarageCars,
    LotConfig,
    RemodelFlag,
    HeatingQC
  )

nrow(subset_df)

# Create waterfall of observations filtered out
d_1 <- ames_df %>%
  filter(Zoning %in% c("RH", "RL", "RP", "RM", "FV"))

d_2 <- d_1 %>%
  filter(BldgType == "1Fam")

d_3 <- d_2 %>%
  filter(SaleCondition == "Normal")

d_4 <- d_3 %>%
  filter(GrLivArea <= 4000)

# Drop Condition Table for write-up
drop_wf <- tibble(
  DropCondition = c(
    "Non-Residential Zoning",
    "Multi-Family Homes",
    "Non-Normal Sale Condition",
    "Data Documentation Preferred Exclusions"
  ),
  ObservationsDropped = c(
    num_obs - nrow(d_1),
    nrow(d_1) - nrow(d_2),
    nrow(d_2) - nrow(d_3),
    nrow(d_3) - nrow(d_4)
  ),
  RemainingObservations = c(nrow(d_1),
                            nrow(d_2),
                            nrow(d_3),
                            nrow(d_4))
)

drop_wf
table(ames_df$SaleCondition)

#####################################################################
# Section 2: Data Quality Checks
####################################################################
names(subset_df)

# Summary table of numeric variables
myfunct1 <- function(x) {
  c(
    "Stand dev" = sd(x, na.rm = TRUE),
    "Mean" = mean(x, na.rm = TRUE),
    "Median" = median(x),
    "Minimum" = min(x, na.rm = TRUE),
    "Maximun" = max(x, na.rm = TRUE)
  )
}

summary_numeric_df <- subset_df %>%
  select_if(is.numeric) %>%
  map( ~ myfunct1(.)) %>%
  as.data.frame()

row_names <- row.names(summary_numeric_df)

summary_numeric_df <- cbind(summary_numeric_df, row_names) %>%
  gather(key = key, value = value, -row_names) %>%
  spread(key = row_names, value = value)

summary_numeric_df
summary_numeric_df[summary_numeric_df$key == "LotArea", ]

# SalePrice
summary(subset_df$SalePrice)
sd(subset_df$SalePrice)

par(mfrow=c(1,2))
boxplot(subset_df$logSalePrice, horizontal = TRUE, col = "orange", xlab = "Log Sale Price", main = "Log Sale Price")
hist(subset_df$logSalePrice,
     main = "Log Sale Price",
     xlab = "Log Sale Price",
     col = "orange")
moments::skewness(subset_df$logSalePrice)
boxplot.stats(subset_df$SalePrice, coef = 3.0)
sum(is.na(subset_df$SalePrice))

# Lot Area
boxplot(subset_df$LotArea, horizontal = TRUE)
length(boxplot.stats(subset_df$LotArea, coef = 3.0)$out)
sum(is.na(subset_df$LotArea))

# Neighborhood
summary(subset_df$Neighborhood)
table(subset_df$Neighborhood)

# Quality Index
summary(subset_df$QualityIndex)
sum(is.na(subset_df$QualityIndex))
subset_df %>%
  ggplot(aes(x = QualityIndex, color = "orange")) +
  geom_bar(fill = "orange") +
  ggtitle("Quality Index Volume") +
  theme_classic()

pnorm(
  25,
  mean = mean(subset_df$QualityIndex),
  sd = sd(subset_df$QualityIndex)
)
pnorm(
  50,
  mean = mean(subset_df$QualityIndex),
  sd = sd(subset_df$QualityIndex)
) - pnorm(
  25,
  mean = mean(subset_df$QualityIndex),
  sd = sd(subset_df$QualityIndex)
)

# Condition 1
table(subset_df$Condition1)
subset_df %>%
  ggplot(aes(x = Condition1, color = "orange")) +
  geom_bar(fill = "orange") +
  coord_flip() +
  ggtitle("Proximity Conditions") +
  theme_classic()

# House Style
table(subset_df$HouseStyle)
sum(is.na(subset_df$HouseStyle))

# Year Built
summary(subset_df$YearBuilt)
quantile(subset_df$YearBuilt)
subset_df %>%
  ggplot(aes(x = YearBuilt, color = "orange")) +
  geom_bar(fill = "orange") +
  ggtitle("Year Built") +
  theme_classic()

# Total SqFt
summary(subset_df$TotalSF)
sd(subset_df$TotalSF)
boxplot(subset_df$TotalSF, coef = 3.0, horizontal = TRUE)
length(boxplot.stats(subset_df$TotalSF, coef = 3.0)$out)

par(mfrow=c(1,2))
boxplot(subset_df$TotalSF, horizontal = TRUE, col = "orange", xlab = "Total SqFt", main = "Total SqFt")
hist(subset_df$TotalSF,
     main = "Total SqFt",
     xlab = "Total SqFt",
     col = "orange")
moments::skewness(subset_df$TotalSF)

# Subclass
table(subset_df$SubClass)
subset_df %>%
  ggplot(aes(x = SubClass, color = "orange")) +
  geom_bar(fill = "orange") +
  coord_flip() +
  ggtitle("Sub-Class Volumes") +
  theme_classic()

# Lot shape
table(subset_df$LotShape)

# Basement sqft
summary(subset_df$TotalBsmtSF)
sd(subset_df$TotalBsmtSF)
sum(is.na(subset_df$TotalBsmtSF))

# Central Air
table(subset_df$CentralAir)

# Total Rooms above ground
summary(subset_df$TotRmsAbvGrd)

# Garage Type
table(subset_df$GarageType)

# Lot config
table(subset_df$LotConfig)

# Heating quality
table(subset_df$HeatingQC)

# Price per sqft
par(mfrow=c(1,2))
boxplot(subset_df$price_sqft, horizontal = TRUE, col = "orange", xlab = "Price per SqFt", main = "Price per SqFt")
hist(subset_df$price_sqft,
     main = "Price per SqFt",
     xlab = "Price per SqFt",
     col = "orange")
moments::skewness(subset_df$price_sqft)

# Neighbohood
subset_df %>%
  ggplot(aes(x = Neighborhood, color = "orange")) +
  geom_bar(fill = "orange") +
  coord_flip() +
  ggtitle("Neighborhood Volumes") +
  theme_classic()

# House Style
subset_df %>%
  ggplot(aes(x = HouseStyle, color = "orange")) +
  geom_bar(fill = "orange") +
  coord_flip() +
  ggtitle("House Style") +
  theme_classic()

# Sales vs year built
subset_df %>%
  ggplot(aes(x = YearBuilt, y = SalePrice)) +
  geom_point()


plot(x = subset_df$QualityIndex, y = subset_df$SalePrice, col = "orange",
     main = "Quality Index vs Sales Price",
     xlab = "Quality Index ",
     ylab = "Sales Price")
plot(x = subset_df$QualityIndex, y = subset_df$logSalePrice, col = "blue",
     main = "Quality Index  vs Log Sales Price",
     xlab = "Quality Index ",
     ylab = "Log Sales Price")

cor(subset_df$QualityIndex, y = subset_df$logSalePrice)






