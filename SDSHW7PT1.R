library(tidyverse)
library(ggplot2)
library(readr)
armfold <- read_csv("SDS313/armfold.csv")

sex_counts <- table(armfold$Sex)
print(sex_counts)

male_prop <- mean(armfold$LonR_fold[armfold$Sex == "Male"])
female_prop <- mean(armfold$LonR_fold[armfold$Sex == "Female"])

cat("Proportion (males, left on top):", round(male_prop, 3), "\n")
cat("Proportion (females, left on top):", round(female_prop, 3), "\n")

obs_diff <- male_prop - female_prop
cat("Observed difference (male - female):", round(obs_diff, 3), "\n")

n_male <- sum(armfold$Sex == "Male")
n_female <- sum(armfold$Sex == "Female")

SE <- sqrt((male_prop * (1 - male_prop)) / n_male +
             (female_prop * (1 - female_prop)) / n_female)

z_star <- 1.96 

CI_lower <- obs_diff - z_star * SE
CI_upper <- obs_diff + z_star * SE

cat("95% CI:", round(CI_lower, 3), "to", round(CI_upper, 3), "\n")

table2 <- table(armfold$Sex, armfold$LonR_fold)
prop.test(x = c(table2["Male", "1"], table2["Female", "1"]),
          n = c(sum(table2["Male", ]), sum(table2["Female", ])),
          correct = FALSE)
