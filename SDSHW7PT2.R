library(tidyverse)
library(ggplot2)
library(MatchIt)
library(readr)
turnout <- read_csv("SDS313/turnout.csv")


p1 <- mean(turnout$voted1998[turnout$GOTV_call == 1])
p0 <- mean(turnout$voted1998[turnout$GOTV_call == 0])
diff <- p1 - p0

n1 <- sum(turnout$GOTV_call == 1)
n0 <- sum(turnout$GOTV_call == 0)

se <- sqrt(p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0)

ci <- c(diff - 1.96 * se, diff + 1.96 * se)

cat("Proportion Voted (GOTV = 1):", round(p1, 3), "\n")
cat("Proportion Voted (GOTV = 0):", round(p0, 3), "\n")
cat("Difference in proportions:", round(diff, 3), "\n")
cat("95% Confidence Interval:", round(ci, 3), "\n\n")


cat("Group Summary Statistics by GOTV_call\n")
turnout %>%
  group_by(GOTV_call) %>%
  summarise(
    mean_voted1996 = mean(voted1996),
    mean_AGE = mean(AGE),
    mean_MAJORPTY = mean(MAJORPTY)
  )

confounder_CI <- function(var) {
  t1 <- turnout[turnout$GOTV_call == 1, var]
  t0 <- turnout[turnout$GOTV_call == 0, var]
  diff <- mean(t1) - mean(t0)
  se <- sqrt(var(t1)/length(t1) + var(t0)/length(t0))
  ci <- c(diff - 1.96 * se, diff + 1.96 * se)
  cat(var, "→ Difference:", round(diff, 3), " 95% CI:", round(ci, 3), "\n")
}

cat("\n Confidence Intervals for Confounder Differences:\n")
confounder_CI("voted1996")
confounder_CI("AGE")
confounder_CI("MAJORPTY")

match_out <- matchit(GOTV_call ~ voted1996 + AGE + MAJORPTY,
                     data = turnout, method = "nearest", ratio = 5)

matched_data <- match.data(match_out)

cat("\n Matched Summary Statistics by GOTV_call\n")
matched_data %>%
  group_by(GOTV_call) %>%
  summarise(
    mean_voted1996 = mean(voted1996),
    mean_AGE = mean(AGE),
    mean_MAJORPTY = mean(MAJORPTY)
  )

confounder_CI_matched <- function(var) {
  t1 <- matched_data[matched_data$GOTV_call == 1, var]
  t0 <- matched_data[matched_data$GOTV_call == 0, var]
  diff <- mean(t1) - mean(t0)
  se <- sqrt(var(t1)/length(t1) + var(t0)/length(t0))
  ci <- c(diff - 1.96 * se, diff + 1.96 * se)
  cat(var, "→ Matched Difference:", round(diff, 3), " 95% CI:", round(ci, 3), "\n")
}

p1_matched <- mean(matched_data$voted1998[matched_data$GOTV_call == 1])
p0_matched <- mean(matched_data$voted1998[matched_data$GOTV_call == 0])
diff_matched <- p1_matched - p0_matched

n1_matched <- sum(matched_data$GOTV_call == 1)
n0_matched <- sum(matched_data$GOTV_call == 0)

se_matched <- sqrt(p1_matched * (1 - p1_matched) / n1_matched +
                     p0_matched * (1 - p0_matched) / n0_matched)
ci_matched <- c(diff_matched - 1.96 * se_matched,
                diff_matched + 1.96 * se_matched)

cat("Proportion Voted (GOTV = 1):", round(p1_matched, 3), "\n")
cat("Proportion Voted (GOTV = 0):", round(p0_matched, 3), "\n")
cat("Difference in proportions:", round(diff_matched, 3), "\n")
cat("95% Confidence Interval:", round(ci_matched, 3), "\n")
