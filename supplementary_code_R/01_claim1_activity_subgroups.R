# Claim 1: supplementary activity-classification and subgroup analyses
# Authored in this ChatGPT conversation, extending (not copying) the I4R checks.
# Run from the directory containing pkg/Dataset_Abreu et al.xlsx.
# Required package: readxl

library(readxl)

path <- "pkg/Dataset_Abreu et al.xlsx"
if (!file.exists(path)) stop("Missing dataset: ", path)
AB <- as.data.frame(read_excel(path))
stopifnot(nrow(AB) == 30)

AB$Original <- as.character(AB[["Physical Activity"]])
AB$Equivalent <- AB$Moderate + 2 * AB$Vigorous
AB$Meets_WHO <- AB$Equivalent >= 150

# Alternative binary rules compared against the deposited labels.
rules <- data.frame(
  Original = AB$Original == "Active",
  Any_vigorous = AB$Vigorous > 1,
  Any_MVPA = AB$Moderate > 1 | AB$Vigorous > 1,
  WHO = AB$Equivalent >= 150,
  MVPA_75 = AB$Moderate + AB$Vigorous >= 75,
  Total_150 = AB$Light + AB$Moderate + AB$Vigorous >= 150
)

cat("\n=== Table 1: Alternative classifications ===\n")
for (v in names(rules)[-1]) {
  t <- table(Original = rules$Original, Predicted = rules[[v]])
  cat("\nRULE: ", v, "\n", sep = "")
  print(t)
  cat("Matches:", sum(rules$Original == rules[[v]]), "/", nrow(AB), "\n")
}

# The three groups distinguish the eight disputed Active participants.
AB$Subgroup <- ifelse(
  AB$Original == "Sedentary", "C: Original Sedentary",
  ifelse(AB$Meets_WHO, "A: Active meets WHO", "B: Active below WHO")
)
cat("\n=== Table 2: Median activity by subgroup ===\n")
print(aggregate(cbind(Moderate, Vigorous, Equivalent) ~ Subgroup,
                data = AB, FUN = median))
cat("\nSubgroup sizes:\n")
print(table(AB$Subgroup))

# Compare eight disputed Active participants with 14 originally Sedentary.
B <- AB$Original == "Active" & !AB$Meets_WHO
C <- AB$Original == "Sedentary"
variables <- c("Light", "Moderate", "Vigorous", "Equivalent")
cat("\n=== Table 3: Wilcoxon rank-sum comparisons ===\n")
for (v in variables) {
  test <- wilcox.test(AB[[v]][B], AB[[v]][C], exact = FALSE)
  cat(sprintf("%-12s W = %5.1f, p = %.8f\n",
              v, unname(test$statistic), test$p.value))
}
cat("\nRange-separation diagnostic (equivalent activity):\n")
cat("Minimum among disputed Active:", min(AB$Equivalent[B]), "\n")
cat("Maximum among originally Sedentary:", max(AB$Equivalent[C]), "\n")

# Interpretation note: value 1 has no deposited codebook definition.
# Classifications and p-values are exploratory, not confirmation of the authors' rule.
