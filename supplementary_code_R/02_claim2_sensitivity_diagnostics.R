# Claim 2: standalone extended sensitivity diagnostics drafted in this conversation.
# This is NOT the original I4R code, although it independently reimplements
# some checks so that the additional descriptives can be inspected in context.
# Required packages: readxl, afex.

library(readxl)
library(afex)
path <- "pkg/Dataset_Abreu et al.xlsx"
if (!file.exists(path)) stop("Missing dataset: ", path)
AB <- as.data.frame(read_excel(path))
AB$Grp <- factor(AB[["Physical Activity"]])
AB$id <- seq_len(nrow(AB))

# Column order documented in the paper and I4R script:
# C pre, C post, AE pre, AE post, AO pre, AO post.
x <- AB[, grep("^VAS", names(AB))]
stopifnot(ncol(x) == 6)
AB$Control_change <- x[, 2] - x[, 1]
AB$AE_change <- x[, 4] - x[, 3]
AB$AO_change <- x[, 6] - x[, 5]
AB$AE_CRD <- AB$AE_change - AB$Control_change
AB$AO_CRD <- AB$AO_change - AB$Control_change
AB$Mean_CRD <- (AB$AE_CRD + AB$AO_CRD) / 2
AB$Mean_raw <- (AB$AE_change + AB$AO_change) / 2

run_mixed <- function(data) {
  n <- nrow(data)
  long <- data.frame(
    id = factor(rep(seq_len(n), 2)),
    Group = factor(rep(data$Grp, 2)),
    Condition = factor(rep(c("AE", "AO"), each = n)),
    CRD = c(data$AE_CRD, data$AO_CRD)
  )
  fit <- suppressMessages(
    aov_ez(id = "id", dv = "CRD", data = long,
           between = "Group", within = "Condition",
           anova_table = list(es = "pes"))
  )
  as.data.frame(fit$anova_table)
}

cat("\n=== Original mixed ANOVA ===\n")
original <- run_mixed(AB)
print(original)
# The afex ANOVA factor row uses the label Group.
p_original <- original["Group", "Pr(>F)"]

cat("\n=== Rank-based group test (average CRD) ===\n")
mw <- wilcox.test(Mean_CRD ~ Grp, data = AB, exact = FALSE)
print(mw)

cat("\n=== Leave-one-out analysis (one record at a time) ===\n")
loo <- data.frame(Excluded_row = seq_len(nrow(AB)),
                  Excluded_group = as.character(AB$Grp), p_value = NA_real_)
for (i in seq_len(nrow(AB))) {
  loo$p_value[i] <- run_mixed(AB[-i, , drop = FALSE])["Group", "Pr(>F)"]
}
print(loo, row.names = FALSE)
cat("Count with p >= .05:", sum(loo$p_value >= .05), "\n")
cat("Maximum p:", max(loo$p_value), "\n")
print(subset(loo, p_value >= .05))

cat("\n=== Group test without control subtraction ===\n")
raw_model <- aov(Mean_raw ~ Grp, data = AB)
print(summary(raw_model))

cat("\n=== Two-outcome Bonferroni adjustment ===\n")
cat("Adjusted p:", min(1, 2 * p_original), "\n")

# Additional detail beyond the compact I4R verification block:
cat("\n=== Group mean changes, raw and control-adjusted ===\n")
print(aggregate(cbind(Control_change, AE_change, AO_change,
                      Mean_raw, Mean_CRD) ~ Grp, data = AB, FUN = mean))
cat("\n=== Group variability, raw and control-adjusted ===\n")
print(aggregate(cbind(Mean_raw, Mean_CRD) ~ Grp,
                data = AB, FUN = sd))
