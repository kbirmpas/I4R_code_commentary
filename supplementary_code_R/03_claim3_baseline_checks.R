# Claim 3: expanded baseline and pre/post diagnostic analysis drafted here.
# This script supplies descriptive tables and independent companion tests.
# Required package: readxl.

library(readxl)
path <- "pkg/Dataset_Abreu et al.xlsx"
if (!file.exists(path)) stop("Missing dataset: ", path)
AB <- as.data.frame(read_excel(path))
x <- AB[, grep("^VAS", names(AB))]
stopifnot(ncol(x) == 6)
AB$C_pre <- x[, 1]; AB$C_post <- x[, 2]
AB$AE_pre <- x[, 3]; AB$AE_post <- x[, 4]
AB$AO_pre <- x[, 5]; AB$AO_post <- x[, 6]

AB$C_change <- AB$C_post - AB$C_pre
AB$AE_change <- AB$AE_post - AB$AE_pre
AB$AO_change <- AB$AO_post - AB$AO_pre
AB$AE_CRD <- AB$AE_change - AB$C_change
AB$AO_CRD <- AB$AO_change - AB$C_change
AB$CRD_contrast <- AB$AO_CRD - AB$AE_CRD
AB$Raw_contrast <- AB$AO_change - AB$AE_change
AB$Pre_difference <- AB$AO_pre - AB$AE_pre
AB$Post_difference <- AB$AO_post - AB$AE_post

cat("\n=== Control cancellation (maximum absolute discrepancy) ===\n")
print(max(abs(AB$CRD_contrast - AB$Raw_contrast)))

cat("\n=== Pre/post means by condition ===\n")
print(data.frame(Condition = c("AE", "AO"),
                 Pre = c(mean(AB$AE_pre), mean(AB$AO_pre)),
                 Post = c(mean(AB$AE_post), mean(AB$AO_post)),
                 Change = c(mean(AB$AE_change), mean(AB$AO_change))))
cat("Mean AO - AE change-score contrast:", mean(AB$Raw_contrast), "\n")

cat("\n=== Paired pre-test difference ===\n")
print(t.test(AB$Pre_difference))
cat("\nArithmetic baseline share (%; NOT causal attribution):\n")
share <- -mean(AB$Pre_difference)/mean(AB$Raw_contrast)*100
print(share)

cat("\n=== Paired post-test difference ===\n")
print(t.test(AB$Post_difference))
cat("\n=== Paired change-score difference ===\n")
print(t.test(AB$Raw_contrast))
cat("\n=== Wilcoxon signed-rank companion ===\n")
print(wilcox.test(AB$Raw_contrast, exact = FALSE))

cat("\n=== Regression adjustment for pre-test difference ===\n")
model <- lm(Post_difference ~ Pre_difference, data = AB)
print(summary(model)); print(confint(model))
coef_tab <- coef(summary(model))
cat("Adjusted contrast (intercept):",coef_tab["(Intercept)","Estimate"],"\n")
cat("Adjusted p:",coef_tab["(Intercept)","Pr(>|t|)"],"\n")
# Interpret the intercept as a post-test difference at zero pre-test difference.
# Unlike a causal attribution, this is model-based and sensitive to assumptions.
