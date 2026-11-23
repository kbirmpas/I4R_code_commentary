# Claim 4: Independent G*Power-style repeated-measures power audit
# Only uses base R (no packages). Run: source('claim4_power_checks.R')
# Assumptions: alpha = .05; two groups; two repeated measurements;
# one between-group effect, one within-subject condition effect.

rm_power <- function(f, n, effect = c('within', 'between'),
                     groups = 2, measures = 2, rho = .5,
                     epsilon = 1, alpha = .05) {
  effect <- match.arg(effect)
  if (f < 0 || n <= groups || measures < 2 ||
      rho <= -1 / (measures - 1) || rho >= 1)
    stop('Invalid parameters')

  if (effect == 'between') {
    df1 <- groups - 1
    df2 <- n - groups
    lambda <- (f^2 * n * measures) / (1 + (measures - 1) * rho)
  } else {
    df1 <- (measures - 1) * epsilon
    df2 <- (n - groups) * (measures - 1) * epsilon
    lambda <- (f^2 * n * measures * epsilon) / (1 - rho)
  }
  critical <- qf(1 - alpha, df1, df2)
  pf(critical, df1, df2, ncp = lambda, lower.tail = FALSE)
}

cat('\n=== A. Independent reproduction, rho=.5, 2 groups, 2 measures ===\n')
fs <- c(0.025, 0.05, 0.25, 0.50)
labels <- c('d=.05 -> f=.025 (2-group conversion)', 'f=.05',
            'medium f=.25', 'large f=.50')
tab <- data.frame(
  assumption = labels,
  f = fs,
  condition_N12_pct = 100 * sapply(fs, function(z) rm_power(z,12,'within')),
  group_N26_pct = 100 * sapply(fs, function(z) rm_power(z,26,'between'))
)
print(tab, row.names = FALSE, digits = 5)

cat('\n=== B. Change rho while retaining medium f=.25 ===\n')
rhos <- c(0, 0.25, 0.50, 0.75, 0.875)
rho_tab <- data.frame(
  rho = rhos,
  condition_N12_pct = 100*sapply(rhos, function(r) rm_power(.25,12,'within',rho=r)),
  group_N26_pct = 100*sapply(rhos, function(r) rm_power(.25,26,'between',rho=r))
)
print(rho_tab, row.names=FALSE, digits=5)

cat('\n=== C. Rho needed to reproduce each published power at medium f=.25 ===\n')
# Targets are to a little more precision than published, from the reconstructed f=.5, rho=.5 case.
target_condition <- 0.8764
target_group <- 0.8063
rho_condition <- uniroot(function(r) rm_power(.25,12,'within',rho=r)-target_condition,
                          interval=c(-0.999,0.999))$root
rho_group <- uniroot(function(r) rm_power(.25,26,'between',rho=r)-target_group,
                      interval=c(-0.999,0.999))$root
cat(sprintf('For condition N=12: rho = %.4f\n', rho_condition))
cat(sprintf('For group N=26:     rho = %.4f\n', rho_group))
cat('A single correlation cannot satisfy both at medium f=.25.\n')

cat('\n=== D. What if power planning used THREE repeated conditions? ===\n')
# This is an alternative mathematical reconstruction, NOT evidence that authors used it.
objective3 <- function(x) {
  f <- x[1]; rho <- x[2]
  (rm_power(f,12,'within',measures=3,rho=rho)-target_condition)^2 +
    (rm_power(f,26,'between',measures=3,rho=rho)-target_group)^2
}
fit3 <- optim(c(0.46,0.45), objective3, method='L-BFGS-B',
              lower=c(0.01,-0.49), upper=c(2,0.99),
              control=list(factr=1e7, pgtol=1e-13))
cat(sprintf('Three-measure solution: f = %.4f; rho = %.4f\n',
            fit3$par[1],fit3$par[2]))
cat(sprintf('Power: condition=%.2f%%; group=%.2f%%\n',
            100*rm_power(fit3$par[1],12,'within',measures=3,rho=fit3$par[2]),
            100*rm_power(fit3$par[1],26,'between',measures=3,rho=fit3$par[2])))
cat('But the reported mixed ANOVA uses TWO CRD conditions, so this is not the reported model.\n')

cat('\n=== E. At the actual N=30, still under rho=.5 ===\n')
cat(sprintf('Medium f=.25: condition %.2f%%; group %.2f%%\n',
            100*rm_power(.25,30,'within'),100*rm_power(.25,30,'between')))
cat(sprintf('Large f=.5:   condition %.2f%%; group %.2f%%\n',
            100*rm_power(.5,30,'within'),100*rm_power(.5,30,'between')))

cat('\n=== F. Optional: observed sample correlation between AO and AE CRDs ===\n')
path <- 'pkg/Dataset_Abreu et al.xlsx'
if (file.exists(path) && requireNamespace('readxl',quietly=TRUE)) {
  df <- as.data.frame(readxl::read_excel(path))
  v <- df[,grepl('^VAS',names(df))]
  if (ncol(v)==6) {
    dc <- v[,2]-v[,1]
    ae <- (v[,4]-v[,3])-dc
    ao <- (v[,6]-v[,5])-dc
    cat(sprintf('Observed correlation of AE and AO CRDs: %.4f\n',cor(ae,ao)))
  } else cat('Could not identify six VAS columns.\n')
} else cat('Skipped: workbook not present at pkg/ or readxl unavailable.\n')
