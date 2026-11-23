## ============================================================================================
## CLAIMS SCRIPT — verify every claim in the AI report, one numbered block at a time.
## Claim numbers match the [claim N] tags in the report, the Appendix-A findings table, and the
## rows of the verdict survey.
##
## THE RULE: if you cannot verify a claim marked RETRACTION or CORRIGENDUM MATERIAL after
## honest study, RETURN THE PAPER and take another. Do not skip it. Do not let AI decide.
##
## NEVER contact the paper's authors or the journal. I4R handles all correspondence.
##
## 9 claims: 0 retraction material, 6 corrigendum material, 3 comment material. File language: .R (the deposit's own).
## ============================================================================================

## ---- standalone check harness: the verification notebook's setup chunk -------------------------
## How to run it: Rscript claims_script.R from any folder, with the deposit's folder in I4R_PKG_DIR (or a pkg/
## folder beside the working directory). Checks that read the article find it in paper/ beside pkg/ (or in
## I4R_AUDIT_DIR/paper/): the PDF as paper/paper.pdf and its text as paper/paper.txt, made with
##     pdftotext -layout paper/paper.pdf paper/paper.txt
## Run it in a UTF-8 locale. Every check passed, with these helpers, in the audit's verification notebook.
if (!isTRUE(l10n_info()[["UTF-8"]]))
  message("This R session is not in a UTF-8 locale; checks on text with non-ASCII characters may FAIL. ",
          "Run it with LANG=en_US.UTF-8 (or C.UTF-8).")

## The packages these checks use. A missing one is named here; the claims that need it record a FAIL.
.I4R_NEED <- c("readxl", "afex")
.I4R_MISSING <- .I4R_NEED[!vapply(.I4R_NEED, requireNamespace, logical(1), quietly = TRUE)]
if (length(.I4R_MISSING)) message(
  "These R packages are used below but not installed: ", paste(.I4R_MISSING, collapse = ", "),
  "\nInstall them, then run this file again: install.packages(", deparse(.I4R_MISSING), ")",
  "\n(a Bioconductor package installs with BiocManager::install()). Until then the claims that need them FAIL.")
suppressPackageStartupMessages(for (.p in c("knitr", "readxl")) if (requireNamespace(.p, quietly = TRUE)) library(.p, character.only = TRUE))

## Resolve the deposit WITHOUT hardcoding the auditor's machine: I4R_PKG_DIR, else a pkg/ folder beside the
## working directory (or the audit layout's), else stop.
PKG_DIR <- local({
  e <- Sys.getenv("I4R_PKG_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  for (cand in c(file.path(getwd(), "pkg"), file.path(dirname(getwd()), "audit", "pkg"), "pkg"))
    if (dir.exists(cand)) return(normalizePath(cand))
  stop("cannot locate the deposit: set I4R_PKG_DIR to the extracted package directory (the pkg/ folder)")
})
AUDIT_DIR <- local({
  e <- Sys.getenv("I4R_AUDIT_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  normalizePath(dirname(PKG_DIR))
})
SNIPS_DIR <- local({
  e <- Sys.getenv("I4R_SNIPS_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(e)
  cand <- file.path(AUDIT_DIR, "verification_snippets")
  if (dir.exists(cand)) normalizePath(cand) else cand
})
## The notebook's chunks run from the audit folder; so do these checks.
setwd(AUDIT_DIR)


## ---- read-only guard: pkg/ is the authors' deposit. Fingerprint on entry, re-check in the summary; scratch goes
## to tempdir(), never beside the deposit.
.pkg_manifest <- function() {
  fs <- list.files(PKG_DIR, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  data.frame(file = fs, size = file.size(fs), md5 = unname(tools::md5sum(fs)), stringsAsFactors = FALSE)
}
.PKG_ON_ENTRY <- .pkg_manifest()
SCRATCH <- tempfile("verif_"); dir.create(SCRATCH)

## ---- deposit loader: always R, whatever the package was written in.
read_deposit <- function(file, ...) {
  path <- if (file.exists(file.path(PKG_DIR, file))) file.path(PKG_DIR, file) else file
  if (!file.exists(path)) stop("not in the deposit: ", file)
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    xlsx = , xls  = readxl::read_excel(path, ...),
    dta          = haven::read_dta(path, ...),
    sav = , por  = haven::read_sav(path, ...),
    sas7bdat     = haven::read_sas(path, ...),
    omv          = read_omv_deposit(path, ...),
    csv          = utils::read.csv(path, ...),
    tsv = , txt  = utils::read.delim(path, ...),
    rds          = readRDS(path),
    rdata = , rda = { e <- new.env(); load(path, envir = e); as.list(e) },
    parquet      = arrow::read_parquet(path, ...),
    stop("no loader for .", ext, " -- add one to read_deposit()")
  )
}
## A jamovi .omv is a zip: prefer the package; fall back to the zip so a missing jmvReadWrite does not block a check.
read_omv_deposit <- function(path, ...) {
  if (requireNamespace("jmvReadWrite", quietly = TRUE))
    return(jmvReadWrite::read_omv(path, ...))
  message("jmvReadWrite unavailable; reading the .omv as a zip archive")
  ex <- file.path(SCRATCH, paste0("omv_", basename(path)))
  dir.create(ex, showWarnings = FALSE); utils::unzip(path, exdir = ex)
  list(parts = list.files(ex, recursive = TRUE, full.names = TRUE))
}

## ---- expectation / check machinery: the notebook's verdict rules, one printed line per value.
.V <- new.env(parent = emptyenv())
.V$exp <- list(); .V$obs <- list(); .V$state <- list(); .V$order <- character(0); .V$current <- NULL
## A claim's block may record outcomes only under its own claim id.
.bind_claim <- function(id) {
  if (!is.null(.V$current) && !identical(id, .V$current))
    stop(sprintf("the %s block recorded an outcome under %s", .V$current, id))
}
.fmt <- function(x) {
  v <- if (is.numeric(x)) {
    ifelse(is.na(x), "NA",
      ifelse(x == round(x) & abs(x) < 1e15, format(x, big.mark = ",", scientific = FALSE),
             format(round(x, 6), nsmall = 0, scientific = FALSE)))
  } else as.character(x)
  paste(v, collapse = ", ")
}
.show <- function(x) if (is.null(x)) "NULL" else paste(format(x), collapse = ", ")
## expect() runs BEFORE anything is computed and prints the prediction.
expect <- function(id, ..., tol = 0, note = NULL) {
  .bind_claim(id)
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("expect(): every value needs a name")
  for (k in names(vals)) {
    key <- paste(id, k, sep = "|")
    .V$exp[[key]] <- list(id = id, key = k, expected = vals[[k]], tol = tol, note = note)
    if (!key %in% .V$order) .V$order <- c(.V$order, key)
    cat(sprintf("[%-6s %-34s] EXPECTED  %s\n", id, k, .show(vals[[k]])))
  }
  invisible(TRUE)
}
## check() prints the observation and the verdict, and never stops the run.
check <- function(id, ...) {
  .bind_claim(id)
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("check(): every value needs a name")
  for (k in names(vals)) {
    key <- paste(id, k, sep = "|"); e <- .V$exp[[key]]; ob <- vals[[k]]
    if (is.null(e)) {
      .V$obs[[key]] <- list(id = id, key = k, observed = vals[[k]], verdict = "FAIL", detail = "never declared with expect()")
      if (!key %in% .V$order) .V$order <- c(.V$order, key)
      cat(sprintf("[%-6s %-34s] FAIL  undeclared\n", id, k)); next
    }
    ex <- e$expected; tol <- e$tol
    if (is.numeric(ex) && is.numeric(ob) && length(ex) == length(ob)) {
      d <- max(abs(ex - ob)); ok <- isTRUE(all(!is.na(d)) && d <= tol + 1e-12)
    } else ok <- identical(as.character(ex), as.character(ob))
    .V$obs[[key]] <- list(id = id, key = k, observed = ob, verdict = if (ok) "PASS" else "FAIL")
    cat(sprintf("[%-6s %-34s] OBSERVED  %s  %s\n", id, k, .show(ob), if (ok) "PASS" else "FAIL"))
  }
  invisible(TRUE)
}
## Reach back into an earlier claim's observation.
observed_of <- function(claim, key) {
  o <- .V$obs[[paste(claim, key, sep = "|")]]
  if (is.null(o)) NA else o$observed
}
## The claim rests on an assessment, stated so a replicator can reject it on the merits.
judgement <- function(id, statement, basis = "") {
  .bind_claim(id)
  .V$state[[id]] <- list(id = id, verdict = "JUDGEMENT")
  cat(sprintf("[%-6s %-34s] JUDGEMENT  %s\n", id, "-", statement))
  if (nzchar(basis)) cat(sprintf("[%-6s %-34s] basis      %s\n", id, "-", basis))
  invisible(TRUE)
}
## The deposit cannot settle it; the reason is given.
unverifiable <- function(id, reason, observed = NULL) {
  .bind_claim(id)
  .V$state[[id]] <- list(id = id, verdict = "UNVERIFIABLE")
  cat(sprintf("[%-6s %-34s] UNVERIFIABLE  %s\n", id, "-", reason))
  invisible(TRUE)
}
## A runtime error inside a claim's block is that claim's FAIL, never the end of the run.
runtime_failure <- function(id, error) {
  .V$state[[id]] <- list(id = id, verdict = "FAIL")
  cat(sprintf("[%-6s %-34s] FAIL  runtime error: %s\n", id, "-", conditionMessage(error)))
  invisible(FALSE)
}
claims_summary <- function(claims = character(0)) {
  for (key in .V$order) if (is.null(.V$obs[[key]]))
    .V$obs[[key]] <- list(id = .V$exp[[key]]$id, key = .V$exp[[key]]$key, verdict = "FAIL")
  verdicts <- vapply(.V$obs, function(o) o$verdict, character(1))
  states <- vapply(.V$state, function(s) s$verdict, character(1))
  recorded <- unique(c(vapply(.V$obs, function(o) o$id, character(1)), names(.V$state)))
  silent <- setdiff(claims, recorded)
  foreign <- if (length(claims)) setdiff(recorded, claims) else character(0)
  after <- .pkg_manifest()
  sorted <- function(m) { m <- m[order(enc2utf8(m$file), method = "radix"), c("file", "size", "md5")]; rownames(m) <- NULL; m }
  same <- identical(sorted(.PKG_ON_ENTRY), sorted(after))
  prelude_ok <- isTRUE(get0(".I4R_PRELUDE_OK", ifnotfound = TRUE))
  n_pass <- sum(verdicts == "PASS")
  n_fail <- sum(verdicts != "PASS") + sum(states == "FAIL") + length(silent) + length(foreign) + (!same) + (!prelude_ok)
  cat(sprintf("\n%d checks, %d PASS, %d FAIL\n", n_pass + n_fail, n_pass, n_fail))
  cat(sprintf("claims marked JUDGEMENT: %d; UNVERIFIABLE: %d\n", sum(states == "JUDGEMENT"), sum(states == "UNVERIFIABLE")))
  if (!prelude_ok) cat("the shared helpers (_prelude.R) stopped with an error; claims that use them FAIL\n")
  if (any(states == "FAIL")) cat("claims that stopped with an error:", paste(names(states)[states == "FAIL"], collapse = ", "), "\n")
  if (length(silent)) cat("claims that recorded no outcome:", paste(silent, collapse = ", "), "\n")
  if (length(foreign)) cat("outcomes recorded under ids that are not claims here:", paste(foreign, collapse = ", "), "\n")
  cat(sprintf("deposit unmodified: %s\n", if (same) "yes" else "NO -- INVESTIGATE"))
  invisible(n_fail)
}
## ---- audit-specific helpers (verification_snippets/_prelude.R) ----
.I4R_PRELUDE_OK <- tryCatch({
## Audit-specific helpers: Abreu, Monteiro & Rando (2023), action observation and proneness for exercise.
## One workbook, no code deposited. Only aggregate values are printed.
suppressMessages({ library(readxl); library(afex) })
AB <- as.data.frame(read_excel(file.path(PKG_DIR, "Dataset_Abreu et al.xlsx")))
names(AB)[2] <- "Sex"; AB$Grp <- AB[["Physical Activity"]]
crd <- function(v) { x <- AB[, grep(paste0("^", v), names(AB))]
  list(ae = (x[, 4] - x[, 2]) - (x[, 3] - x[, 1]), ao = (x[, 6] - x[, 2]) - (x[, 5] - x[, 1]), x = x) }
mixed <- function(v, grp = AB$Grp, keep = rep(TRUE, nrow(AB))) { z <- crd(v)
  L <- data.frame(id = rep(seq_len(nrow(AB)), 2), g = factor(rep(grp, 2)), c = rep(c("AE", "AO"), each = nrow(AB)), y = c(z$ae, z$ao))
  L <- L[rep(keep, 2), ]
  t <- as.data.frame(suppressMessages(aov_ez("id", "y", L, between = "g", within = "c", anova_table = list(es = "pes")))$anova_table)
  t }
who <- function() AB$Vigorous >= 75 | AB$Moderate >= 150 | (AB$Moderate + 2 * AB$Vigorous) >= 150
TRUE }, error = function(e) { cat(sprintf("[%-6s %-34s] FAIL  runtime error: %s\n", "prelude", "-", conditionMessage(e))); FALSE })
## ============================================================================================
## ============================================================================================
## CLAIM 1 [CORRIGENDUM MATERIAL]  (finding F1, Section 3)
##   Read as weekly minutes, the deposited activity minutes do not reproduce the stated
##   classification (75 min/week vigorous or 150 min/week moderate) for 8 of 16 participants
##   labelled Active; units and coding are undocumented, so the cause cannot be determined, and
##   the Active versus Sedentary proneness effect (F(1,28) = 5.225, p = .030) is not supported
##   under the stated criterion (F = 0.238, p = .630).
## Check written by the audit (no author code covers this claim):

.V$current <- "C-1"
tryCatch({
expect("C-1", active_n = 16, active_meeting_rule = 8, sedentary_meeting_rule = 0, cells_equal_one = 52, tol = 0, note = "WHO rule read as weekly minutes: vigorous >= 75 or moderate >= 150 (or moderate + 2 x vigorous >= 150)")
expect("C-1", group_F_as_labelled = 5.225, group_p_as_labelled = 0.030, group_F_stated_rule = 0.238, group_p_stated_rule = 0.630, tol = 0.001, note = "group main effect on proneness CRD, labelled groups and groups defined by the stated rule")
w <- who(); a <- AB$Grp == "Active"
t1 <- mixed("VAS"); t2 <- mixed("VAS", grp = ifelse(w, "meets", "not"))
check("C-1", active_n = sum(a), active_meeting_rule = sum(a & w), sedentary_meeting_rule = sum(!a & w),
      cells_equal_one = sum(AB[, c("Light", "Moderate", "Vigorous")] == 1),
      group_F_as_labelled = round(t1["g", "F"], 3), group_p_as_labelled = round(t1["g", "Pr(>F)"], 3),
      group_F_stated_rule = round(t2["g", "F"], 3), group_p_stated_rule = round(t2["g", "Pr(>F)"], 3))
}, error = function(e) runtime_failure("C-1", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 2 [CORRIGENDUM MATERIAL]  (finding F2, Section 4)
##   The Active versus Sedentary effect on proneness (p = .030) is sensitive to the test and to
##   the control-session subtraction: Mann-Whitney p = .059, 4 of 30 leave-one-out runs at or
##   above .05, p = .0925 without the control subtraction, and p = .060 under a Bonferroni
##   correction over the two outcomes; the article states no multiplicity approach.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-2"
tryCatch({
expect("C-2", mann_whitney_p = 0.059, bonferroni_two_outcomes_p = 0.060, loo_runs_at_or_above_05 = 4, tol = 0.001, note = "rank test on the mean of the two CRDs; two-outcome Bonferroni; leave-one-out runs of the mixed ANOVA")
expect("C-2", no_control_subtraction_p = 0.0925, tol = 0.0005, note = "one-way group test on mean raw change (observation and execution) without the control subtraction")
z <- crd("VAS"); m <- (z$ae + z$ao) / 2; x <- z$x
p_full <- mixed("VAS")["g", "Pr(>F)"]
loo <- sapply(seq_len(nrow(AB)), function(i) { k <- rep(TRUE, nrow(AB)); k[i] <- FALSE; mixed("VAS", keep = k)["g", "Pr(>F)"] })
raw <- ((x[, 4] - x[, 3]) + (x[, 6] - x[, 5])) / 2
check("C-2", mann_whitney_p = round(suppressWarnings(wilcox.test(m ~ AB$Grp, exact = FALSE))$p.value, 3),
      bonferroni_two_outcomes_p = round(min(1, 2 * p_full), 3), loo_runs_at_or_above_05 = sum(loo >= 0.05),
      no_control_subtraction_p = round(summary(aov(raw ~ AB$Grp))[[1]][1, "Pr(>F)"], 4))
}, error = function(e) runtime_failure("C-2", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 3 [CORRIGENDUM MATERIAL]  (finding F3, Section 5)
##   The action observation versus action execution contrast is a change-score difference from
##   which the control session cancels algebraically; about 70% of the 10.9-point contrast comes
##   from a lower pre-test in the observation session, post-test proneness is not significantly
##   different between the two conditions (p = .54), and the baseline-adjusted contrast is 8.6
##   points (p = .043).
## Check written by the audit (no author code covers this claim):

.V$current <- "C-3"
tryCatch({
expect("C-3", control_cancels_max_abs = 0, tol = 1e-9, note = "AO CRD minus AE CRD equals the observation change minus the execution change")
expect("C-3", pre_ae = 54.4, pre_ao = 46.7, post_ae = 54.6, post_ao = 57.9, contrast = 10.9, tol = 0.05, note = "VAS means before and after the execution and observation sessions; mean AO minus AE CRD")
expect("C-3", post_diff_p = 0.54, adjusted_contrast = 8.6, adjusted_p = 0.043, tol = 0.005, note = "paired post-test test; post difference regressed on pre difference, intercept")
z <- crd("VAS"); x <- z$x
dd <- (z$ao - z$ae) - ((x[, 6] - x[, 5]) - (x[, 4] - x[, 3]))
dpost <- x[, 6] - x[, 4]; dpre <- x[, 5] - x[, 3]; f <- summary(lm(dpost ~ dpre))$coefficients
check("C-3", control_cancels_max_abs = max(abs(dd)), pre_ae = round(mean(x[, 3]), 1), pre_ao = round(mean(x[, 5]), 1),
      post_ae = round(mean(x[, 4]), 1), post_ao = round(mean(x[, 6]), 1), contrast = round(mean(z$ao - z$ae), 1),
      post_diff_p = round(t.test(dpost)$p.value, 2), adjusted_contrast = round(f[1, 1], 1), adjusted_p = round(f[1, 4], 3))
}, error = function(e) runtime_failure("C-3", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 4 [CORRIGENDUM MATERIAL]  (finding F4, Section 6)
##   The printed power-analysis input, a medium effect size of d = 0.05, cannot produce the
##   printed powers (87.6% at N = 12, 80.6% at N = 26); under G*Power repeated-measures defaults
##   with two groups, two measurements and correlation .5, both printed powers reproduce for
##   Cohen f = 0.50 (d = 1.0, a large effect), while Cohen's medium f = 0.25 gives 34.7% and
##   29.3%.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-4"
tryCatch({
expect("C-4", power_d005_within_n12 = 0.061, power_d005_between_n26 = 0.059, power_f050_within_n12 = 0.876, power_f050_between_n26 = 0.806, power_f025_within_n12 = 0.347, power_f025_between_n26 = 0.293, tol = 0.001, note = "G*Power repeated-measures formulas, two groups, two measurements, correlation .5, alpha .05")
pw <- function(f, n, within) { lam <- if (within) f^2 * n * 2 / 0.5 else f^2 * n * 2 / 1.5
  1 - pf(qf(0.95, 1, n - 2), 1, n - 2, ncp = lam) }
check("C-4", power_d005_within_n12 = round(pw(0.05, 12, TRUE), 3), power_d005_between_n26 = round(pw(0.05, 26, FALSE), 3),
      power_f050_within_n12 = round(pw(0.5, 12, TRUE), 3), power_f050_between_n26 = round(pw(0.5, 26, FALSE), 3),
      power_f025_within_n12 = round(pw(0.25, 12, TRUE), 3), power_f025_between_n26 = round(pw(0.25, 26, FALSE), 3))
}, error = function(e) runtime_failure("C-4", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 5 [COMMENT MATERIAL]  (finding F5, Section 7)
##   The deposited package is one workbook: it holds no heart-rate or %HRmax data,
##   condition-order variable, codebook, units for the activity-minute columns, analysis syntax,
##   README, licence or software record, and 52 of 90 activity-minute cells hold an undocumented
##   value of 1; Table 1 %HRmax and its t-test and any order effects cannot be checked from the
##   package.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-5"
tryCatch({
expect("C-5", files_in_package = 1, sheets = 1, rows = 30, columns = 19, tol = 0, note = "the deposited package")
expect("C-5", heart_rate_columns = 0, order_or_session_columns = 0, code_or_doc_files = 0, cells_equal_one = 52, tol = 0, note = "heart-rate, condition-order or session columns; code, README, codebook or licence files; activity cells holding 1")
f <- list.files(PKG_DIR, recursive = TRUE)
check("C-5", files_in_package = length(f), sheets = length(excel_sheets(file.path(PKG_DIR, "Dataset_Abreu et al.xlsx"))),
      rows = nrow(AB), columns = ncol(AB) - 1,
      heart_rate_columns = sum(grepl("heart|hr|fc|bpm", names(AB), ignore.case = TRUE)),
      order_or_session_columns = sum(grepl("order|ordem|sequence|session|day|dia", names(AB), ignore.case = TRUE)),
      code_or_doc_files = sum(grepl("\\.(sps|spv|jasp|R|py|do|md|txt|pdf|docx)$|readme|codebook|licen", f, ignore.case = TRUE)),
      cells_equal_one = sum(AB[, c("Light", "Moderate", "Vigorous")] == 1))
}, error = function(e) runtime_failure("C-5", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 6 [COMMENT MATERIAL]  (finding F6, Section 8)
##   The deposited workbook holds one row per participant (30 rows) keyed by a two-character
##   study code together with exact age in whole years, sex, activity group and three
##   activity-minute columns; no direct identifiers, dates or free text are present, the article
##   makes no anonymisation claim, and 10 of 30 rows are unique on sex and age and 17 of 30 on
##   sex, age and group.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-6"
tryCatch({
expect("C-6", rows = 30, distinct_codes = 30, unique_sex_age = 10, unique_sex_age_group = 17, min_age = 18, tol = 0, note = "per-participant uniqueness within the file; counts only, no values printed")
k <- function(cols) { key <- do.call(paste, c(AB[cols], sep = "|")); sum(table(key)[key] == 1) }
check("C-6", rows = nrow(AB), distinct_codes = length(unique(AB$ID)), unique_sex_age = k(c("Sex", "Age")),
      unique_sex_age_group = k(c("Sex", "Age", "Grp")), min_age = min(AB$Age))
}, error = function(e) runtime_failure("C-6", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 7 [COMMENT MATERIAL]  (finding F7, Section 9)
##   The workbook's internal package metadata holds one local home-directory path that contains
##   an operating-system account name; no data cell holds a path, and the article makes no
##   anonymisation claim.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-7"
tryCatch({
expect("C-7", metadata_path_occurrences = 1, cells_with_path = 0, tol = 0, note = "absolute home-directory path attribute in the workbook's package metadata; the path itself is not printed")
td <- tempfile(); dir.create(td); unzip(file.path(PKG_DIR, "Dataset_Abreu et al.xlsx"), exdir = td)
xmls <- list.files(td, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
hits <- sum(sapply(xmls, function(p) length(gregexpr("absPath url=\"/", paste(readLines(p, warn = FALSE), collapse = ""), fixed = TRUE)[[1]][gregexpr("absPath url=\"/", paste(readLines(p, warn = FALSE), collapse = ""), fixed = TRUE)[[1]] > 0])))
cells <- sum(sapply(AB, function(col) sum(grepl("^/|[A-Za-z]:\\\\", as.character(col)))))
unlink(td, recursive = TRUE)
check("C-7", metadata_path_occurrences = hits, cells_with_path = cells)
}, error = function(e) runtime_failure("C-7", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 8 [CORRIGENDUM MATERIAL]  (finding F8, Section 9)
##   Table 2 prints -0.001 for the Sedentary, Action Observation cortisol CRD mean; the
##   deposited data, Figure 3 and the printed marginal mean indicate about -0.008.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-8"
tryCatch({
expect("C-8", sedentary_ao_cortisol_crd = -0.008, sedentary_ao_cortisol_sd = 0.061, sedentary_marginal = 0.0003, tol = 0.0005, note = "Table 2 prints -0.001 for this cell; the data value rounds to -0.008")
z <- crd("Cortisol"); s <- AB$Grp == "Sedentary"
check("C-8", sedentary_ao_cortisol_crd = round(mean(z$ao[s]), 3), sedentary_ao_cortisol_sd = round(sd(z$ao[s]), 3),
      sedentary_marginal = round((mean(z$ae[s]) + mean(z$ao[s])) / 2, 4))
}, error = function(e) runtime_failure("C-8", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 9 [CORRIGENDUM MATERIAL]  (finding F9, Section 9)
##   The sex-by-activity chi-square pairs the uncorrected statistic (2.143) with a
##   continuity-corrected or exact p-value (.272); the uncorrected p is .143, and the two groups
##   are not balanced on sex.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-9"
tryCatch({
expect("C-9", pearson_chi2 = 2.143, pearson_p = 0.143, yates_chi2 = 1.205, yates_p = 0.272, tol = 0.001, note = "sex by activity group, N = 30; the article prints 2.143 with p = .272")
expect("C-9", male_active = 10, male_sedentary = 5, tol = 0, note = "cross-table counts")
tb <- table(AB$Sex, AB$Grp); a <- suppressWarnings(chisq.test(tb, correct = FALSE)); b <- suppressWarnings(chisq.test(tb))
check("C-9", pearson_chi2 = round(unname(a$statistic), 3), pearson_p = round(a$p.value, 3), yates_chi2 = round(unname(b$statistic), 3),
      yates_p = round(b$p.value, 3), male_active = tb["Male", "Active"], male_sedentary = tb["Male", "Sedentary"])
}, error = function(e) runtime_failure("C-9", e))
.V$current <- NULL

## ---- summary of every audit-written check above ----
claims_summary(c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7", "C-8", "C-9"))
