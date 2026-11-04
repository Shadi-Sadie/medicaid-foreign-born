# Title: Table C2. Wild cluster restricted bootstrap p-values
# Author: Shadi Seyedi
# Description: Reproduces the six Table 2 contrasts for each of the four outcomes, adds wild
#              cluster restricted (WCR) bootstrap p-values beside the clustered p-values, and
#              writes Table C2 (tex, docx, csv). Algorithm: Roodman, MacKinnon, Nielsen and Webb
#              (2019, fast WCR). Fixed effects nested in the cluster (ST^grp) are partialled out;
#              YEAR^grp enters as dummies so year effects can be re-estimated in each draw.
# Output: output/tables/wild_bootstrap.{tex,docx,csv}

source("code/00_setup.R")
library(fixest)
library(kableExtra)


####################
## Functions
####################

# Fit m2 (same model as m but with YEAR^grp as explicit dummies and only ST^grp absorbed), demean
# X and y by ST^grp with weights, and return the weighted design.
# Returns list(Xw, yw, g, Binv, b, m2, w) where:
#   Xw   = weight-scaled demeaned design matrix (n_obs x k)
#   yw   = weight-scaled demeaned outcome (length n_obs)
#   g    = character vector of state IDs (length n_obs), one per obs in obs(m2)
#   Binv = inverse of Xw'Xw (k x k), after dropping exactly collinear columns
#   b    = least-squares coefficients on the demeaned design (equal to coef(m) by FWL)
#   m2   = the fitted feols object (used only for its design matrix and sample)
wcr_prepare <- function(m, Data, y) {
  obs_idx <- fixest::obs(m)
  Data_obs <- Data[obs_idx, ]

  # year x group factor for the explicit year dummies
  Data_obs$yg <- interaction(Data_obs$YEAR, Data_obs$grp, drop = TRUE, sep = "_")

  # m2: same covariates as m, year-by-group as dummies, only ST^grp absorbed
  cov_str  <- event_covariates(Data_obs, nested = FALSE)
  fml_str  <- paste(y, "~ i(grp, treat) +", cov_str, "+ i(yg) | ST^grp")
  m2 <- fixest::feols(as.formula(fml_str), data = Data_obs, weights = ~PWGTP, vcov = ~ST)

  obs2_idx <- fixest::obs(m2)
  Data2    <- Data_obs[obs2_idx, ]
  w        <- Data2$PWGTP

  # Design matrix (RHS of m2); restrict to columns that survived collinearity removal
  X_full <- model.matrix(m2, type = "rhs")
  X <- X_full[, names(coef(m2)), drop = FALSE]   # order matches coef(m2)

  # Demean X and y by ST^grp with weights
  st_grp_f <- interaction(Data2$ST, Data2$grp, drop = TRUE)
  dm <- fixest::demean(cbind(y = Data2[[y]], X), f = list(st_grp_f), weights = w, as.matrix = TRUE)

  yw <- dm[, "y"] * sqrt(w)
  Xw <- dm[, -1, drop = FALSE] * sqrt(w)
  colnames(Xw) <- colnames(X)

  # fixest keeps one year dummy too many in m2: a group's year dummies sum to that group's
  # intercept, which ST^grp absorbs (exactly collinear). m2's coefficients and residuals are then
  # unreliable (residuals off by up to ~15 on the weighted scale), so drop the collinear columns
  # with a pivoted QR on the column-scaled design and solve the full-rank least squares directly.
  d    <- sqrt(colSums(Xw^2))
  qrX  <- qr(sweep(Xw, 2, d, "/"), tol = 1e-10)
  keep <- sort(qrX$pivot[seq_len(qrX$rank)])
  dropped <- colnames(Xw)[-keep]
  stopifnot(!any(dropped %in% names(coef(m))))
  Xw <- Xw[, keep, drop = FALSE]
  d  <- d[keep]

  # B = Xw'Xw, inverted on the column-scaled scale (well conditioned once collinearity is gone)
  Binv <- solve(crossprod(sweep(Xw, 2, d, "/"))) / outer(d, d)
  b    <- setNames(drop(Binv %*% crossprod(Xw, yw)), colnames(Xw))

  g <- as.character(Data2$ST)

  list(Xw = Xw, yw = yw, g = g, Binv = Binv, b = b, m2 = m2, w = w,
       Data2 = Data2, fml_str = fml_str, u_r = NULL)
}

# WCR bootstrap for one contrast vector a (named, same length as prep$b).
# V: G x B integer matrix of Rademacher draws (+/-1); if NULL, skip bootstrap (return se0 only).
# Returns list(estimate, se0, t, t_star, p_wcr); t_star NULL when V is NULL.
wcr_test <- function(prep, a, V) {
  Xw   <- prep$Xw
  yw   <- prep$yw
  Binv <- prep$Binv   # k x k inverse of Xw'Xw
  b    <- prep$b
  g    <- prep$g

  # Align a to column order of Xw
  a_vec <- a[colnames(Xw)]
  stopifnot(!any(is.na(a_vec)))

  q <- drop(Binv %*% a_vec)   # q = B^{-1} a
  u    <- yw - drop(Xw %*% b) # unrestricted weighted residuals (length n)

  # State-level scores for the unrestricted problem
  g_factor  <- factor(g)
  g_levels  <- levels(g_factor)
  G         <- nlevels(g_factor)
  g_int     <- as.integer(g_factor)

  # CRVE se (no small-sample correction — ssc is applied externally if needed)
  Sg  <- t(rowsum(Xw * u, g_int))   # k x G state scores
  qSg <- drop(q %*% Sg)             # G-vector: q'S_g
  se0 <- sqrt(sum(qSg^2))

  estimate <- sum(a_vec * b)
  t_val    <- estimate / se0

  # Restricted estimate: b_r = b - q * (a'b) / (a'q)
  aq  <- drop(a_vec %*% q)    # scalar a'q
  b_r <- b - q * (estimate / aq)
  u_r <- yw - drop(Xw %*% b_r)

  if (is.null(V)) {
    return(list(estimate = estimate, se0 = se0, t = t_val, t_star = NULL, p_wcr = NA,
                u_r = u_r))
  }

  # State-level scores for the restricted problem (k x G)
  s_g <- t(rowsum(Xw * u_r, g_int))

  c_g <- drop(q %*% s_g)   # G-vector: q's_g

  # P: G x k matrix, row g = q'H_g where H_g = sum_{i in g} Xw_i Xw_i'
  # q'H_g = sum_{i in g} (q'Xw_i) * Xw_i  =>  P = rowsum((Xw q) * Xw, g)
  qXw <- drop(Xw %*% q)           # length n: q'Xw_i
  P   <- rowsum(qXw * Xw, g_int)  # G x k

  # D = P B^{-1} s_g  (G x G)
  BinvSg <- Binv %*% s_g   # k x G
  D <- P %*% BinvSg          # G x G

  # Bootstrap draws: V is G x Nboot (Rademacher +/-1)
  Nboot <- ncol(V)
  num         <- drop(c_g %*% V)           # length Nboot: a'b* under the null
  scores_star <- c_g * V - D %*% V          # G x Nboot
  se_star     <- sqrt(colSums(scores_star^2))
  t_star      <- num / se_star

  p_wcr <- mean(abs(t_star) >= abs(t_val))

  list(estimate = estimate, se0 = se0, t = t_val, t_star = t_star, p_wcr = p_wcr,
       u_r = u_r)
}


####################
## Guard: source for functions only when WCB_FUNCTIONS_ONLY is set
####################

if (exists("WCB_FUNCTIONS_ONLY") && isTRUE(WCB_FUNCTIONS_ONLY)) {
  invisible(NULL)
} else {

####################
## Main: fit, bootstrap, Table C2
####################

name  <- "wild_bootstrap"
title <- "Wild cluster restricted bootstrap p-values for effects of Medicaid expansion on coverage, by citizenship"

Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))
set.seed(20261103)

n_states     <- length(unique(Data$ST))
n_exp_states <- length(unique(Data$ST[Data$expansion == 1]))

results <- vector("list", length(outcome_vars))
names(results) <- outcome_vars

for (y in outcome_vars) {
  cat("Outcome:", y, "\n")
  t0 <- proc.time()["elapsed"]
  m    <- fit_static_models(Data, y, nested = FALSE)
  prep <- wcr_prepare(m, Data, y)

  G    <- nlevels(factor(prep$g))
  Nboot <- 9999L
  V    <- matrix(sample(c(-1L, 1L), G * Nboot, replace = TRUE), G, Nboot)

  rows <- vector("list", length(effect_contrasts))
  names(rows) <- names(effect_contrasts)
  for (cn in names(effect_contrasts)) {
    k   <- effect_contrasts[[cn]]
    a   <- setNames(numeric(length(prep$b)), names(prep$b))
    treat_terms <- sprintf("grp::%s:treat", k$series)
    for (j in seq_along(treat_terms)) a[treat_terms[j]] <- k$w[j]
    e   <- static_estimate(m, k$series, k$w)
    res <- wcr_test(prep, a, V)
    p_cluster <- 2 * pt(-abs(e$estimate / e$se), df = G - 1)
    rows[[cn]] <- data.frame(
      outcome   = y,
      contrast  = cn,
      estimate  = e$estimate,
      se        = e$se,
      p_cluster = p_cluster,
      p_wcr     = res$p_wcr,
      B         = Nboot,
      stringsAsFactors = FALSE
    )
  }
  results[[y]] <- do.call(rbind, rows)
  t1 <- proc.time()["elapsed"]
  cat("  done in", round(t1 - t0, 1), "s\n")
  rm(m, prep, V)
  gc()
}

res_all <- do.call(rbind, results)
rownames(res_all) <- NULL

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(res_all, file.path(table_dir, paste0(name, ".csv")), row.names = FALSE)
stopifnot(nrow(res_all) == length(outcome_vars) * length(effect_contrasts))


####################
## Table C2
####################

fmt_est <- function(x) sub("^-", "−", sprintf("%.1f", x))  # true minus sign
fmt_p   <- function(p) ifelse(p < 0.001, "< 0.001", paste("=", sprintf("%.3f", p)))

row_labels <- c(HICOV = "Any insurance", HINS4 = "Medicaid",
                HINS1 = "Employer-sponsored insurance", HINS2 = "Direct purchase")

tab <- do.call(rbind, lapply(outcome_vars, function(y) {
  e <- res_all[res_all$outcome == y, ]
  e <- e[match(names(effect_contrasts), e$contrast), ]
  cells <- sprintf("%s\np %s\nWCR p %s",
                   fmt_est(e$estimate),
                   fmt_p(e$p_cluster),
                   fmt_p(e$p_wcr))
  data.frame(Outcome = row_labels[[y]],
             t(setNames(cells, names(effect_contrasts))),
             check.names = FALSE, stringsAsFactors = FALSE)
}))

note <- sprintf(paste(
  "Note: American Community Survey (ACS), 2011 to 2019.",
  "Estimates of Table 2 (all covariates), in percentage points.",
  "p: two-sided, from standard errors clustered by state,",
  "t distribution with G − 1 = %d degrees of freedom.",
  "WCR p: wild cluster restricted bootstrap (Rademacher weights by state,",
  "%s replications, null imposed; Roodman, MacKinnon, Nielsen and Webb 2019).",
  "%d states, %d of which expanded."),
  n_states - 1L,
  format(9999L, big.mark = ","),
  n_states,
  n_exp_states)
abbreviations <- "Abbreviations: ACS, American Community Survey; US, United States."

col_heads <- c("Outcome", "US-born", "Naturalized\ncitizen", "Non-citizen",
               "Naturalized\ncitizen minus\nUS-born", "Non-citizen\nminus\nUS-born",
               "Non-citizen\nminus\nnaturalized\ncitizen")
spanners <- c("Effect of expansion" = 3, "Difference between groups" = 3)

## LaTeX
tex_body <- tab
tex_body[-1] <- lapply(tex_body[-1], function(x) {
  x <- gsub("−", "$-$", x)
  x <- gsub("<", "$<$", x)
  sprintf("\\makecell[tr]{%s}", gsub("\n", "\\\\\\\\", x))
})
tex <- kableExtra::kbl(tex_body,
  col.names = sprintf("\\makecell[b%s]{%s}", c("l", rep("r", 6)),
                      gsub("\n", "\\\\\\\\", col_heads)),
  format = "latex", booktabs = TRUE, escape = FALSE,
  align = c("l", rep("r", 6)), caption = title, linesep = "") |>
  kableExtra::add_header_above(c(" " = 1, spanners)) |>
  kableExtra::footnote(general = c(gsub("−", "$-$", note), abbreviations),
                       general_title = "", threeparttable = TRUE, escape = FALSE)
tex <- sub("\\begin{table}",
           paste0("\\begin{landscape}\\begin{table}\\renewcommand{\\thetable}{",
                  exhibit_number(name), "}"),
           tex, fixed = TRUE)
tex <- sub("\\end{table}",
           "\\end{table}\\end{landscape}",
           tex, fixed = TRUE)
tex <- sub("\\centering",
           "\\centering\\renewcommand{\\baselinestretch}{1}\\footnotesize\\setlength{\\tabcolsep}{3pt}",
           tex, fixed = TRUE)
writeLines(tex, file.path(table_dir, paste0(name, ".tex")))

## Word
docx_body <- setNames(tab, c("label", paste0("c", 1:6)))
ft <- flextable::flextable(docx_body) |>
  flextable::set_header_labels(
    values = setNames(as.list(gsub("\n", " ", col_heads)), names(docx_body))) |>
  flextable::add_header_row(values = c("", names(spanners)), colwidths = c(1, 3, 3)) |>
  flextable::add_footer_lines(c(note, abbreviations)) |>
  docx_style(spanner_cols = list(2:4, 5:7)) |>
  flextable::align(j = 2:7, align = "right", part = "all") |>
  flextable::align(i = 1, j = 2:7, align = "center", part = "header") |>
  flextable::width(j = 1, width = 1.4) |>
  flextable::width(j = 2:7, width = 0.95)
save_docx_table(ft, exhibit_number(name), title,
                file.path(table_dir, paste0(name, ".docx")))

cat("Table C2 written to", table_dir, "\n")
cat("Expansion states in main sample:", n_exp_states, "\n")
cat("Significance changes under WCR:\n")
sig_change <- res_all[
  (res_all$p_cluster < 0.05) != (res_all$p_wcr < 0.05), ]
if (nrow(sig_change) == 0) {
  cat("  None — all contrasts retain the same significance at 5% under WCR\n")
} else {
  for (i in seq_len(nrow(sig_change))) {
    r <- sig_change[i, ]
    cat(sprintf("  %s / %s: p_cluster=%.3f, p_wcr=%.3f\n",
                r$outcome, r$contrast, r$p_cluster, r$p_wcr))
  }
}

}  # end guard
