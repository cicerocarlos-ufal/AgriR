test_that("triple factorial ANOVA and unfolding", {
  f3 <- ext("fat3.csv")
  res <- quiet(anova_fat3_rbd(f3$cultivar, f3$adubo, f3$espac, f3$bloco,
                              f3$resp, graph = FALSE))
  a <- res$Anova
  expect_equal(rownames(a), c("Block", "F1", "F2", "F3", "F1*F2", "F1*F3",
                              "F2*F3", "F1*F2*F3", "Residuals", "Total"))
  expect_equal(a["Residuals", "DF"], 22)
  expect_equal(a["Total", "DF"], nrow(f3) - 1)
  ref <- summary(aov(resp ~ factor(bloco) + cultivar * adubo * espac,
                     data = f3))[[1]]
  expect_equal(sum(a$SS[1:8]), sum(ref[1:8, "Sum Sq"]))
  # F1*F2 and F1*F3 are significant: four unfoldings, no main effects
  expect_length(res$Unfolding, 4)
  expect_length(res$Mcomp, 3 + 2 + 2 + 2)
  # SS of F1 inside F2 = SS(F1) + SS(F1*F2)
  u <- res$Unfolding[["F1 inside of F2"]]
  expect_equal(sum(u$SS[1:3]), a["F1", "SS"] + a["F1*F2", "SS"],
               tolerance = 1e-4)

  res2 <- quiet(anova_fat3_crd(f3$cultivar, f3$adubo, f3$espac, f3$resp,
                               graph = FALSE, mcomp = "sk"))
  expect_equal(res2$Anova["Residuals", "DF"], 24)
})

test_that("split-split-plot ANOVA uses errors a, b and c", {
  ssp <- ext("ssp.csv")
  res <- quiet(anova_ssp_rbd(ssp$irrigacao, ssp$cultivar, ssp$epoca,
                             ssp$bloco, ssp$resp, graph = FALSE))
  a <- res$Anova
  expect_equal(a["Error a", "DF"], 3)
  expect_equal(a["Error b", "DF"], 12)
  expect_equal(a["Error c", "DF"], 18)
  expect_equal(a["F1", "Fc"], a["F1", "MS"] / a["Error a", "MS"])
  expect_equal(a["F2", "Fc"], a["F2", "MS"] / a["Error b", "MS"])
  expect_equal(a["F1*F2", "Fc"], a["F1*F2", "MS"] / a["Error b", "MS"])
  expect_equal(a["F3", "Fc"], a["F3", "MS"] / a["Error c", "MS"])
  expect_length(res$CV, 3)
  # F2*F3 is significant: F2 inside F3 (pooled error) and F3 inside F2
  expect_true(all(c("F2 inside of F3", "F3 inside of F2") %in%
                    names(res$Unfolding)))
  u <- res$Unfolding[["F2 inside of F3"]]
  msb <- a["Error b", "MS"]
  msc <- a["Error c", "MS"]
  expect_equal(u["Pooled error", "MS"], round((msb + msc) / 2, 5))
  satt <- ((msb + msc) / 2)^2 / ((msb / 2)^2 / 12 + (msc / 2)^2 / 18)
  expect_equal(u["Pooled error", "DF"], round(satt, 2))
  expect_equal(rownames(res$Unfolding[["F3 inside of F2"]])[4], "Error c")
  # factor 1 (not in significant interactions): main effect with error a
  expect_true("F1" %in% names(res$Mcomp))
  expect_length(res$Mcomp, 2 + 3 + 1)

  res2 <- quiet(anova_ssp_crd(ssp$irrigacao, ssp$cultivar, ssp$epoca,
                              ssp$bloco, ssp$resp, graph = FALSE))
  expect_equal(res2$Anova["Error a", "DF"], 6)
  expect_equal(res2$Anova["Error c", "DF"], 18)
})

test_that("three-factor designs contain every treatment", {
  set.seed(3)
  d <- quiet(des.fat3.crd(2, 3, 2, 2))
  expect_equal(nrow(d$Book), 24)
  expect_equal(as.vector(table(d$Book$factor1, d$Book$factor2,
                               d$Book$factor3)), rep(2, 12))
  d <- quiet(des.fat3.rbd(2, 2, 2, 3))
  expect_equal(as.vector(table(d$Book$Block, d$Book$factor1, d$Book$factor2,
                               d$Book$factor3)), rep(1, 24))
  d <- quiet(des.ssp.crd(2, 3, 2, 2))
  expect_equal(nrow(d$Book), 2 * 3 * 2 * 2)
  # each plot has every combination of factors 2 and 3 once
  expect_true(all(tapply(paste(d$Book$factor2, d$Book$factor3), d$Book$plot,
                         function(v) length(unique(v)) == 6)))
  d <- quiet(des.ssp.rbd(3, 2, 2, 2))
  expect_equal(as.vector(table(d$Book$Block, d$Book$factor1)), rep(4, 6))
  expect_true(all(tapply(d$Book$factor1, d$Book$plot,
                         function(v) length(unique(v)) == 1)))
})
