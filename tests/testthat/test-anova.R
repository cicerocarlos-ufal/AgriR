test_that("CRD and RBD ANOVA match aov()", {
  crd <- ext("crd.csv")
  res <- quiet(anova_crd(crd$trat, crd$resp, graph = FALSE))
  ref <- summary(aov(resp ~ factor(trat), data = crd))[[1]]
  expect_equal(res$Anova[1, "SS"], ref[1, "Sum Sq"])
  expect_equal(res$Anova["Total", "DF"], nrow(crd) - 1)
  expect_true(is.na(res$Anova["Total", "Fc"]))
  expect_s3_class(res$Mcomp, "data.frame")

  rbd <- ext("rbd.csv")
  res <- quiet(anova_rbd(rbd$trat, rbd$resp, rbd$bloco, mcomp = "sk",
                         graph = FALSE))
  expect_equal(rownames(res$Anova),
               c("Treatments", "Blocks", "Residuals", "Total"))
  expect_equal(res$Anova["Residuals", "DF"], 9)
})

test_that("quantitative treatments give a regression", {
  dose <- ext("dose.csv")
  res <- quiet(anova_crd(dose$dose, dose$resp, quali = FALSE, graph = FALSE))
  expect_true(all(c("Anova.Linear", "Anova.Quad", "Anova.Cub") %in%
                    names(res$Reg)))
  expect_equal(res$Reg$Best.model, "quadratic")
})

test_that("reg.poly fits a quadratic model with 3 levels", {
  x <- rep(c(0, 1, 2), each = 3)
  y <- 1 + 2 * x - x^2 + rep(c(-0.1, 0, 0.1), 3)
  fit <- summary(aov(y ~ factor(x)))[[1]]
  res <- quiet(reg.poly(y, x, fit[2, 1], fit[2, 2], fit[1, 1], fit[1, 2],
                        graph = FALSE))
  expect_false(is.null(res$Anova.Quad))
  expect_equal(unname(res$Coef.Quad), c(1, 2, -1), tolerance = 1e-8)
  expect_equal(res$R2.Quad, 1)
})

test_that("Latin square works", {
  lq <- ext("latsq.csv")
  res <- quiet(latsd(lq$trat, lq$linha, lq$coluna, lq$resp, graph = FALSE))
  expect_equal(res$Anova["Residuals", "DF"], 6)
})

test_that("factorial with significant interaction is unfolded", {
  fat <- ext("fat2.csv")
  res <- quiet(anova_fat2_crd(fat$cultivar, fat$adubo, fat$resp,
                              graph = FALSE))
  expect_false(is.null(res$Unfolding))
  expect_length(res$Mcomp, 2)
  # SS of the unfolding = SS of factor 2 + SS of factor 1 + interaction
  u <- res$Unfolding
  expect_equal(sum(u$SS[1:3]), sum(res$Anova$SS[1:3]), tolerance = 1e-4)

  res2 <- quiet(anova_fat2_rbd(fat$cultivar, fat$adubo, fat$bloco, fat$resp,
                               fac.fix = "fac.B", graph = FALSE,
                               mcomp = "scheffe"))
  expect_length(res2$Mcomp, 3)
  expect_equal(rownames(res2$Unfolding)[1], "Block")
})

test_that("split-plot uses error a for factor 1", {
  sp <- ext("split.csv")
  res <- quiet(anova_sp2_rbd(sp$irrigacao, sp$cultivar, sp$bloco, sp$resp,
                             graph = FALSE))
  a <- res$Anova
  expect_equal(a["Error a", "DF"], 6)
  expect_equal(a["Error b", "DF"], 27)
  expect_equal(a["fac.A", "Fc"], a["fac.A", "MS"] / a["Error a", "MS"])
  res2 <- quiet(anova_sp2_crd(sp$irrigacao, sp$cultivar, sp$bloco, sp$resp,
                              graph = FALSE, quali = c(TRUE, TRUE)))
  expect_equal(res2$Anova["Error a", "DF"], 9)
})

test_that("descriptive functions run", {
  y <- c(3, 4, 4, 4, 5, 5, 6, 8, 8, 9, 9, 10, 11, 12)
  d <- quiet(desc(y))
  expect_equal(d$Result["Mean", 1], round(mean(y), 3))
  ft <- quiet(freqtable(y, 4))
  expect_equal(sum(ft$Frequency), length(y))
  for (s in c("empiric", "sturges", "ricerule", "scott", "fd")) {
    h <- quiet(histg(y, scores = s))
    expect_equal(sum(h$Dist.Freq$Frequency), length(y))
  }
})
