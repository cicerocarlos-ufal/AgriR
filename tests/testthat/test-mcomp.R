y <- c(10, 11, 10.5, 10.2, 20, 21, 20.5, 20.2, 30, 31, 30.5, 30.2)
trt <- rep(c("A", "B", "C"), each = 4)
fit <- aov(y ~ factor(trt))
DFe <- fit$df.residual
SSe <- sum(residuals(fit)^2)

test_that("well separated means get different letters in every test", {
  for (f in list(tukey, duncan, lsd, snk, scottknott, scheffe)) {
    tab <- quiet(f(y, trt, DFe, SSe, graph = FALSE))
    expect_s3_class(tab, "data.frame")
    expect_equal(rownames(tab), c("A", "B", "C"))
    expect_equal(tab$Medias, as.numeric(tapply(y, trt, mean)))
    expect_equal(length(unique(tab[[5]])), 3)
    # the highest mean receives the letter "a"
    expect_equal(tab[["C", 5]], "a")
  }
})

test_that("equal means share the same letter", {
  y2 <- rep(c(10, 10.1, 10.05), each = 4) + rep(c(-2, 2, -1, 1), 3)
  fit2 <- aov(y2 ~ factor(trt))
  tab <- quiet(tukey(y2, trt, fit2$df.residual, sum(residuals(fit2)^2),
                     graph = FALSE))
  expect_true(all(grepl("a", tab$tukey)))
  sk <- quiet(scottknott(y2, trt, fit2$df.residual, sum(residuals(fit2)^2),
                         graph = FALSE))
  expect_equal(unique(sk$scott), "a")
})

test_that("letters follow the levels when there are 10 or more treatments", {
  trt10 <- rep(1:10, each = 3)
  y10 <- rep(1:10 * 10, each = 3) + rep(c(-0.1, 0, 0.1), 10)
  fit10 <- aov(y10 ~ factor(trt10))
  tab <- quiet(tukey(y10, trt10, fit10$df.residual, sum(residuals(fit10)^2),
                     graph = FALSE))
  expect_equal(rownames(tab), as.character(1:10))
  expect_equal(tab[["10", "tukey"]], "a")
  expect_equal(tab[["1", "tukey"]], "j")
})

test_that("graphs can be drawn", {
  for (g in c("barplotb", "barplotg", "pointplotg", "boxplotb")) {
    expect_no_error(quiet(tukey(y, trt, DFe, SSe, graph.type = g)))
  }
})

test_that("invalid test names give an error", {
  expect_error(quiet(anova_crd(trt, y, mcomp = "ccboot")), "mcomp")
})
