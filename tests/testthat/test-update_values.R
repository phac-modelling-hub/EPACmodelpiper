test_that("value update returns the correct output", {
  params <- c("days_incubation" = 5)
  values <- update_values(
    params,
    model.name = "hosp",
    state = c(),
    pop = tibble::tibble(
      age = seq(0, 80, by = 5),
      value = rep(100, 17)
    ),
    age_param_names = NULL,
    disease = NULL
  )

  expect_type(values, "list")
  expect_equal(values[["days_incubation"]], params[["days_incubation"]])

  expect_equal(
    update_values(
      params = c("prop_hosp" = 0.1),
      model.name = "hosp",
      state = c(),
      pop = tibble::tibble(
        age = seq(0,80,by = 5),
        value = rep(100,17)
      ),
      age_param_names = c("prop_hosp"),
      disease = "sarscov2"
    )$prop_hosp,
    c(rep(0.09,4),rep(0.1,9),rep(0.11,4)))
})

test_that("value update triggers error if any flows are negative", {
  expect_error(
    update_values(
      params = c("days_incubation" = -0.5),
      model.name = "hosp",
      state = c(),
      pop = tibble::tibble(
        age = seq(0, 80, by = 5),
        value = rep(100, 17)
      ),
      age_param_names = NULL,
      disease = NULL
    )
  )
})
