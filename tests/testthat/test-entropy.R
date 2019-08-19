test_that("Entropy results are equal", {
  expect_equal(joint_entropy(cars), joint_entropy2(cars))
})
