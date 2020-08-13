# consider taking this test out if i ever make this public. don't want the test
# hitting their actual server probably

test_that("can get a test page", {
  expect_silent({
    get_matchday_page(55, 6)
  })
})

test_that("errors if page doesn't exist", {
  expect_error({
    get_matchday_page(40, 6)
  })
})
