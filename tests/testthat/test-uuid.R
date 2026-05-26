test_that(".uuid() returns a 36-character RFC 4122-style string", {
  id <- jupytergis:::.uuid()
  expect_type(id, "character")
  expect_length(id, 1L)
  expect_match(
    id,
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  )
})
