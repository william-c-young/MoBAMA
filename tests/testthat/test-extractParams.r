context("MoBAMA Extract Parameters")

obj <- list(parameters = data.frame(var = c("a",
                                            "ab",
                                            "abc",
                                            "abcd",
                                            "abcde",
                                            "abcde",
                                            "bcde",
                                            "cde",
                                            "de",
                                            "e",
                                            "ace",
                                            "bd"),
                                    stringsAsFactors = FALSE))

test_that("The extractParams function selects the correct rows", {
    expect_equal(extractParams(obj, "a")$var,
                 c("a", "ab", "abc", "abcd", "abcde", "abcde", "ace"))
    expect_equal(extractParams(obj, "ab")$var,
                 c("ab", "abc", "abcd", "abcde", "abcde"))
    expect_equal(extractParams(obj, "f")$var,
                 character(0))
    expect_equal(extractParams(obj, "ab", TRUE)$var,
                 "ab")
    expect_equal(extractParams(obj, "d", TRUE)$var,
                 character(0))
})

test_that("The extractParams2 function selects the correct rows", {
    expect_equal(extractParams2(obj, "a", "e")$var,
                 c("abcde", "abcde", "ace"))
    expect_equal(extractParams2(obj, "ab", "d")$var,
                 "abcd")
    expect_equal(extractParams2(obj, "ab", "bc")$var,
                 "abc")
    expect_equal(extractParams2(obj, "ab", "ce")$var,
                 character(0))
})
