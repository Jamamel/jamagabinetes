in_dates <- c('18 de enero de 1861',
                '18 de febrero 1861',
                '18 de marzo de 1861',
                '18 de abril de 1861',
                '18 de mayo de 1861',
                '18 de junio de 1861',
                '18 de julio de 1861',
                '18 de agosto de 1861',
                '18 de septiembre de 1861',
                '18 de octubre de 1861',
                '18 de noviembre de 1861',
                '18 de diciembre de 1861')

out_dates <- c("1861-01-18",
               "1861-02-18",
               "1861-03-18",
               "1861-04-18",
               "1861-05-18",
               "1861-06-18",
               "1861-07-18",
               "1861-08-18",
               "1861-09-18",
               "1861-10-18",
               "1861-11-18",
               "1861-12-18") %>%
  lubridate::as_date()

test_that("transformation of different text formats", {

  expect_equal(date_trans(in_dates), out_dates)

})
