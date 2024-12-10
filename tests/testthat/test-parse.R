testthat::test_that('build test file', {
x <- "2024-10-27 07:37 PM
Type: Pumping
====================
2024-10-27 06:34 PM
Type: Day Sleep
====================
2024-10-27 12:08 PM
Type: Pee
Diaper type: Poop
====================
2024-10-27 08:26 AM ~ 2024-10-27 10:01 AM
Type: Day Sleep
Duration: 95 (min)
====================
2024-10-27 08:25 AM
Type: Pumped Milk
Pumped Milk Total Amount(ml): 5 (fl_oz)
====================
2024-10-27 05:41 AM ~ 2024-10-27 06:20 AM
Type: Night Sleep
Duration: 39 (min)
====================
2024-10-27 05:38 AM ~ 2024-10-27 05:58 AM
Type: Pumping
Duration: 20 (min)
Pumping Total Amount(ml): 9.5 (fl_oz)
Pumping Left Amount(ml): 4.5 (fl_oz)
Pumping Right Amount(ml): 5 (fl_oz)
====================
2024-10-27 05:35 AM
Type: Pumped Milk
Pumped Milk Total Amount(ml): 5 (fl_oz)
====================
2024-10-26 11:58 PM ~ 2024-10-27 02:19 AM
Type: Night Sleep
Duration: 140 (min)
====================
2024-10-26 11:13 PM
Type: Medicine
Medicine type: Analgesic
Memo: Tylenol
====================
2024-10-26 10:06 PM ~ 2024-10-26 10:26 PM
Type: Pumping
Duration: 20 (min)
Pumping Total Amount(ml): 8.25 (fl_oz)
Pumping Left Amount(ml): 3.25 (fl_oz)
Pumping Right Amount(ml): 5 (fl_oz)
====================
2024-10-26 06:58 PM ~ 2024-10-26 10:59 PM
Type: Night Sleep
Duration: 241 (min)
====================
2024-10-26 06:51 PM ~ 2024-10-26 07:00 PM
Type: Pumped Milk
Pumped Milk Total Amount(ml): 5 (fl_oz)
Memo: Spilled a little
mixed with cow's milk
====================
"
	sink('activity_Dann_202410.txt')
	cat(x)
	sink()
	testthat::expect_true(file.exists('activity_Dann_202410.txt'))
	zip('activity_Dann_202410.zip', 'activity_Dann_202410.txt')
	testthat::expect_true(file.exists('activity_Dann_202410.zip'))
})

testthat::test_that('Read in Test txt File', {
	list_db <- read_one_bt_activity_file(
		infile = file.path('activity_Dann_202410.txt')
	)
	testthat::expect_equal(class(list_db), 'Raw BT List DB')
	testthat::expect_equal(length(list_db), 6)
	list_db <- list_db |>
		clean_bt_list_db()
	testthat::expect_equal(class(list_db), 'Clean BT List DB')
	testthat::expect_equal(length(list_db), 5)
	saveRDS(list_db, file.path('test_list_db.RDS'))
	testthat::expect_true(file.exists(file.path('test_list_db.RDS')))
})

testthat::test_that('Read in Test zip File', {
	list_db <- read_one_bt_activity_file(
		infile = file.path('activity_Dann_202410.zip')
	)
	testthat::expect_equal(class(list_db), 'Raw BT List DB')
	testthat::expect_equal(length(list_db), 6)
})

testthat::test_that('Multi-line Memo', {
	list_db <- readRDS('test_list_db.RDS')
	testthat::expect_true(stringr::str_detect(list_db$pumped_milk$memo[3], '\t'))
})

testthat::test_that('Accurate Data (Pumping)', {
	list_db <- readRDS('test_list_db.RDS')
	total_pumped <- list_db$pumping$pumping_total_amount_ml |>
		sum(na.rm = TRUE)
	testthat::expect_equal(total_pumped, 17.75)
})

testthat::test_that('Accurate Data (Sleep)', {
	list_db <- readRDS('test_list_db.RDS')
	total_sleep <- list_db$sleep$duration_min |>
		sum(na.rm = TRUE)
	testthat::expect_equal(total_sleep, 515)
})

testthat::test_that('Accurate Data (Last Analgesia)', {
	list_db <- readRDS('test_list_db.RDS')
	last_med <- list_db$medicine |>
		dplyr::filter(medicine_type == 'Analgesic') |>
		dplyr::slice_head(n = 1)
	testthat::expect_equal(last_med$memo, 'Tylenol')
	testthat::expect_equal(
		lubridate::ymd_hms(last_med$start_dttm  , tz = 'America/Detroit'),
		lubridate::ymd_hms('2024-10-26 23:13:00', tz = 'America/Detroit')
	)
})
