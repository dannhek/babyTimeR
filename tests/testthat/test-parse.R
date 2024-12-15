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
2024-10-03
Here's some garbage data
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
2024-10-22 05:22 PM
Type: Baby Food
Chicken, Vegetables, Yogurt, Fruit: None (None)
====================
2024-10-29 04:30 PM ~ 2024-10-29 04:39 PM
Type: Breastfeeding (right)
Duration: 8 (min)
Breastfeeding Right: 8 (min)
====================
2024-10-29 04:13 PM ~ 2024-10-29 04:25 PM
Type: Breastfeeding
Duration: 11 (min)
====================
2024-10-27 08:02 PM
Type: Breastfeeding (left)
Duration: 8 (min)
Breastfeeding Left: 8 (min)
====================
2024-10-18 03:16 PM ~ 2024-10-18 03:26 PM
Type: Breastfeeding (both)
Duration: 10 (min)
Breastfeeding Left: 5 (min)
Breastfeeding Right: 5 (min)
====================
"
	sink('activity_Dann_202410.txt')
	cat(x)
	sink()
	testthat::expect_true(file.exists('activity_Dann_202410.txt'))
	zip('activity_Dann_202410.zip', 'activity_Dann_202410.txt')
	testthat::expect_true(file.exists('activity_Dann_202410.zip'))
})

testthat::test_that('Test Fake Names', {
	testthat::expect_error({
		file.create('activity_Dann_20240.txt')
		list_db <- read_one_bt_activity_file(
			infile = file.path('activity_Dann_20240.txt')
		)
	})
	file.remove('activity_Dann_20240.txt')
	testthat::expect_error({
		list_db <- read_one_bt_activity_file(
			infile = file.path('activity_Dann_202403.txt')
		)
	})
})
testthat::test_that('Read in Test txt File', {
	testthat::expect_warning({
		list_db <- read_one_bt_activity_file(
			infile = file.path('activity_Dann_202410.txt')
		)
	})
	testthat::expect_equal(class(list_db), 'Raw BT List DB')
	testthat::expect_equal(length(list_db), 11)
	list_db <- list_db |>
		clean_bt_list_db()
	testthat::expect_equal(class(list_db), 'Clean BT List DB')
	testthat::expect_equal(length(list_db), 7)
	saveRDS(list_db, file.path('test_list_db.RDS'))
	testthat::expect_true(file.exists(file.path('test_list_db.RDS')))
})

testthat::test_that('Read in Test zip File', {
	testthat::expect_warning({
		list_db <- read_one_bt_activity_file(
			infile = file.path('activity_Dann_202410.zip'),
			verbose = TRUE
		)
	})
	testthat::expect_equal(class(list_db), 'Raw BT List DB')
	testthat::expect_equal(length(list_db), 11)
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


testthat::test_that('Accurate Data (Baby Food)', {
	list_db <- readRDS('test_list_db.RDS')
	had_veggies <- max(dplyr::case_when(
		stringr::str_detect(list_db$baby_food$food_type, 'Vegetables') ~ 1,
		.default = 0
	), na.rm = TRUE)
	testthat::expect_equal(had_veggies, 1)
})


testthat::test_that('Accurate Data (Breastfeeding)', {
	list_db <- readRDS('test_list_db.RDS')
	total_duration <- sum(list_db$breastfeeding$duration_min)
	left_duration <- sum(readr::parse_number(list_db$breastfeeding$breastfeeding_left), na.rm = TRUE)
	right_duration <- sum(readr::parse_number(list_db$breastfeeding$breastfeeding_right), na.rm = TRUE)

	testthat::expect_equal(total_duration, 37)
	testthat::expect_equal(left_duration, 13)
	testthat::expect_equal(right_duration, 13)
})


testthat::test_that('Combine Lists', {
	list_db1 <- readRDS('test_list_db.RDS')
	list_db2 <- readRDS('test_list_db.RDS')
	list_db_comb <- combine_clean_bt_list_dbs(
		list_db1,
		list_db2
	)
	testthat::expect_equal(
		nrow(list_db_comb$sleep),
		nrow(list_db1$sleep) + nrow(list_db2$sleep)
	)
	testthat::expect_error({
		combine_clean_bt_list_dbs(
			'i am a string, not a cleaned babytime list',
			list_db2
		)
	})
	testthat::expect_error({
		combine_clean_bt_list_dbs(
			list_db1,
			'i am a string, not a cleaned babytime list'
		)
	})
})


#Remove Test Files
	file.remove('activity_Dann_202410.txt')
	file.remove('activity_Dann_202410.zip')
	file.remove('test_list_db.RDS')
