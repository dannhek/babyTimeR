testthat::test_that('build test file', {
	x <- c("2024-10-27 07:37 PM
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
====================",
"2024-11-27 05:41 AM ~ 2024-11-27 06:20 AM
Type: Night Sleep
Duration: 39 (min)
====================
2024-11-27 05:38 AM ~ 2024-11-27 05:58 AM
Type: Pumping
Duration: 20 (min)
Pumping Total Amount(ml): 9.5 (fl_oz)
Pumping Left Amount(ml): 4.5 (fl_oz)
Pumping Right Amount(ml): 5 (fl_oz)
====================
2024-11-27 05:35 AM
Type: Pumped Milk
Pumped Milk Total Amount(ml): 5 (fl_oz)
====================
")
	sink('activity_Dann_202410.txt')
	cat(x[1])
	sink()
	testthat::expect_true(file.exists('activity_Dann_202410.txt'))
	zip('activity_Dann_202410.zip', 'activity_Dann_202410.txt')
	testthat::expect_true(file.exists('activity_Dann_202410.zip'))

	sink('activity_Dann_202411.txt')
	cat(x[2])
	sink()
	testthat::expect_true(file.exists('activity_Dann_202411.txt'))
	zip('activity_Dann_202411.zip', 'activity_Dann_202411.txt')
	testthat::expect_true(file.exists('activity_Dann_202411.zip'))
})

testthat::test_that('Read in Directory TXT Files', {
	list_db <- process_one_directory(
		directory = '.',
		remove_txts = FALSE,
		verbose = TRUE
	)
	testthat::expect_equal(class(list_db), 'Clean BT List DB')
	testthat::expect_equal(length(list_db), 4)
	testthat::expect_equal(unique(lubridate::month(list_db$sleep$start_dttm)), c(10,11))
	saveRDS(list_db, file.path('test_list_db2.RDS'))
	testthat::expect_true(file.exists(file.path('test_list_db2.RDS')))
})

testthat::test_that('Read in Directory ZIP Files', {
	list_db <- process_one_directory(
		directory = '.',
		remove_txts = TRUE,
		verbose = TRUE
	)
	testthat::expect_equal(class(list_db), 'Clean BT List DB')
	testthat::expect_equal(length(list_db), 4)
	testthat::expect_equal(unique(lubridate::month(list_db$sleep$start_dttm)), c(10,11))
	saveRDS(list_db, file.path('test_list_db2.RDS'))
	testthat::expect_true(file.exists(file.path('test_list_db2.RDS')))
})



