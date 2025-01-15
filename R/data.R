#' Sample 'BabyTime' Data List Database
#'
#' Sample output from `read_one_bt_activity_file` and `clean_bt_list_db`.
#' A "Clean BT List DB" object.
#'
#' @format ## `bt_list_db`
#' A "Clean BT List DB" object.
#' \describe{
#'	 \item{pumping}{Breast Pumping Data (Data Frame; 3 obs. 8 variables)}
#'	 \item{pee}{Diaper Change Data (Data Frame; 1 obs. 5 variables)}
#'	 \item{pumped_milk}{Drinking Breast Milk from Bottle Data (Data Frame; 3 obs. 6 variables)}
#'	 \item{medicine}{Medications Given Data (Data Frame; 1 obs. 6 variables)}
#'	 \item{baby_food}{Data on Eating Solids (Data Frame; 1 obs. 6 variables)}
#'	 \item{sleep}{Sleep Data. Includes both Night and Daytime Sleep (Data Frame; 5 obs. 5 variables)}
#'	 \item{breastfeeding}{Breast Feeding Data (Data Frame; 4 obs. 7 variables)}
#' }
#' @source Synthetic Data used to test this package. Based loosely on author's
#' personal data.
"bt_list_db"
