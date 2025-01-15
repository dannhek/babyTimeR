#' Process One Directory of 'BabyTime' Activity Files
#'
#' Only works with with .zip files
#'
#' @param directory directory with activity files in it
#' @param remove_txts boolean. whether to remove txt files before processing
#' @param verbose boolean. print out processing details?
#'
#' @return Clean BT List DB object (and also write it to the `out` RDS file)
#' @export
#' @importFrom stringr str_replace
#'
#' @examples
#' data_dir <- system.file('extdata', package = 'babyTimeR')
#' if (file.exists('parsed_data.RDS')) {
#'     baby_dann_db <- readRDS('parsed_data.RDS')
#' } else {
#'     baby_dann_db <- process_one_directory(data_dir)
#' }
process_one_directory <- function(
		directory,
		remove_txts = TRUE,
		verbose = TRUE
) {
	list_db <- list()
	txt_files <- list.files(directory, pattern = 'activity_[A-Za-z]*_[0-9]{6}\\.txt', full.names = T)
	zip_files <- list.files(directory, pattern = 'activity_[A-Za-z]*_[0-9]{6}\\.zip', full.names = T)
	if (remove_txts) {
		if (verbose) {
			message(paste0(
				"Removing extracted TXT files:\n\t",
				paste(txt_files, collapse = '\n\t')
			))
		}
		file.remove(txt_files)
	}
	for (f in unique(stringr::str_replace(c(txt_files, zip_files), '(\\.txt|\\.zip)', ''))) {
		if (verbose) {
			message(glue::glue("Processing {f}..."))
		}
		if (file.exists(paste0(f, '.txt'))) {
			list_db <- read_one_bt_activity_file(
				infile = paste0(f, '.txt'),
				list_db = list_db
			)
		} else if (file.exists(paste0(f, '.zip'))) {
			list_db <- read_one_bt_activity_file(
				infile = paste0(f, '.zip'),
				list_db = list_db
			)
		}
	}
	list_db <- clean_bt_list_db(list_db)
	list_db
}
