#' Read One BabyTime Activity File
#'
#' Read line-by-line and parse into a "database" in a list.
#'
#' @param infile a .zip or .txt file with the format activity_BabyName_yyyymm
#'	as the BabyTime app exports data.
#' @param list_db a list object on which to append data. If none exists, this
#'	will create a new list object.
#' @param verbose whether or not to write out progress messages.
#'
#' @return a populated `list_db` list object
#' @export
#'
#' @examples
#' \notrun{
#' baby_dann_db <- read_one_bt_activity_file(
#'     infile = here::here('Data', activity_Dann_202305.zip')
#' ) |>
#' clean_bt_list_db()
#' }
read_one_bt_activity_file <- function(
		infile, list_db = NULL, verbose = FALSE
) {
	if (!file.exists(infile)) {
		stop(glue::glue('File {infile} does not exist.'))
	}
	if (grepl('\\.zip$',infile)) {
		unzip(infile, exdir = dirname(infile))
		infile <- gsub('\\.zip','.txt',infile)
	}
	if (!grepl('activity_[A-Za-z]*_[0-9]{6}\\.(zip|txt)',basename(infile))) {
		stop('Not a BabyTime Activity File\nMust be a file name "activity_BabyName_yyyymm.txt"')
	}
	# Open Connection
	con  <- file(infile, open = "r")
	# Initialize Variables
	i <- 0
	one_block <- character(0)
	if (is.null(list_db)) {list_db <- list()}
	baby_name <- unlist(stringr::str_extract_all(basename(infile), '[A-Z][A-Za-z]+'))[1]
	# Read the file one line at a time
	while (length(
		one_line <- readLines(con, n = 1, warn = FALSE)
	) > 0) {
		if (verbose) {
			print(glue::glue("{i}: {one_line}"))
		}
		if (one_line == "==================== ") {
			if (length(one_block) == 1) {
				# Just a date and nothing else. skip to next
				one_block <- character(0)
				next
			}
			# New Block
			# Push Block into list of key-value pairs
			date_line <- stringr::str_split(one_block[1], ' ~ ')[[1]]
			block_list <- list(
				baby_name  = baby_name,
				start_dttm = lubridate::ymd_hm(date_line[1], tz = Sys.timezone()),
				end_dttm   = lubridate::ymd_hm(date_line[2], tz = Sys.timezone())
			)
			for (j in c(2:length(one_block))) {
				thisline <- one_block[j]
				if (thisline == '' | grepl('^[[:space:]]$',thisline)) {
					next
				}
				# Merge Memo into a single thing
				if (grepl('^Memo:', thisline)) {
					thisline <- str_trim(paste(one_block[j:length(one_block)], collapse = '\t'))
				}

				key_val <- stringr::str_trim(stringr::str_split(thisline, ': ')[[1]])
				if (key_val[1] != '') {
					block_list[[key_val[1]]] <- key_val[2]
				}
				if (key_val[1] == 'Memo') {
					# Done here. don't need multiple lines from memo showing up as keys
					one_block[(j+1):length(one_block)] <- rep.int('', length((j+1):length(one_block)))
				}
			}
			# Push Block into table within the list_db variable
			if (is.null(block_list[['Type']])) {
				warning(glue::glue('Could not process line {i} of {basename(infile)}. Missing value for Type.'))
			} else {
				if (is.null(list_db[[block_list$Type]])) {
					list_db[[block_list$Type]] <- dplyr::as_tibble(block_list)
				} else {
					list_db[[block_list$Type]] <- dplyr::bind_rows(list_db[[block_list$Type]],dplyr::as_tibble(block_list))
				}
			}
			# Get ready for next batch
			rm(block_list)
			one_block <- character(0)
		} else {
			one_block <- append(one_block, one_line)
		}
		i <- i + 1
	}
	close(con)
	return(list_db)
}

#' Title
#'
#' @param list_db output from `read_one_bt_activity_file`
#'
#' @return a Clean BT List DB object
#' @export
#'
#' @examples
#' \notrun{
#' baby_dann_db <- read_one_bt_activity_file(
#'     infile = here::here('Data', activity_Dann_202305.zip')
#' ) |>
#' clean_bt_list_db()
#' }
clean_bt_list_db <- function(list_db) {
	## Now that we've parsed the file into a list, clean the list
	# First, combine like things
	## Sleep
	tables <- names(list_db)[grepl('Sleep$', names(list_db))]
	x <- bind_rows(list_db[tables])
	list_db[tables] <- NULL
	list_db$Sleep <- x
	## Breastfeeding
	tables <- names(list_db)[grepl('^Breastfeeding', names(list_db))]
	x <- dplyr::bind_rows(list_db[tables])
	list_db[tables] <- NULL
	list_db$Breastfeeding <- x

	## Clean Names and add Baby Name as a column
	names(list_db) <- snakecase::to_snake_case(names(list_db))
	for (i in names(list_db)) {
		list_db[[i]] <- janitor::clean_names(list_db[[i]])
		list_db[[i]] <- dplyr::mutate(list_db[[i]], baby_name = baby_name, .before = start_dttm)

	}
	class(list_db) <- 'Clean BT List DB'
	return(list_db)
}


#' Combine two Babylist List DBs
#'
#' Generally not needed if you parse all files at once, but can be useful if
#' combining data extracted at multiple times or across multiple directories
#'
#' @param la a Clean BT List DB object
#' @param lb another Clean BT List DB object
#'
#' @return a Clean BT List DB object that contains all elements from `la` and `lb`
#' @export
#'
#' @examples
#' \notrun{
#' baby_dann_db_may <- read_one_bt_activity_file(
#'     infile = here::here('Data', activity_Dann_202305.zip')
#' ) |>
#' clean_bt_list_db()
#' baby_dann_db_june <- read_one_bt_activity_file(
#'     infile = here::here('Data', activity_Dann_202306.zip')
#' ) |>
#' clean_bt_list_db()
#' baby_dann_db <- combine_clean_bt_list_dbs(
#'     baby_dann_db_may,
#'     baby_dann_db_june
#' )
#' }
combine_clean_bt_list_dbs <- function(la,lb) {
	if (class(la) != 'Clean BT List DB') {
		stop("First Parameter not a cleaned BabyTime List Database\nUse the output of `clean_bt_list_db`")
	}
	if (class(lb) != 'Clean BT List DB') {
		stop("Second Parameter not a cleaned BabyTime List Database\nUse the output of `clean_bt_list_db`")
	}
	lc <- list()
	all_tables <- unique(c(names(la),names(lb)))
	for (t in all_tables) {
		lax <- la[[t]]
		lbx <- lb[[t]]
		lc[[t]] <- dplyr::bind_rows(lax,lbx)
	}
	class(lc) <- 'Clean BT List DB'
	return(lc)
}
