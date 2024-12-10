
# babyTimeR

<!-- badges: start -->
<!-- badges: end -->

The goal of babyTimeR is to parse the output from the [BabyTime App](https://www.babytime.care/) into usable tibbles for analyzing sleeping and eating habits. 

## Installation

You can install the development version of babyTimeR from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dannhek/babyTimeR")
```

## Example Sleep Graph

Example of tracking sleep over time 

``` r
library(babyTimeR)
if (file.exists('baby_db.RDS')) {
	baby_db <- readRDS('baby_db.RDS')
} else {
	baby_db <- process_one_directory(directory = 'Raw Data')
	saveRDS(baby_db, 'baby_db.RDS')
}

library(tidyverse)
sleep_df <- baby_db$sleep |>
	mutate(
		sleep_night = case_when(
			hour(start_dttm) <= 6 ~ glue("{format(start_dttm-days(1),'%Y%m%d')}-{format(start_dttm,'%Y%m%d')}"),
			hour(start_dttm) >= 19 ~ glue("{format(start_dttm,'%Y%m%d')}-{format(start_dttm+days(1),'%Y%m%d')}"),
			.default = 'Daytime Nap'
		) 
	) |>
	select(-baby_name,-memo,-type) |>
	distinct()

gg_sleep_df <- sleep_df |>
	filter(sleep_night != 'Daytime Nap') |>
	arrange(start_dttm) |> 
	group_by(sleep_night) |> 
	mutate(
		time_awake = difftime(start_dttm, lag(end_dttm), units = 'mins')
	) |> 
	summarise(
		n_sleep_entries = n(), 
		longest_sleep = max_na(duration_min), 
		wake_ups = sum(time_awake > minutes(15), na.rm = T),
		longest_wake_up = max_na(time_awake)
	) 
	
glimpse(gg_sleep_df)

colors <- RColorBrewer::brewer.pal(name = 'Set1', n = 3)
ratio <- 60

ggplot(
	gg_sleep_df |> drop_na(), 
	aes(x = ymd(substr(sleep_night,1,8)))
) +
	# Sleep Length
	geom_bar(
		aes(
			fill = 'Longest Sleep of Night', 
			y = longest_sleep
		), 
		stat = 'identity', 
		alpha = 0.4
	) +
	geom_line(aes(
		color = 'Moving Average (Longest Sleep)', 
		y = zoo::rollmean(longest_sleep, 7, fill = NA, na.rm = T)
	)) +
	# Sleep Count
	geom_line(aes(
		color = 'Number of Wake-ups', 
		y = ratio*n_sleep_entries
	)) +
	# Wake Length
	geom_point(aes(
		shape = 'Longest Wake Period', 
		y = as.numeric(longest_wake_up)
	)) +
	# Scales 
	scale_x_date(
		name = 'Date', 
		date_breaks = '3 weeks', 
		date_labels = '%d%b%y'
	) +
	scale_y_continuous(
		name = 'Longest Sleep', 
		labels = display_time, 
		limits = c(0,10*ratio), 
		breaks = ratio*seq(0.5,9.5,1),
		sec.axis = sec_axis(
			~./ratio,name = 'Number of Wake-ups', 
			breaks = c(0:10)
		)
	) +
	scale_color_manual(
		name = '', 
		values = colors[2:3]
	) +
	scale_fill_manual(
		name = '', 
		values = colors[1]
	) +
	scale_shape(name = '') +
	# Theme/Look'n'Feel
	theme_minimal() +
	theme(
		legend.position = 'bottom',
		axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
		plot.title.position = 'plot',
		plot.caption.position = 'plot'
	) +
	labs(
		title = 'Overnight Sleep Trends for My Baby',
		subtitle = 'Includes sleeps recorded as beginning between 7p and 7a',
		caption = 'Data collected via BabyTime app'
	)
```

