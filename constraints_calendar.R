library(tidyverse)
library(lubridate)

# schedule as list with start, end, holidays
dates <- list(
  start = as.Date('2024-07-01'), 
  end = as.Date('2024-12-31'),
  week = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
  weekend = c("Saturday", "Sunday"),
  holiday_major = as.Date(c('2024-07-04', '2024-09-02', '2024-11-28', '2024-12-25', '2025-01-01', '2025-05-26')),
  holiday_uic = as.Date(c('2024-07-04', '2024-09-02', '2024-11-28', '2024-12-25', '2025-01-01', '2025-01-20', '2025-05-26', '2025-06-19')),
  holiday_jbva = as.Date(c('2024-07-04', '2024-09-02', '2024-10-14', '2024-12-25', '2025-01-01', '2025-02-17', '2025-06-19'))
)
# name holidays
names(dates$holiday_major) <- c('Fourth of July', 'Labor Day', 'Thanksgiving', 'Christmas', 'New Years Day', 'Memorial Day')
names(dates$holiday_uic) <- c('Fourth of July', 'Labor Day', 'Thanksgiving', 'Christmas', 'New Years Day', 'MLK Day', 'Memorial Day', 'Juneteenth')
names(dates$holiday_jbva) <- c('Fourth of July', 'Labor Day', 'Indigenous Peoples Day', 'Christmas', 'New Years Day', 'Washington’s Birthday', 'Juneteenth')

#glimpse(dates)
#print(dates)


# create tibble of calendar from start to end dates
calendar <- tibble(date = seq(dates$start, dates$end, by = "1 day")) |> mutate(dow = weekdays(date))
# glimpse(calendar)


# create schedule of dates by each shift (holiday, night, short, day) by start time, end time, duration
# each row is a single day
# https://library.virginia.edu/data/articles/working-with-dates-and-time-in-r-using-the-lubridate-package

## function for standard timezone
set_start_time <- function(x, t) {as.POSIXct(paste(x, t), tz = "America/Chicago")}

sched_uic <- calendar |> mutate(

  ## holiday calls 0800 to 0800 next day on holidays
  holiday_start = case_when(date %in% dates$holiday_uic ~ set_start_time(date, "08:00:00")),
  holiday_end = holiday_start + days(1),
  holiday_dur = as.duration(holiday_end - holiday_start),

  ## night calls 2000 to 0800 next day on non-holidays
  night_start = case_when(is.na(holiday_start) ~ set_start_time(date, "20:00:00")),
  night_end = night_start + hours(12),
  night_dur = as.duration(night_end - night_start),

  ## short calls 0430 to 2000 on non-holidays from Monday to Friday
  short_start = case_when(is.na(holiday_start) & dow %in% dates$week ~ set_start_time(date, "16:30:00")),
  short_end = short_start + minutes(60*3.5),
  short_dur = as.duration(short_end - short_start),

  ## day calls 0800 to 2000 on non-holidays from Saturday to Sunday
  day_start = case_when(is.na(holiday_start) & dow %in% dates$weekend ~ set_start_time(date, "08:00:00")),
  day_end = day_start + hours(12),
  day_dur = as.duration(day_end - day_start)
)

## check output in console
print(sched_uic, n = 20)
glimpse(sched_uic)

## print to .csv file in folder
write.table(sched_uic, file = "schedule_empty.csv", sep = ", ", row.names = F)


# create all shifts with start time, end time, duration
# each row is a single shift

## function trims data to single time element (start, end, or duration)
## then pivots it into a longer, tidy dataset
trim_pivot_sched <- function(df, suffix, name_time) {
    df |> 
      select(date, dow, ends_with(suffix)) |>
      pivot_longer(cols = ends_with(suffix), names_to = "shift", values_to = name_time, values_drop_na = TRUE) |>
      mutate(shift =  str_remove(shift, suffix))
}

## merge the three datasets by date, dow, shift so each shift row has a start, end, duration
shifts_uic <- reduce(list(
  sched_uic |> trim_pivot_sched(suffix = "_start", name_time = "time_start"),
  sched_uic |> trim_pivot_sched(suffix = "_end", name_time = "time_end"),
  sched_uic |> trim_pivot_sched(suffix = "_dur", name_time = "duration")
), full_join, by = c("date", "dow", "shift")) |> 
### sort by time_start
arrange(time_start)

## check output in console
print(shifts_uic, n = 20)
glimpse(shifts_uic)

## print to .csv file in folder
write.table(shifts_uic, file = "shifts_empty.csv", sep = ", ", row.names = F)

