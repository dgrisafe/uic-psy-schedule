library(tidyverse)
library(lubridate)

# schedule as list with start, end, holidays
dates <- list(

  start = as.Date('2024-07-01'), 
  end = as.Date('2024-12-31'),
  week = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
  weekend = c("Saturday", "Sunday"),

  holiday_major = as.Date(c('2024-07-04', '2024-09-02', '2024-11-28', '2024-12-25', '2025-01-01', '2025-05-26')),

  # UIC holiday shifts UIC residents cover
  holiday_uic = as.Date(c('2024-07-04', '2024-09-02', '2024-11-28', '2024-12-25', '2025-01-01', '2025-01-20', '2025-05-26', '2025-06-19')),

  # JBVA holiday shifts UIC residents cover
  holiday_jbva = as.Date(c('2024-07-04', '2024-09-02', '2024-10-14', '2024-12-25', '2025-01-01', '2025-02-17', '2025-06-19')),

  # JBVA holiday shifts outside residents cover
  holiday_jbva_nwm = as.Date(c('2024-11-28', '2025-01-20', '2025-05-26')),
  # JBVA night shifts outside residents cover
  night_jbva_nwm = as.Date(c('2024-09-09', '2024-09-10', '2024-09-11', '2024-09-12', '2024-09-13')),
  # JBVA short shifts outside residents cover
  short_jbva_nwm = as.Date(c('2024-10-08', '2024-11-13')),
  # JBVA day shifts outside residents cover
  day_jbva_nwm = as.Date(c('2024-11-10', '2024-11-23'))

)

# name holidays
names(dates$holiday_major) <- c('Fourth of July', 'Labor Day', 'Thanksgiving', 'Christmas', 'New Years Day', 'Memorial Day')
names(dates$holiday_uic) <- c('Fourth of July', 'Labor Day', 'Thanksgiving', 'Christmas', 'New Years Day', 'MLK Day', 'Memorial Day', 'Juneteenth')
names(dates$holiday_jbva) <- c('Fourth of July', 'Labor Day', 'Indigenous Peoples Day', 'Christmas', 'New Years Day', 'Washington’s Birthday', 'Juneteenth')
names(dates$holiday_jbva_nwm) <- c('Thanksgiving', 'MLK Day', 'Memorial Day')

#glimpse(dates)
#print(dates)


# create tibble of calendar from start to end dates
calendar <- tibble(date = seq(dates$start, dates$end, by = "1 day")) |> mutate(dow = weekdays(date))
# glimpse(calendar)


# create schedule of dates by each shift (holiday, night, short, day) by start time, end time, duration
#   each row is a single day
#   https://library.virginia.edu/data/articles/working-with-dates-and-time-in-r-using-the-lubridate-package
create_sched <- function(df_cal = calendar, li_holi, li_holi_nwm = NULL) {

  ## function for standard timezone
  set_start_time <- function(x, t) {as.POSIXct(paste(x, t), tz = "America/Chicago")}

  ## define possible shifts: holiday, night, short, day
  sched <- df_cal |> mutate(

    ## holiday calls 0800 to 0800 next day on holidays
    holiday_start = case_when(date %in% li_holi ~ set_start_time(date, "08:00:00")),
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

  # remove holiday dates covered by outside residents, if dates supplied
  if(!is.null(li_holi_nwm)) { sched <- sched |> filter(!(date %in% li_holi_nwm)) }

  ## check output in console
  print(sched, n = 20)
  glimpse(sched)

  return(sched)

}

# schedule for UIC
sched_uic <- create_sched(df_cal = calendar, li_holi = dates$holiday_uic)
## print to .csv file in folder
write.table(sched_uic, file = "schedule_uic_empty.csv", sep = ", ", row.names = F)

# schedule for JBVA
sched_jbva <- create_sched(df_cal = calendar, li_holi = dates$holiday_jbva, li_holi_nwm = dates$holiday_jbva_nwm)
## print to .csv file in folder
write.table(sched_jbva, file = "schedule_jbva_empty.csv", sep = ", ", row.names = F)


# create all shifts with start time, end time, duration
# each row is a single shift
create_shifts <- function(sched, li_night_nwm = NULL, li_short_nwm = NULL, li_day_nwm = NULL) {

  ## function trims data to single time element (start, end, or duration)
  ## then pivots it into a longer, tidy dataset
  trim_pivot_sched <- function(df, suffix, name_time) {
      df |> 
        select(date, dow, ends_with(suffix)) |>
        pivot_longer(cols = ends_with(suffix), names_to = "shift", values_to = name_time, values_drop_na = TRUE) |>
        mutate(shift =  str_remove(shift, suffix)) |>
        filter(
		!(date %in% li_night_nwm & shift == "night") &
		!(date %in% li_short_nwm & shift == "short") &
		!(date %in% li_day_nwm & shift == "day")
	  )
  }

  ## merge the three datasets by date, dow, shift so each shift row has a start, end, duration
  shifts <- reduce(list(
    sched |> trim_pivot_sched(suffix = "_start", name_time = "time_start"),
    sched |> trim_pivot_sched(suffix = "_end", name_time = "time_end"),
    sched |> trim_pivot_sched(suffix = "_dur", name_time = "duration")
  ), full_join, by = c("date", "dow", "shift")) |> 
  ### sort by time_start
  arrange(time_start)

  ## check output in console
  print(shifts, n = 20)
  glimpse(shifts)

  return(shifts)

}

# shifts for UIC
shifts_uic <- create_shifts(sched = sched_uic)
## print to .csv file in folder
write.table(shifts_uic, file = "shifts_uic_empty.csv", sep = ", ", row.names = F)

# shifts for JBVA
shifts_jbva <- create_shifts(sched = sched_jbva, 
  li_night_nwm = dates$night_jbva_nwm, 
  li_short_nwm = dates$short_jbva_nwm, 
  li_day_nwm = dates$day_jbva_nwm
)
## print to .csv file in folder
write.table(shifts_jbva, file = "shifts_jbva_empty.csv", sep = ", ", row.names = F)



# 2/7/25: as written, the shifts outside residents cover at JBVA are removed from the list of shifts.
# this assumes uic starts making assigning their shifts with the outside residents have first pick
# this is probably not a valid assumption, and may make more sense to list all possible shifts
# then have the constraints for the number of shifts the outside residents have, and any other unforseen preferences
# will leave for now, but consider removing the code that exlucudes shifts a priori and figure out later
