library(tidyverse)
library(lubridate)

# schedule as list with start, end, holidays
dates <- list(
  start = as.Date('2024-07-01'), 
  end = as.Date('2024-12-31'),
  holiday_major = as.Date(c('2024-07-04', '2024-09-02', '2024-11-28', '2024-12-25', '2025-01-01', '2025-05-26')),
  holiday_uic = as.Date(c('2024-07-04', '2024-09-02', '2024-11-28', '2024-12-25', '2025-01-01', '2025-01-20', '2025-05-26', '2025-06-19')),
  holiday_jbva = as.Date(c('2024-07-04', '2024-09-02', '2024-10-14', '2024-12-25', '2025-01-01', '2025-02-17', '2025-06-19'))
)

## night float 2000 to 0800 from Sunday to Thursday
## short calls 0430 to 2000 from Monday to Thursday
## Friday calls 0430 Friday to 0800 Saturday
## weekend calls 0800 to 2000 Saturday, 2000 Saturday to 0800 Sunday, 0800 to 2000 Sunday

# name major holidays
names(dates$holiday_major)<- c('Fourth of July', 'Labor Day', 'Thanksgiving', 'Christmas', 'New Years Day', 'Memorial Day')
names(dates$holiday_uic)<- c('Fourth of July', 'Labor Day', 'Thanksgiving', 'Christmas', 'New Years Day', 'MLK Day', 'Memorial Day', 'Juneteenth')
names(dates$holiday_jbva)<- c('Fourth of July', 'Labor Day', 'Indigenous Peoples Day', 'Christmas', 'New Years Day', 'Washington’s Birthday', 'Juneteenth')

glimpse(dates)
print(dates)

# create tibble from start to end dates
sched <- 
  tibble(date = seq(dates$start, dates$end, by = "1 day")) |>
  mutate(dow = weekdays(date))

glimpse(sched)