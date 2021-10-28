#' Body Mass Index
#'
#' This function calculates BMI records using height and weight results from
#' CPRD GOLD.
#'
#' It assumes that your cohort contains a column named patid and that the weight
#' and height tables have already been extracted.
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all systolic and diastolic blood pressure records
#' @export
bmi <- function(cohort, height, weight) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve patient table entries for cohort and then convert each patient's
  # yob to a date format assuming first day of the year (e.g. "1997-01-01").

  patient_table <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("patient"))) %>%
    filter(patid %in% cohort_patid) %>%
    select(patid,
           yob,
           end_CPRD_fu) %>%
    collect() %>%
    mutate(yob = ymd(paste0(as.character(yob), "0101")))

  # Rename columns

  height <-
    height %>%
    rename(centile_data2 = height_centile_data2)

  weight <-
    weight %>%
    rename(centile_data2 = weight_centile_data2)

  # Combine patient and height_weight tables.

  height_2 <-
    left_join(x = height,
              y = patient_table,
              by = "patid")

  weight_2 <-
    left_join(x = weight,
              y = patient_table,
              by = "patid")

  # Exclude records where the event date is less than 18 years after yob or is
  # greater than end_CPRD_fu. Then exclude weights outside the range 20-635kg
  # and heights outside the range 121-214cm (i.e. 4-7ft).

  height_2 <-
    height_2 %>%
    filter(!(eventdate < yob + (18*365.25) |
               eventdate > end_CPRD_fu)) %>%
    select(-c(yob, end_CPRD_fu)) %>%
    filter(!(data1 < 1.21 | data1 > 2.14))

  weight_2 <-
    weight_2 %>%
    filter(!(eventdate < yob + (18*365.25) |
               eventdate > end_CPRD_fu)) %>%
    select(-c(yob, end_CPRD_fu)) %>%
    filter(!(data1 < 20 | data1 > 635))

  # Remove duplicates

  height_2 <-
    height_2 %>%
    distinct(patid,
             eventdate,
             data1,
             .keep_all = TRUE)

  weight_2 <-
    weight_2 %>%
    distinct(patid,
             eventdate,
             data1,
             .keep_all = TRUE)

  # If more than one weight record (height record) taken on a single day then
  # substitute the mean of those weight measurements (height measurements) as
  # long as the standard deviation of less than or equal to 3; otherwise, exclude
  # the weight records (height records). Then remove the new duplicated rows.

  height_2 <-
    height_2 %>%
    group_by(patid, eventdate) %>%
    mutate(mean = mean(data1),
           sd = sd(data1)) %>%
    filter(is.na(sd) | sd <= 3) %>%
    mutate(data1 = mean) %>%
    select(-c(mean, sd)) %>%
    slice_head() %>%
    ungroup()

  weight_2 <-
    weight_2 %>%
    group_by(patid, eventdate) %>%
    mutate(mean = mean(data1),
           sd = sd(data1)) %>%
    filter(is.na(sd) | sd <= 3) %>%
    mutate(data1 = mean) %>%
    select(-c(mean, sd)) %>%
    slice_head() %>%
    ungroup()

  # Select relevant columns

  height_3 <-
    height_2 %>%
    select(patid, eventdate, data1)

  weight_3 <-
    weight_2 %>%
    select(patid, eventdate, data1)

  # Combine height and weight tables

  defaultW <- getOption("warn")

  options(warn = -1)

  height_weight <-
    full_join(x = weight_3,
              y = height_3,
              by = "patid") %>%
    rename(eventdate.weight = eventdate.x,
           eventdate.height = eventdate.y,
           data1.weight = data1.x,
           data1.height = data1.y) %>%
    group_by(patid) %>%
    arrange(patid, eventdate.weight, eventdate.height) %>%
    mutate(bmi = case_when(eventdate.weight == eventdate.height ~ data1.weight/data1.height^2)) %>%
    ungroup() %>%
    group_by(patid, eventdate.weight) %>%
    mutate(nearest_height = case_when(
      eventdate.weight != eventdate.height ~
        date_near(dates = eventdate.height, target = eventdate.weight, onlypre = TRUE, sidepref = 'l')),
      nearest_height2 = if_else(eventdate.weight == min(eventdate.height),
                                min(eventdate.height),
                                nearest_height)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(nearest_height3 = min(c_across(cols = c(nearest_height,
                                                   nearest_height2)),
                                 na.rm = TRUE))

  options(warn = defaultW)

  height_weight <-
    height_weight %>%
    mutate(nearest_height4 = case_when(
      is.na(as.character(bmi)) & is.na(as.character(nearest_height3)) ~
        date_near(dates = eventdate.height, target = eventdate.weight, onlypre = FALSE, sidepref = 'r')))

  height_weight <-
    height_weight %>%
    mutate(height_date = case_when(
      !(is.na(as.character(bmi))) ~ eventdate.height,
      is.na(as.character(bmi)) & !is.na(as.character(nearest_height)) ~ nearest_height,
      is.na(as.character(bmi)) & is.na(as.character(nearest_height)) & !is.na(as.character(nearest_height2)) ~ nearest_height2,
      is.na(as.character(bmi)) & is.na(as.character(nearest_height)) & is.na(as.character(nearest_height2)) & !is.na(as.character(nearest_height3)) ~ nearest_height3,
      is.na(as.character(bmi)) & is.na(as.character(nearest_height)) & is.na(as.character(nearest_height2)) & is.na(as.character(nearest_height3)) & !is.na(as.character(nearest_height4)) ~ nearest_height4)) %>%
    select(patid, eventdate.weight, data1.weight, eventdate.height, data1.height, height_date) %>%
    group_by(patid) %>%
    mutate(height_index = match(x = height_date, table = eventdate.height),
           id = row_number()) %>%
    ungroup()

  height_weight <-
    left_join(x = height_weight,
              y = height_weight %>% select(patid, data1.height, id),
              by = c("patid",
                     "height_index" = "id")) %>%
    select(patid, eventdate.weight, data1.weight, height_date, data1.height.y) %>%
    rename(height_record = data1.height.y)

  # Calculate BMI and if no height record is available, retrieve CPRD recorded
  # BMI record.

  weight_bmi <-
    weight %>%
    select(patid, eventdate, data3)

  height_weight <-
    height_weight %>%
    left_join(x = .,
              y = weight_bmi,
              by = c("patid",
                     "eventdate.weight" = "eventdate")) %>%
    mutate(bmi = case_when(
      !is.na(height_record) ~ data1.weight/height_record^2,
      is.na(height_record) ~ data3)) %>%
    select(patid, eventdate.weight, data1.weight, height_date, height_record, bmi) %>%
    rename(weight_record = data1.weight,
           weight_date = eventdate.weight) %>%
    filter(!(bmi < 5 | bmi > 200))

  return(height_weight)

}
