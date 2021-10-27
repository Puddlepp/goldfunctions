#' Serum Albumin
#'
#' This function extracts serum albumin records from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' Codes:
#'
#' Medcode  Readcode  Description
#' 23       44M4.00   Serum albumin
#' 13708    44MI.00   Plasma albumin level
#' 26892    44M4000   Serum albumin normal
#' 26893    44M4100   Serum albumin low
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all serum albumin records
#' @export
albumin <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  albumin_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c("44M4.00",
                           "44MI.00",
                           "44M4000",
                           "44M4100")) %>%
    collect()

  # Draw out the medcodes.

  albumin_gold_medcode <-
    albumin_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all test table entries for the cohort.

  albumin_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("test"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% albumin_gold_medcode,
           enttype %in% c("152", "201", "288", "480"),
           data3 %in% c("0", "57")) %>%
    collect()

  # Retrieve entity table records for use below.

  albumin_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("152", "201", "288", "480")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  albumin_cohort_1 <-
    albumin_cohort %>%
    mutate(data1_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data1_lkup),
           data2_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data2_lkup),
           data3_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data3_lkup),
           data4_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data4_lkup),
           data5_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data5_lkup),
           data6_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data6_lkup),
           data7_lkup = VLOOKUP(albumin_cohort$enttype,
                                albumin_entity,
                                enttype,
                                data7_lkup),
           operator_data1 = VLOOKUP(paste0(data1_lkup, data1),
                                    lookup,
                                    together,
                                    text),
           value_data2 = "Value",
           unit_of_measure_data3 = VLOOKUP(paste0(data3_lkup, data3),
                                           lookup,
                                           together,
                                           text),
           qualifier_data4 = VLOOKUP(paste0(data4_lkup, data4),
                                     lookup,
                                     together,
                                     text),
           normal_range_from_data5 = "Normal range from",
           normal_range_to_data6 =  "Normal range to",
           normal_range_basis_data7 = VLOOKUP(paste0(data7_lkup, data7),
                                              lookup,
                                              together,
                                              text),
           consultation_type = VLOOKUP(paste0("SED", constype),
                                       lookup,
                                       together,
                                       text),
           medical_type = VLOOKUP(medcode,
                                  albumin_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 albumin_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, operator_data1, data2, value_data2,
           data3, unit_of_measure_data3, data4, qualifier_data4, data5, normal_range_from_data5,
           data6, normal_range_to_data6)

  return(albumin_cohort_1)

}
