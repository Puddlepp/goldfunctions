#' Serum Creatine
#'
#' This function extracts serum creatine records from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' Codes:
#'
#' Medcode  Readcode  Description
#' 5        44J3.00   Serum creatine
#' 3927     44J3300   Serum creatine raised
#' 13736    44JF.00   Plasma creatinine level
#' 26903    44J3200   Serum creatinine normal
#' 27095    4Q40.00   Creatinine level
#' 35545    44J3100   Serum creatinine low
#' 42345    44J3z00   Serum creatinine NOS
#' 45096    44JD.00   Corrected serum creatinine level
#' 62062    44JC.00   Corrected plasma creatinine level
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all serum creatine records
#' @export
creatine <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  creatine_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c("44J3.00",
                           "44J30000",
                           "44J3100",
                           "44J3200",
                           "44J3300",
                           "44J3z00",
                           "44JC.00",
                           "44JD.00",
                           "44JF.00",
                           "4Q40.00")) %>%
    collect()

  # Draw out the medcodes.

  creatine_gold_medcode <-
    creatine_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all test table entries for the cohort.

  creatine_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("test"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% creatine_gold_medcode,
           enttype %in% c("165", "166", "288", "438", "480"),
           data3 %in% c("0", "82", "96", "99", "138", "142", "149")) %>%
    collect()

  # Retrieve entity table records for use below.

  creatine_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("165", "166", "288", "438", "480")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  creatine_cohort_1 <-
    creatine_cohort %>%
    mutate(data1_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
                                enttype,
                                data1_lkup),
           data2_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
                                enttype,
                                data2_lkup),
           data3_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
                                enttype,
                                data3_lkup),
           data4_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
                                enttype,
                                data4_lkup),
           data5_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
                                enttype,
                                data5_lkup),
           data6_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
                                enttype,
                                data6_lkup),
           data7_lkup = VLOOKUP(creatine_cohort$enttype,
                                creatine_entity,
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
                                  creatine_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 creatine_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, operator_data1, data2, value_data2,
           data3, unit_of_measure_data3, data4, qualifier_data4, data5, normal_range_from_data5,
           data6, normal_range_to_data6)

  return(creatine_cohort_1)

}
