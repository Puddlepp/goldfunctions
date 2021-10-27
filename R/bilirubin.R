#' Serum Bilirubin
#'
#' This function extracts serum bilirubin records from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' Codes:
#'
#' \tabular{lll}{
#' Medcode \tab Readcode \tab Description\cr
#' 59 \tab 44E..00 \tab Serum bilirubin level\cr
#' 66 \tab 44EC.00 \tab Serum total bilirubin level\cr
#' 1183 \tab 44E2.00 \tab Serum bilirubin raised\cr
#' 13724 \tab 44E3.00 \tab Total bilirubin\cr
#' 13725 \tab 44E9.00 \tab Plasma total bilirubin level\cr
#' 26898 \tab 44E1.00 \tab Serum bilirubin normal\cr
#' 37205 \tab 44E6.00 \tab Serum bilirubin borderline\cr
#' 44363 \tab 44EZ.00 \tab Serum bilirubin NOS\cr
#' 101070 \tab 4QCE.00 \tab Bilirubin level\cr
#' 101501 \tab 4QCE000 \tab Bilirubin direct measurement\cr
#' }
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all serum bilirubin records
#' @export
bilirubin <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  bilirubin_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c("44EC.00",
                           "44E..00",
                           "44E3.00",
                           "44E9.00",
                           "44E1.00",
                           "44E2.00",
                           "44EZ.00",
                           "4QCE.00",
                           "44E6.00",
                           "4QCE000")) %>%
    collect()

  # Draw out the medcodes.

  bilirubin_gold_medcode <-
    bilirubin_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all test table entries for the cohort.

  bilirubin_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("test"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% bilirubin_gold_medcode,
           enttype %in% c("158", "288", "438", "480"),
           data3 %in% c("0", "96", "99", "142", "149", "164")) %>%
    collect()

  # Retrieve entity table records for use below.

  bilirubin_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("158", "288", "438", "480")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  bilirubin_cohort_1 <-
    bilirubin_cohort %>%
    mutate(data1_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
                                enttype,
                                data1_lkup),
           data2_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
                                enttype,
                                data2_lkup),
           data3_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
                                enttype,
                                data3_lkup),
           data4_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
                                enttype,
                                data4_lkup),
           data5_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
                                enttype,
                                data5_lkup),
           data6_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
                                enttype,
                                data6_lkup),
           data7_lkup = VLOOKUP(bilirubin_cohort$enttype,
                                bilirubin_entity,
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
                                  bilirubin_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 bilirubin_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, operator_data1, data2, value_data2,
           data3, unit_of_measure_data3, data4, qualifier_data4, data5, normal_range_from_data5,
           data6, normal_range_to_data6)

  return(bilirubin_cohort_1)

}
