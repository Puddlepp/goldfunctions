#' Aspartate Aminotransferase (AST)
#'
#' This function extracts aspartate aminotransferase test results from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all aspartate aminotransferase records
#' @export
aspartate <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  aspartate_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c("44H5.00",
                           "44H5.11",
                           "44HB.00",
                           "44HB.11",
                           "44HC.00")) %>%
    collect()

  # Draw out the medcodes.

  aspartate_gold_medcode <-
    aspartate_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all test table entries for the cohort.

  aspartate_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("test"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% aspartate_gold_medcode,
           enttype %in% c("156", "288", "332", "480"),
           data3 %in% c("0", "59", "61", "127", "277")) %>%
    collect()

  # Retrieve entity table records for use below.

  aspartate_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("156", "288", "332", "480")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  aspartate_cohort_1 <-
    aspartate_cohort %>%
    mutate(data1_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
                                enttype,
                                data1_lkup),
           data2_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
                                enttype,
                                data2_lkup),
           data3_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
                                enttype,
                                data3_lkup),
           data4_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
                                enttype,
                                data4_lkup),
           data5_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
                                enttype,
                                data5_lkup),
           data6_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
                                enttype,
                                data6_lkup),
           data7_lkup = VLOOKUP(aspartate_cohort$enttype,
                                aspartate_entity,
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
                                  aspartate_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 aspartate_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, operator_data1, data2, value_data2,
           data3, unit_of_measure_data3, data4, qualifier_data4, data5, normal_range_from_data5,
           data6, normal_range_to_data6)

  return(aspartate_cohort_1)

}
