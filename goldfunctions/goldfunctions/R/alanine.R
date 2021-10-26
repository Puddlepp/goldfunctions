#' Alanine Aminotransferase (ALT)
#'
#' This function extracts alanine aminotransferase test results from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all alanine aminotransferase records
#' @export
alanine <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  alanine_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c("44G..11",
                           "44G..12",
                           "44G3.00",
                           "44G3000",
                           "44G3100",
                           "44GA.00",
                           "44GB.00")) %>%
    collect()

  # Draw out the medcodes.

  alanine_gold_medcode <-
    alanine_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all test table entries for the cohort.

  alanine_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("test"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% alanine_gold_medcode,
           enttype %in% c("155",
                          "288",
                          "480"),
           data3 %in% c("0",
                        "61",
                        "127",
                        "138",
                        "142",
                        "164",
                        "277")) %>%
    collect()

  # Retrieve entity table records for use below.

  alanine_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("155", "288", "480")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  alanine_cohort_1 <-
    alanine_cohort %>%
    mutate(data1_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data1_lkup),
           data2_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data2_lkup),
           data3_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data3_lkup),
           data4_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data4_lkup),
           data5_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data5_lkup),
           data6_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data6_lkup),
           data7_lkup = VLOOKUP(.lookup_values = alanine_cohort$enttype,
                                .data = alanine_entity,
                                .lookup_column = enttype,
                                .return_column = data7_lkup),
           operator_data1 = VLOOKUP(.lookup_values = paste0(data1_lkup, data1),
                                    .data = lookup,
                                    .lookup_column = together,
                                    .return_column = text),
           value_data2 = "Value",
           unit_of_measure_data3 = VLOOKUP(.lookup_values = paste0(data3_lkup, data3),
                                           .data = lookup,
                                           .lookup_column = together,
                                           .return_column = text),
           qualifier_data4 = VLOOKUP(.lookup_values = paste0(data4_lkup, data4),
                                     .data = lookup,
                                     .lookup_column = together,
                                     .return_column = text),
           normal_range_from_data5 = "Normal range from",
           normal_range_to_data6 =  "Normal range to",
           normal_range_basis_data7 = VLOOKUP(.lookup_values = paste0(data7_lkup, data7),
                                              .data = lookup,
                                              .lookup_column = together,
                                              .return_column = text),
           consultation_type = VLOOKUP(.lookup_values = paste0("SED", constype),
                                       .data = lookup,
                                       .lookup_column = together,
                                       .return_column = text),
           medical_type = VLOOKUP(.lookup_values = medcode,
                                  .data = alanine_gold,
                                  .lookup_column = medcode,
                                  .return_column = desc),
           entity_type = VLOOKUP(.lookup_values = enttype,
                                 .data = alanine_entity,
                                 .lookup_column = enttype,
                                 .return_column = description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, operator_data1, data2, value_data2,
           data3, unit_of_measure_data3, data4, qualifier_data4, data5, normal_range_from_data5,
           data6, normal_range_to_data6)

  return(alanine_cohort_1)

}
