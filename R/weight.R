#' Weight
#'
#' This function extracts weight records from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all weight records
#' @export
weight <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Identify relevant enttypes and units of measurement (on separate script).

  weight_gold_enttype <-
    c("13")

  # Retrieve all clinical table entries for the cohort.

  weight_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("clinical"))) %>%
    filter(patid %in% cohort_patid,
           enttype == "13") %>%
    collect()

  # Draw out the adids.

  weight_adid <-
    weight_cohort %>%
    pull(adid)

  # Retrieve all additional table entries for the cohort.

  weight_additional <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("additional"))) %>%
    filter(adid %in% weight_adid) %>%
    collect()

  # Combine the clinical and additional tables.

  weight_cohort_adid <-
    left_join(x = weight_cohort,
              y = weight_additional,
              by = c("patid",
                     "eventdate",
                     "sysdate",
                     "enttype",
                     "adid"))

  # Draw out the medcodes.

  weight_gold_medcode <-
    weight_cohort_adid %>%
    pull(medcode)

  # Retrieve medical table entries for the medcodes for use below.

  weight_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(medcode %in% weight_gold_medcode) %>%
    collect()

  # Retrieve entity table records for use below.

  weight_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype == "13") %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  weight_cohort_1 <-
    weight_cohort_adid %>%
    mutate(weight_centile_data2 = VLOOKUP(paste0("CEN", data2),
                                          lookup,
                                          together,
                                          text),
           unit_of_measure_data1 = "Weight in kilos",
           unit_of_measure_data3 = "BMI",
           consultation_type = VLOOKUP(paste0("SED", constype),
                                       lookup,
                                       together,
                                       text),
           medical_type = VLOOKUP(medcode,
                                  weight_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 weight_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, unit_of_measure_data1,
           data2, weight_centile_data2, data3, unit_of_measure_data3)

  return(weight_cohort_1)

}
