#' Height
#'
#' This function extracts height records from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all height records
#' @export
height <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Identify relevant enttypes and units of measurement (on separate script).

  height_enttype <-
    c("14")

  # Retrieve all clinical table entries for the cohort.

  height_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("clinical"))) %>%
    filter(patid %in% cohort_patid,
           enttype == "14") %>%
    collect()

  # Draw out the adids.

  height_adid <-
    height_cohort %>%
    pull(adid)

  # Retrieve all additional table entries for the cohort.

  height_additional <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("additional"))) %>%
    filter(adid %in% height_adid) %>%
    collect()

  # Combine the clinical and additional tables.

  height_cohort_adid <-
    left_join(x = height_cohort,
              y = height_additional,
              by = c("patid",
                     "eventdate",
                     "sysdate",
                     "enttype",
                     "adid"))

  # Draw out the medcodes.

  height_gold_medcode <-
    height_cohort_adid %>%
    pull(medcode)

  # Retrieve medical table entries for the medcodes for use below.

  height_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(medcode %in% height_gold_medcode) %>%
    collect()

  # Retrieve entity table records for use below.

  height_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("14")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  height_cohort_1 <-
    height_cohort_adid %>%
    mutate(height_centile_data2 = VLOOKUP(paste0("CEN", data2),
                                          lookup,
                                          together,
                                          text),
           unit_of_measure_data1 = "Height in metres",
           consultation_type = VLOOKUP(paste0("SED", constype),
                                       lookup,
                                       together,
                                       text),
           medical_type = VLOOKUP(medcode,
                                  height_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 height_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, unit_of_measure_data1, data2, height_centile_data2)

  return(height_cohort_1)

}
