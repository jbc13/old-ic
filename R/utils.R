# Funções criadas pelo professor Jonatan

##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange_vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}


replace_inf <- function(x) ifelse(!is.finite(x), NA, x)
#replace_inf(c(Inf, -Inf, 1, NA))

# Apply fun dealing with vectors values that may be all NA ---------------------
fun_NA <- function(x, fun, ...){
  if(all(is.na(x))) return(NA)
  fun(x, ...)
}


replace_NA <- function(x, fill.value = 0) {
  #x <- summary_qc_j$tot
  replace(x, is.na(x), fill.value)
}


point <- scales::format_format(big.mark = ".",
                               decimal.mark = ",",
                               scientific = FALSE)

###############################################################################

# Funções extras que criei

## Calcula a porcentagem
pctg <- function(value, total, houses = 1)
{ p = round(((value/total) * 100), houses)
  return(p) }

## Calcula a porcentagem de NAs
pctg_NA <- function(var, houses = 1, valid = FALSE)
{ p1 = round(((sum(is.na(var))/length(var)) * 100), houses)
  if (valid == FALSE) {p2 = p1}
  if (valid == TRUE) {p2 = 100 - p1}
  return(p2) }

## Salva determinado produto automáticamente na pasta "output"
saveRDS_product <- function(NUMBER_PRODUCT, DATA_PRODUCT, PREFIX, ID)
{ if (NUMBER_PRODUCT == 1) {prod_file_name <- "../output/qcID-application-PREFIX.rds"}
  if (NUMBER_PRODUCT == 2) {prod_file_name <- "../output/qcID-metadata-PREFIX.rds"}
  if (NUMBER_PRODUCT == 3) {prod_file_name <- "../output/qcID-summary-PREFIX.rds"}
  prod_file <- prod_file_name %>%
    str_replace("PREFIX", as.character(PREFIX)) %>%
    str_replace("ID", as.character(ID))
  saveRDS(DATA_PRODUCT, file = prod_file) }

## Determina o número de EMAs adicionadas a cada ano
emas_by_year <- function(info, sdate) {
  info_mut <- # coluna 'year' adicionada ao conjunto 'info' original
    info %>%
    mutate(year = lubridate::year(info[[sdate]])) %>%
    select(site, state, name, year, everything())
  syear <- # ano inicial do conjunto de emas
    min(info_mut$year)
  eyear <- # ano final do conjunto de emas
    max(info_mut$year)
  en <- # número de emas por ano sem a adição de anos com zero emas
    info_mut %>%
    group_by(year) %>%
    summarise(emas_number = length(site)) %>%
    ungroup()
  seq_year <- # sequência de anos baseadas no ano inicial e final
    syear:eyear
  missing_year <- # anos com zero emas adicionadas
    seq_year[which((seq_year %in% en[['year']]) == FALSE)]
  dt_my <- # data table com os anos onde nenhuma ema foi adicionada
    data.table::data.table(year = missing_year, emas_number = 0)
  bind_en_dt <- rbind(en, dt_my)
  eby <- # organizando a ordem dos anos
    dplyr::arrange(bind_en_dt, year)
  return(eby)
}

################################################################################

temp.summary <- function(DATA, INFO, TYPE, HOUSES, SAVE = F, DIRECTORY) { 
  per <-
    DATA %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      start = as.Date(min(date)), # data inicial
      end = as.Date(max(date)), # data final
      period = round(time_length(end - start, unit = "year"), 1)) %>%
    ungroup()
  perinfo <-
    dplyr::full_join(per, INFO, by = "site") %>%
    dplyr::select(site, state, name, everything())
  availability <-
    DATA %>% 
    dplyr::group_by(site) %>% 
    dplyr::summarise(
    total = n(),
    valid_tinst = sum(!is.na(tinst)),
    valid_tmin = sum(!is.na(tmin)),
    valid_tavg = sum(!is.na(tavg)),
    valid_tmax = sum(!is.na(tmax)),
    valid_tdinst = sum(!is.na(tdinst)),
    valid_tdmin = sum(!is.na(tdmin)),
    valid_tdavg = sum(!is.na(tdavg)),
    valid_tdmax = sum(!is.na(tdmax)),
    pctg_valid_tinst = pctg(valid_tinst, total, HOUSES),
    pctg_valid_tmin = pctg(valid_tmin, total, HOUSES),
    pctg_valid_tavg = pctg(valid_tavg, total, HOUSES),
    pctg_valid_tmax = pctg(valid_tmax, total, HOUSES),
    pctg_valid_tdinst = pctg(valid_tinst, total, HOUSES),
    pctg_valid_tdmin = pctg(valid_tdmin, total, HOUSES),
    pctg_valid_tdavg = pctg(valid_tdavg, total, HOUSES),
    pctg_valid_tdmax = pctg(valid_tdmax, total, HOUSES)
    ) %>% 
    ungroup()
  names(availability)[2] <- paste0(TYPE, "_total")
  names(availability)[3] <- paste0(TYPE, "_valid_tinst")
  names(availability)[4] <- paste0(TYPE, "_valid_tmin")
  names(availability)[5] <- paste0(TYPE, "_valid_tavg")
  names(availability)[6] <- paste0(TYPE, "_valid_tmax")
  names(availability)[7] <- paste0(TYPE, "_valid_tdinst")
  names(availability)[8] <- paste0(TYPE, "_valid_tdmin")
  names(availability)[9] <- paste0(TYPE, "_valid_tdavg")
  names(availability)[10] <- paste0(TYPE, "_valid_tdmax")
  names(availability)[11] <- paste0(TYPE, "_pctg_valid_tinst")
  names(availability)[12] <- paste0(TYPE, "_pctg_valid_tmin")
  names(availability)[13] <- paste0(TYPE, "_pctg_valid_tavg")
  names(availability)[14] <- paste0(TYPE, "_pctg_valid_tmax")
  names(availability)[15] <- paste0(TYPE, "_pctg_valid_tdinst")
  names(availability)[16] <- paste0(TYPE, "_pctg_valid_tdmin")
  names(availability)[17] <- paste0(TYPE, "_pctg_valid_tdavg")
  names(availability)[18] <- paste0(TYPE, "_pctg_valid_tdmax")
  summary <-
    full_join(perinfo, availability, by = "site") %>%
    select(site, state, name, everything())
  if (SAVE == T) {saveRDS(summary, DIRECTORY)}
  return(summary)
}


################################################################################

complete.ema <- function(SUMMARY, PRODUCT3) {
  n <- which((SUMMARY$site %in% PRODUCT3$site) == FALSE)
  QC <- unique(PRODUCT3$qc)
  N <- unique(PRODUCT3$n_obs)
  empty <-
    SUMMARY[n,] %>%
    dplyr::select(site) %>%
    dplyr::mutate(qc = QC, n_obs = N, tot = NA, perc = NA, n_events = NA, n_max = NA)
  ce <-
    dplyr::bind_rows(PRODUCT3, empty) %>%
    dplyr::arrange(desc(tot), site)
  return(ce)
}
























