
# [Funções para a aplicação dos QCs]

################################################################################

## (Testes da amplitude de variação)

qc_1a <- function(data, variable, exmin, exmax) {
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "VAR")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      qc = "1a",
      suspect = ifelse(VAR > exmax | VAR < exmin, 1, 0)) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n2 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n2] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n3 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n3] <- paste0(variable, '_qc1a')
  return(test)
  }

qc_1b <- function(data, variable, var_low, var_high) {
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Localizar o número da coluna de "data" correspondente à "var_low"
  n2 <- which(names(data) == var_low)
  # Substituir o nome original de "var_low" por 'LOW' em "data"
  names(data)[n2] <- "LOW"
  # Localizar o número da coluna de "data" correspondente à "var_high"
  n3 <- which(names(data) == var_high)
  # Substituir o nome original de "var_high" por 'HIGH' em "data"
  names(data)[n3] <- "HIGH"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "VAR", "LOW" , "HIGH")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      qc = "1b",
      suspect = ifelse(VAR > HIGH | VAR < LOW, 1, 0)) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n4 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n4] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'LOW'
  n5 <- which(names(test) == "LOW")
  # Substituir o nome 'LOW' pela "var_low" em 'test' (processo reverso)
  names(test)[n5] <- var_low
  # Localizar o número da coluna de 'test' correspondente à 'HIGH'
  n6 <- which(names(test) == "HIGH")
  # Substituir o nome 'HIGH' pela "var_high" em 'test' (processo reverso)
  names(test)[n6] <- var_high
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n7 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n7] <- paste0(variable, '_qc1b')
  return(test)
  }

################################################################################

## (Teste de Persistência Temporal: dados repetidos)

qc_2a <- function(data, variable) {
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "VAR")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      qc = "2a",
      suspect = as.integer(crv_detect(x = VAR, thresh = -99, min.steps = 2) )
      ) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n2 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n2] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n3 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n3] <- paste0(variable, '_qc2')
  return(test)
  }

################################################################################

## (Testes de Consistência Interna)

qc_3a <- function(data, var_min, var_max) {
  # Localizar o número da coluna de "data" correspondente à "var_min"
  n1 <- which(names(data) == var_min)
  # Substituir o nome original de "var_min" por 'MIN' em "data"
  names(data)[n1] <- "MIN"
  # Localizar o número da coluna de "data" correspondente à "var_max"
  n2 <- which(names(data) == var_max)
  # Substituir o nome original de "var_max" por 'MAX' em "data"
  names(data)[n2] <- "MAX"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "MIN", "MAX")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      qc = "3a",
      suspect = ifelse(MIN >= MAX, 1, 0)) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'MIN'
  n3 <- which(names(test) == "MIN")
  # Substituir o nome 'MIN' pela "var_min" em 'test' (processo reverso)
  names(test)[n3] <- var_min
  # Localizar o número da coluna de 'test' correspondente à 'MAX'
  n4 <- which(names(test) == "MAX")
  # Substituir o nome 'MAX' pela "var_max" em 'test' (processo reverso)
  names(test)[n4] <- var_max
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n5 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n5] <- paste0(var_min, '_qc3a')
  return(test)
  }

qc_3b <- function(data, variable, var_min, var_max) { # teste igual ao qc_1b com vars diferentes
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Localizar o número da coluna de "data" correspondente à "var_min"
  n2 <- which(names(data) == var_min)
  # Substituir o nome original de "var_min" por 'MIN' em "data"
  names(data)[n2] <- "MIN"
  # Localizar o número da coluna de "data" correspondente à "var_max"
  n3 <- which(names(data) == var_max)
  # Substituir o nome original de "var_max" por 'MAX' em "data"
  names(data)[n3] <- "MAX"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "VAR", "MIN" , "MAX")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      qc = "3b",
      suspect = ifelse(VAR > MAX | VAR < MIN, 1, 0)) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n4 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n4] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'MIN'
  n5 <- which(names(test) == "MIN")
  # Substituir o nome 'MIN' pela "var_min" em 'test' (processo reverso)
  names(test)[n5] <- var_min
  # Localizar o número da coluna de 'test' correspondente à 'MAX'
  n6 <- which(names(test) == "MAX")
  # Substituir o nome 'MAX' pela "var_max" em 'test' (processo reverso)
  names(test)[n6] <- var_max
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n7 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n7] <- paste0(variable, '_qc3b')
  return(test)
  }

qc3c_filter_td <- # Função para a filtragem da variável tdavg que será usada no qc_3c
  function(data, var_inst, var_avg, var_min, var_max, save_repository) {
    d <- data %>% select(site, date, c(var_inst, var_avg, var_min, var_max))
    name_3a <- paste0(var_min, '_qc', '3a')
    name_3b <- paste0(var_inst, '_qc', '3b')
    t3a <- qc_3a(
      data = data, var_min = var_min, var_max = var_max) %>%
      select(name_3a)
    t3b <- qc_3b(
      data = data, variable = var_inst, var_min = var_min, var_max = var_max) %>%
      select(name_3b)
    d_avg <- d %>% select(var_avg)
    avg_filter <- paste0(var_avg,'_','filter')
    names(d_avg) [1] <- avg_filter
    p3a <- which(t3a[[name_3a]] == 1)
    p3b <- which(t3b[[name_3b]] == 1)
    d_avg[p3a,] <- NA
    d_avg[p3b,] <- NA
    dt <- data.table::data.table(d,t3a,t3b,d_avg) %>%
      select(site, date,
        c(var_inst, var_min, var_max, var_avg, avg_filter, name_3a, name_3b))
    saveRDS(object = dt, file = save_repository)
    return(dt)}

qc_3c <- function(data, variable, var_dew) {
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Localizar o número da coluna de "data" correspondente à "var_dew"
  n2 <- which(names(data) == var_dew)
  # Substituir o nome original de "var_dew" por 'DEW' em "data"
  names(data)[n2] <- "DEW"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "VAR", "DEW")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      qc = "3c",
      suspect = ifelse(VAR < DEW, 1, 0)) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n3 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n3] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'DEW'
  n4 <- which(names(test) == "DEW")
  # Substituir o nome 'DEW' pela "var_dew" em 'test' (processo reverso)
  names(test)[n4] <- var_dew
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n5 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n5] <- paste0(variable, '_qc3c')
  return(test)
  }

qc_3d <- function(data, variable, var_min, var_max) {
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Localizar o número da coluna de "data" correspondente à "var_min"
  n2 <- which(names(data) == var_min)
  # Substituir o nome original de "var_min" por 'MIN' em "data"
  names(data)[n2] <- "MIN"
  # Localizar o número da coluna de "data" correspondente à "var_max"
  n3 <- which(names(data) == var_max)
  # Substituir o nome original de "var_max" por 'MAX' em "data"
    names(data)[n3] <- "MAX"
    # Gerar nova "data" com dados diários para aplicação do teste
  data_d <-
    JC.scripts::Break.Dates(DATA = data, VAR = "date", ALL = T) %>%
    dplyr::select(c("site", "year", "month", "day", "VAR", "MIN" , "MAX")) %>%
    dplyr::group_by(site, year, month, day) %>%
    dplyr::summarise(
      N = sum(!is.na(VAR)),
      VAR = sum(VAR, na.rm = TRUE) / N, 
      MIN = min(MIN, na.rm = TRUE),
      MAX = max(MAX, na.rm = TRUE) ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      date = as.Date.character(paste0(year, "/", month, "/", day))) %>%
    dplyr::select(-N) %>% 
    dplyr::select(site, date, everything())
  ## correção, pois foram gerados valores 'NaN', 'Inf' e '-Inf'
  nVAR <- which(is.nan(data_d$VAR))
  nMIN <- which(is.infinite(data_d$MIN))
  nMAX <- which(is.infinite(data_d$MAX))
  data_d$VAR[nVAR] <- NA
  data_d$MIN[nMIN] <- NA
  data_d$MAX[nMAX] <- NA
  test <-
    data_d %>%
    dplyr::group_by(site, year, month, day) %>%
    dplyr::mutate(
      qc = "3d",
      suspect = ifelse(VAR > MAX | VAR < MIN, 1, 0)
      ) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n4 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n4] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'MIN'
  n5 <- which(names(test) == "MIN")
  # Substituir o nome 'MIN' pela "var_min" em 'test' (processo reverso)
  names(test)[n5] <- var_min
  # Localizar o número da coluna de 'test' correspondente à 'MAX'
  n6 <- which(names(test) == "MAX")
  # Substituir o nome 'MAX' pela "var_max" em 'test' (processo reverso)
  names(test)[n6] <- var_max
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n7 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n7] <- paste0(variable, '_qc3d')
  return(test)
  }

qc_3e <- function(data, var_min, var_max) {
  # Localizar o número da coluna de "data" correspondente à "var_min"
  n1 <- which(names(data) == var_min)
  # Substituir o nome original de "var_min" por 'MIN' em "data"
  names(data)[n1] <- "MIN"
  # Localizar o número da coluna de "data" correspondente à "var_max"
  n2 <- which(names(data) == var_max)
  # Substituir o nome original de "var_max" por 'MAX' em "data"
    names(data)[n2] <- "MAX"
    # Gerar nova "data" com dados diários para aplicação do teste
  data_d <-
    JC.scripts::Break.Dates(DATA = data, VAR = "date", ALL = T) %>%
    dplyr::select(c("site", "year", "month", "day", "MIN" , "MAX")) %>%
    dplyr::group_by(site, year, month, day) %>%
    dplyr::summarise(
      MIN = min(MIN, na.rm = TRUE),
      MAX = max(MAX, na.rm = TRUE) ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      date = as.Date.character(paste0(year, "/", month, "/", day))) %>%
    dplyr::select(site, date, everything())
  ## correção, pois foram gerados valores 'NaN', 'Inf' e '-Inf'
  nMIN <- which(is.infinite(data_d$MIN))
  nMAX <- which(is.infinite(data_d$MAX))
  data_d$MIN[nMIN] <- NA
  data_d$MAX[nMAX] <- NA
  test <-
    data_d %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(LAG = dplyr::lag(MIN)) %>%
    dplyr::select(site, date, MAX, LAG) %>% 
    dplyr::mutate(
      qc = "3e",
      suspect = ifelse(MAX < LAG, 1, 0)
      ) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'LAG'
  n3 <- which(names(test) == "LAG")
  # Substituir o nome 'LAG' pela união da "var_min" com '_LAG' em 'test' (processo reverso com acréscimo)
  names(test)[n3] <- paste0(var_min, "_LAG")
  # Localizar o número da coluna de 'test' correspondente à 'MAX'
  n4 <- which(names(test) == "MAX")
  # Substituir o nome 'MAX' pela "var_max" em 'test' (processo reverso)
  names(test)[n4] <- var_max
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n5 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n5] <- paste0(var_max, '_qc3e')
  return(test)
  }

qc_3f <- function(data, var_min, var_max) {
  # Localizar o número da coluna de "data" correspondente à "var_min"
  n1 <- which(names(data) == var_min)
  # Substituir o nome original de "var_min" por 'MIN' em "data"
  names(data)[n1] <- "MIN"
  # Localizar o número da coluna de "data" correspondente à "var_max"
  n2 <- which(names(data) == var_max)
  # Substituir o nome original de "var_max" por 'MAX' em "data"
    names(data)[n2] <- "MAX"
    # Gerar nova "data" com dados diários para aplicação do teste
  data_d <-
    JC.scripts::Break.Dates(DATA = data, VAR = "date", ALL = T) %>%
    dplyr::select(c("site", "year", "month", "day", "MIN" , "MAX")) %>%
    dplyr::group_by(site, year, month, day) %>%
    dplyr::summarise(
      MIN = min(MIN, na.rm = TRUE),
      MAX = max(MAX, na.rm = TRUE) ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      date = as.Date.character(paste0(year, "/", month, "/", day))) %>%
    dplyr::select(site, date, everything())
  ## correção, pois foram gerados valores 'NaN', 'Inf' e '-Inf'
  nMIN <- which(is.infinite(data_d$MIN))
  nMAX <- which(is.infinite(data_d$MAX))
  data_d$MIN[nMIN] <- NA
  data_d$MAX[nMAX] <- NA
  test <-
    data_d %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(LAG = dplyr::lag(MAX)) %>%
    dplyr::select(site, date, MIN, LAG) %>% 
    dplyr::mutate(
      qc = "3f",
      suspect = ifelse(MIN >= LAG, 1, 0)
      ) %>%
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'LAG'
  n3 <- which(names(test) == "LAG")
  # Substituir o nome 'LAG' pela união da "var_max" com '_LAG' em 'test' (processo reverso com acréscimo)
  names(test)[n3] <- paste0(var_max, "_LAG")
  # Localizar o número da coluna de 'test' correspondente à 'MIN'
  n4 <- which(names(test) == "MIN")
  # Substituir o nome 'MIN' pela "var_min" em 'test' (processo reverso)
  names(test)[n4] <- var_min
  # Localizar o número da coluna de 'test' correspondente à 'suspect'
  n5 <- which(names(test) == "suspect")
  # Substituir o nome 'suspect' pelo padrão que será usado: "variable"_qcID
  names(test)[n5] <- paste0(var_min, '_qc3f')
  return(test)
  }

################################################################################

## (Testes de Consistência Temporal)

qc_4a <- function(data, variable, tol1, tol2, tol3, tol6, tol12, houses) {
  # Localizar o número da coluna de "data" correspondente à "variable"
  n1 <- which(names(data) == variable)
  # Substituir o nome original de "variable" por 'VAR' em "data"
  names(data)[n1] <- "VAR"
  # Aplicar o teste
  test <-
    data %>%
    dplyr::select(c("site", "date", "VAR")) %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(
      VAR_lag1 = dplyr::lag(VAR, n = 1),
      VAR_lag2 = dplyr::lag(VAR, n = 2),
      VAR_lag3 = dplyr::lag(VAR, n = 3),
      VAR_lag6 = dplyr::lag(VAR, n = 6),
      VAR_lag12 = dplyr::lag(VAR, n = 12),
      qc = "4a",
      suspect1 = as.integer(round(abs(VAR - VAR_lag1), houses) > tol1),
      suspect2 = as.integer(round(abs(VAR - VAR_lag2), houses) > tol2),
      suspect3 = as.integer(round(abs(VAR - VAR_lag3), houses) > tol3),
      suspect6 = as.integer(round(abs(VAR - VAR_lag6), houses) > tol6),
      suspect12 = as.integer(round(abs(VAR - VAR_lag12), houses) > tol12) ) %>%  
    dplyr::ungroup()
  # Localizar o número da coluna de 'test' correspondente à 'VAR'
  n2 <- which(names(test) == "VAR")
  # Substituir o nome 'VAR' pela "variable" em 'test' (processo reverso)
  names(test)[n2] <- variable
  # Localizar o número da coluna de 'test' correspondente à 'VAR_lag1'
  n3 <- which(names(test) == "VAR_lag1")
  # Substituir o nome 'VAR_lag1' pela união de "variable" com '_LAG1' em 'test'
  names(test)[n3] <- paste0(variable, "_LAG1")
  # Localizar o número da coluna de 'test' correspondente à 'VAR_lag2'
  n4 <- which(names(test) == "VAR_lag2")
  # Substituir o nome 'VAR_lag2' pela união de "variable" com '_LAG2' em 'test'
  names(test)[n4] <- paste0(variable, "_LAG2")
  # Localizar o número da coluna de 'test' correspondente à 'VAR_lag3'
  n5 <- which(names(test) == "VAR_lag3")
  # Substituir o nome 'VAR_lag3' pela união de "variable" com '_LAG3' em 'test'
  names(test)[n5] <- paste0(variable, "_LAG3")
  # Localizar o número da coluna de 'test' correspondente à 'VAR_lag6'
  n6 <- which(names(test) == "VAR_lag6")
  # Substituir o nome 'VAR_lag6' pela união de "variable" com '_LAG6' em 'test'
  names(test)[n6] <- paste0(variable, "_LAG6")
  # Localizar o número da coluna de 'test' correspondente à 'VAR_lag12'
  n7 <- which(names(test) == "VAR_lag12")
  # Substituir o nome 'VAR_lag12' pela união de "variable" com '_LAG12' em 'test'
  names(test)[n7] <- paste0(variable, "_LAG12")
  # Localizar o número da coluna de 'test' correspondente à 'suspect1'
  n8 <- which(names(test) == "suspect1")
  # Substituir o nome 'suspect1' pelo padrão que será usado: "variable"_qcID
  names(test)[n8] <- paste0(variable, '_qc4a_1h')
  # Localizar o número da coluna de 'test' correspondente à 'suspect2'
  n9 <- which(names(test) == "suspect2")
  # Substituir o nome 'suspect1' pelo padrão que será usado: "variable"_qcID
  names(test)[n9] <- paste0(variable, '_qc4a_2h')
  # Localizar o número da coluna de 'test' correspondente à 'suspect3'
  n10 <- which(names(test) == "suspect3")
  # Substituir o nome 'suspect1' pelo padrão que será usado: "variable"_qcID
  names(test)[n10] <- paste0(variable, '_qc4a_3h')
  # Localizar o número da coluna de 'test' correspondente à 'suspect6'
  n11 <- which(names(test) == "suspect6")
  # Substituir o nome 'suspect1' pelo padrão que será usado: "variable"_qcID
  names(test)[n11] <- paste0(variable, '_qc4a_6h')
  # Localizar o número da coluna de 'test' correspondente à 'suspect12'
  n12 <- which(names(test) == "suspect12")
  # Substituir o nome 'suspect1' pelo padrão que será usado: "variable"_qcID
  names(test)[n12] <- paste0(variable, '_qc4a_12h')
  return(test)
  }


