
  ## Exports (1996)

I_C <- as.character(c(gdp_1995$cty_code))
d1_96 <- NULL
for (i_c in I_C[1:5]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d1_96 <- rbind(d1_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d2_96 <- NULL
for (i_c in I_C[6:10]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d2_96 <- rbind(d2_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d3_96 <- NULL
for (i_c in I_C[11:15]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d3_96 <- rbind(d3_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d4_96 <- NULL
for (i_c in I_C[16:20]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d4_96<- rbind(d4_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

  ## Imports (1996)

Sys.sleep(3660)


I_C <- as.character(c(gdp_1995$cty_code))
d5_96 <- NULL
for (i_c in I_C[1:5]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d5_96 <- rbind(d5_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d6_96 <- NULL
for (i_c in I_C[6:10]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d6_96 <- rbind(d6_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d7_96 <- NULL
for (i_c in I_C[11:15]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d7_96 <- rbind(d7_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d8_96 <- NULL
for (i_c in I_C[16:20]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d8_96 <- rbind(d8_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

data_countries_1996 <- rbind(d1_96, d2_96, d3_96, d4_96, d5_96, d6_96, d7_96, d8_96)  
data_countries_1996 <- data.table(data_countries_1996)


keycols <- c("cty_code_o","cty_code_d")
setnames(data_countries_1996, c("rtCode","ptCode"), keycols)

  ## Convert column classes to numeric. 

data_countries_1996$cty_code_o <- as.numeric(as.character(data_countries_1996$cty_code_o))
data_countries_1996$cty_code_d <- as.numeric(as.character(data_countries_1996$cty_code_d))

gdp_1995 <- data.table(gdp_1995)
data_countries_1996 <- data.table(data_countries_1996)

  ## Match for gdp_1995_i

data_countries_1996$gdp_1995_i <- NA
I <- as.numeric(c(gdp_1995$cty_code_o))
K <- as.numeric(c(data_countries_1996$cty_code_o))

for (i in seq_along(I)) {
  for (j in seq_along(K)) {
    if (data_countries_1996$cty_code_o[j] == gdp_1995$cty_code_o[i]) {
      data_countries_1996$gdp_1995_i[j] <- gdp_1995$gdp_1995[i]
    }
    else if (data_countries_1996$cty_code_o[j] != gdp_1995$cty_code_o[i]) {
      next
    }
  }
}

  ## Match for gdp_1995_j

data_countries_1996$gdp_1995_j <- NA
I <- as.numeric(c(gdp_1995$cty_code_o))
K <- as.numeric(c(data_countries_1996$cty_code_o))

for (i in seq_along(I)) {
  for (j in seq_along(K)) {
    if (data_countries_1996$cty_code_d[j] == gdp_1995$cty_code_o[i]) {
      data_countries_1996$gdp_1995_j[j] <- gdp_1995$gdp_1995[i]
    }
    else if (data_countries_1996$cty_code_d[j] != gdp_1995$cty_code_o[i]) {
      next
    }
  }
}






