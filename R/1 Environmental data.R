# Open libraries

# Create a new project for easier management of files

# --------------------------------------------------------------------
# If the "pacman" package is not among your available packages,
# download it with the next line:
# instal.packages('pacman')
# --------------------------------------------------------------------

pacman:: p_load('tidyverse', # Data wrangling
                'ncdf4', # Read NetCDF files
                'raster', # Extracting NetCDF data
                'gridExtra') # Multiple plots

# Clean console
rm(list = ls())
shell('cls')


# NetCDF files ####
# Create the object of the files to be used
ncfiles<- c('Data/Blue Ocean 1993-2022.nc', 'Data/Salinity 2023-2024.nc',
            'Data/Temperature 2023-2024.nc', 'Data/Green Ocean 1993-2022.nc',
            'Data/Chlorophyll 2023-2024.nc', 'Data/Nutrients 2023-2024.nc',
            'Data/Oxygen 2023-2024.nc', 'Data/pH 2023-2024.nc')


# Blue Ocean (physical properties) ####
# ---------------------------------------------------- Temperature ####
# Open NetCDF file
cmes.data <- nc_open(ncfiles[1])

# Visualize temperature information
print(cmes.data)
names(cmes.data)
names(cmes.data$var)


# Multiple band: Temperature (°C) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[1], varname = 'thetao_mean')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(brick(ncfiles[1], varname = 'thetao_std'), fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Temperature")
plot(polygon.sd, main = "Standard deviation Temperature")


# Layer analysis ####
fechas <- substr(names(multi.band), 2, 11) %>%
  parse_date_time("Ymd")
range(fechas)


# Extracting values from numeric model ####
sites.coords<- read.csv('Data/RBIM_fish.csv', header = T,
                        stringsAsFactors = T) %>%
  dplyr:: select(Isla, Sitio, Latitud, Longitud)

sum(is.na(sites.coords))

# Removing NAs due to missing coordinates
sites.coords<- sites.coords %>% distinct() %>% na.omit()

# Extracting unique coords
coords<- sites.coords[, 3:4]
coordinates(coords)<- ~Longitud + Latitud
crs(coords)<- crs(multi.band)

# Extracting values
mean.temp<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.temp<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.temp) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.temp))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# -------------------------------------- Temperaure data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[3])

# Visualize temperature information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Temperature (°C) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[3], varname = 'thetao')
multi.band

# Layer analysis ####
fechas2 <- substr(names(multi.band), 2, 11) %>%
  parse_date_time("Ymd")
range(fechas2)

# Merging dates and removing second dataset
fechas <- c(fechas, fechas2)
range(fechas)
rm(fechas2)

# Extracting values
mean.temp2<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.temp2<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.temp2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.temp2) %>% print(n = 10)

# Fill NaNs with the values at Cleofas3 due to similar values in the
# previous database (mean.temp)
mean.temp2[c(8, 10:11, 15:18), 3:18]<- mean.temp2[9, 3:18]

# Merge databases and remove the second
mean.temp <- cbind(mean.temp, mean.temp2[, -c(1:2)])
rm(mean.temp2)


# ------------------------------------------------------ Salinity ####
# Open NetCDF file
cmes.data <- nc_open(ncfiles[1])

# Visualize salinity information (PSU)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[1], varname = 'so_mean')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(brick(ncfiles[1], varname = 'so_std'), fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Salinity")
plot(polygon.sd, main = "Standard deviation Salinity")

# Extracting values
mean.sal <-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.sal <- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.sal) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.sal))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# Salinity data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[2])

# Visualize salinity information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Salinity (PSU) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[2], varname = 'so')
multi.band

# Extracting values
mean.sal2<-  raster:: extract(multi.band, coords,
                               fun = mean, na.rm = F)
mean.sal2<- data.frame(Isla = sites.coords[, 1],
                        Sitio = sites.coords[, 2],
                        mean.sal2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.sal2) %>% print(n = 10)

# Fill NaNs with the values at Cleofas3 due to similar values in the
# previous database (mean.sal)
mean.sal2[c(8, 10:11, 15:18), 3:18]<- mean.sal2[9, 3:18]

# Merge databases and remove the second
mean.sal <- cbind(mean.sal, mean.sal2[, -c(1:2)])
rm(mean.sal2)


# Green Ocean (biogeochemical properties) ####
# -------------------------------------------------- Chlorophyll ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize chlorophyll information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'chl')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Chlorophyll")
plot(polygon.sd, main = "Standard deviation Chlorophyll")

# Extracting values from numeric model ####
# Extracting values
mean.chl<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.chl<- data.frame(Comunidad = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.chl) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.chl))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# ----------------------------------- Chlorophyll data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[5])

# Visualize chlorophyll information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Chlorophyll (mg/m3) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[5], varname = 'chl')
multi.band

# Extracting values
mean.chl2<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.chl2<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.chl2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.chl2) %>% print(n = 10)
sum(is.na(mean.chl2))

# Merge databases and remove the second
mean.chl <- cbind(mean.chl, mean.chl2[, -c(1:2)])
rm(mean.chl2)


# -------------------------------------- Nutrients: Dissolved Iron ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize dissolved iron information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'fe')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Dissolved Iron")
plot(polygon.sd, main = "Standard deviation Dissolved Iron")

# Extracting values from numeric model ####
# Extracting values
mean.fe<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.fe<- data.frame(Comunidad = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.fe) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.fe))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# ----------------------- Nutrients: Dissolved Iron data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[6])

# Visualize dissolved iron information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Dissolved Iron (mmol/m3) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[6], varname = 'fe')
multi.band

# Extracting values
mean.fe2<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.fe2<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.fe2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.fe2) %>% print(n = 10)
sum(is.na(mean.fe2))

# Merge databases and remove the second
mean.fe <- cbind(mean.fe, mean.fe2[, -c(1:2)])
rm(mean.fe2)


# -------------------------------------- Nutrients: Nitrate ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize nitrate information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'no3')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Nitrate")
plot(polygon.sd, main = "Standard deviation Nitrate")

# Extracting values from numeric model ####
# Extracting values
mean.nit<-  raster:: extract(multi.band, coords,
                            fun = mean, na.rm = F)
mean.nit<- data.frame(Comunidad = sites.coords[, 1],
                     Sitio = sites.coords[, 2],
                     mean.nit) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.nit))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# ------------------------------ Nutrients: Nitrate data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[6])

# Visualize nitrate information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Nitrate (mmol/m3) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[6], varname = 'no3')
multi.band

# Extracting values
mean.nit2<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.nit2<- data.frame(Isla = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.nit2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.nit2) %>% print(n = 10)
sum(is.na(mean.nit2))

# Merge databases and remove the second
mean.nit <- cbind(mean.nit, mean.nit2[, -c(1:2)])
rm(mean.nit2)


# -------------------------------------- Nutrients: Phosphate ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize phosphate information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'po4')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Phosphate")
plot(polygon.sd, main = "Standard deviation Phosphate")

# Extracting values from numeric model ####
# Extracting values
mean.pho<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.pho<- data.frame(Comunidad = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.pho) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.pho))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# ---------------------------- Nutrients: Phosphate data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[6])

# Visualize phosphate information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Phosphate (mmol/m3) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[6], varname = 'po4')
multi.band

# Extracting values
mean.pho2<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.pho2<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.pho2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.pho2) %>% print(n = 10)
sum(is.na(mean.pho2))

# Merge databases and remove the second
mean.pho <- cbind(mean.pho, mean.pho2[, -c(1:2)])
rm(mean.pho2)


# -------------------------------------- Nutrients: Silicate ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize silicate information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'si')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Silicate")
plot(polygon.sd, main = "Standard deviation Silicate")

# Extracting values from numeric model ####
# Extracting values
mean.si<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.si<- data.frame(Comunidad = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.si) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.si))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# ---------------------------- Nutrients: Silicate data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[6])

# Visualize silicate information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Silicate (mmol/m3) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[6], varname = 'si')
multi.band

# Extracting values
mean.si2<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.si2<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.si2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.si2) %>% print(n = 10)
sum(is.na(mean.si2))

# Merge databases and remove the second
mean.si <- cbind(mean.si, mean.si2[, -c(1:2)])
rm(mean.si2)


# --------------------------------------------- Dissolved Oxygen ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize dissolved oxygen information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'o2')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Dissolved Oxygen")
plot(polygon.sd, main = "Standard deviation Dissolved Oxygen")

# Extracting values from numeric model ####
# Extracting values
mean.oxy<-  raster:: extract(multi.band, coords,
                            fun = mean, na.rm = F)
mean.oxy<- data.frame(Comunidad = sites.coords[, 1],
                     Sitio = sites.coords[, 2],
                     mean.oxy) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.oxy))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# --------------------------------- Dissolved Oxygen data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[7])

# Visualize dissolved oxygen information
print(cmes.data)
names(cmes.data$var)


# Multiple band: Dissolved Oxygen (mmol/m3) ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[7], varname = 'o2')
multi.band

# Extracting values
mean.oxy2<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.oxy2<- data.frame(Isla = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.oxy2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.oxy2) %>% print(n = 10)
sum(is.na(mean.oxy2))

# Merge databases and remove the second
mean.oxy <- cbind(mean.oxy, mean.oxy2[, -c(1:2)])
rm(mean.oxy2)


# ------------------------------------------------------------ pH ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[4])

# Visualize pH information
print(cmes.data)
names(cmes.data$var)

# Multiple band ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[4], varname = 'ph')
multi.band


# Raster file analysis ####
# Summary: mean and standard deviation
polygon.mean <- calc(multi.band, fun = mean)
polygon.mean

polygon.sd <- calc(multi.band, fun = sd)
polygon.sd

# Multiple band plots
plot(polygon.mean, main = "Average Dissolved pH")
plot(polygon.sd, main = "Standard deviation pH")

# Extracting values from numeric model ####
# Extracting values
mean.ph<-  raster:: extract(multi.band, coords,
                             fun = mean, na.rm = F)
mean.ph<- data.frame(Comunidad = sites.coords[, 1],
                      Sitio = sites.coords[, 2],
                      mean.ph) %>%
  distinct(Sitio, .keep_all = T)

# Reviewing missing values
sum(is.na(mean.ph))

# -----------------------------------------------------------------
# So far we have data from 1993-2022. Therefore we will continue
# extracting the data for the next two years.
# -----------------------------------------------------------------

# --------------------------------------------- pH data 2023-2024 ####
# Open NetCDF file
as_tibble(ncfiles)
cmes.data <- nc_open(ncfiles[8])

# Visualize pH information
print(cmes.data)
names(cmes.data$var)


# Multiple band: pH ####
# Create a raster object based on the NetCDF file
multi.band <- brick(ncfiles[8], varname = 'ph')
multi.band

# Extracting values
mean.ph2<-  raster:: extract(multi.band, coords,
                              fun = mean, na.rm = F)
mean.ph2<- data.frame(Isla = sites.coords[, 1],
                       Sitio = sites.coords[, 2],
                       mean.ph2) %>%
  distinct(Sitio, .keep_all = T)

as_tibble(mean.ph2) %>% print(n = 10)
sum(is.na(mean.ph2))

# Merge databases and remove the second
mean.ph <- cbind(mean.ph, mean.ph2[, -c(1:2)])
rm(mean.ph2)


# Merge all datasets ####
environ.data<- data.frame(Date = fechas,
                          Island = rep(t(mean.temp[, 1]),
                                          each =  length(fechas)),
                          Site = rep(t(mean.temp[, 2]),
                                     each =  length(fechas)),
                          Temperature = c(t(mean.temp[, 3:378])),
                          Salinity = c(t(mean.sal[, 3:378])),
                          Chlorophyll = c(t(mean.chl[, 3:378])),
                          Iron = c(t(mean.fe[, 3:378])),
                          Nitrate = c(t(mean.nit[, 3:378])),
                          Phosphate = c(t(mean.pho[, 3:378])),
                          Silicate = c(t(mean.si[, 3:378])),
                          Oxygen = c(t(mean.oxy[, 3:378])),
                          pH = c(t(mean.ph[, 3:378])))

write.csv(environ.data, 'Data/Environmental_data.csv',
          row.names = F)

# Unload raster package to avoid confusion with tidyverse functions
detach("package:raster", unload = TRUE)

# Time series graphs ####
sub.environ.data <- environ.data %>% group_by(Date, Island) %>%
  reframe(Temperature = mean(Temperature),
          Salinity = mean(Salinity),
          Chlorophyll = mean(Chlorophyll),
          Iron = mean(Iron),
          Nitrate = mean(Nitrate),
          Phosphate = mean(Phosphate),
          Silicate = mean(Silicate),
          Oxygen = mean(Oxygen),
          pH = mean(pH)) %>% as.data.frame()

# --------------------------------------------------- Temperature ####
(temp.graph <- ggplot(sub.environ.data, aes(x = Date)) +
  geom_line(aes(y = Temperature, color = Island), linewidth = 1,
            alpha = 0.7, show.legend = F)+
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
  labs(x = NULL,
       y = 'Temperature (°C)')+
  scale_y_continuous(limits = c(21, 32),
                     breaks = seq(21, 32, by = 2.5))+
  theme_classic()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20)))


# --------------------------------------------------- Salinity ####
(sal.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Salinity, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = 'Salinity (UPS)')+
   scale_y_continuous(limits = c(31.5, 35.5),
                      breaks = seq(31.5, 35.5, by = 1))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------- Chlorophyll ####
(chl.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Chlorophyll, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Chlorophyll~(mg~m^{'-3'})))+
   scale_y_continuous(limits = c(0, 0.6),
                      breaks = seq(0, 0.6, by = 0.1))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------- Dissolved Iron ####
(fe.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Iron, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Dissolved~Iron~(mmol~m^{'-3'})))+
   scale_y_continuous(limits = c(0, 0.004),
                      breaks = seq(0, 0.004, by = 0.001))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------- Nitrate ####
(nit.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Nitrate, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Nitrate~(mmol~m^{'-3'})))+
   scale_y_continuous(limits = c(0, 0.3),
                      breaks = seq(0, 0.3, by = 0.05))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------ Phosphate ####
(pho.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Phosphate, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Phosphate~(mmol~m^{'-3'})))+
   scale_y_continuous(limits = c(0, 1.5),
                      breaks = seq(0, 1.5, by = 0.25))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------ Silicate ####
(si.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Silicate, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Silicate~(mmol~m^{'-3'})))+
   scale_y_continuous(limits = c(0.5, 7),
                      breaks = seq(0.5, 7, by = 1.55))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ----------------------------------------------- Dissolved Oxygen ####
(oxy.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = Oxygen, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = "Year",
        y = expression(Dissolved~Oxygen~(mmol~m^{'-3'})))+
   scale_y_continuous(limits = c(195, 230),
                      breaks = seq(195, 230, by = 5))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------------- pH ####
(ph.graph <- ggplot(sub.environ.data, aes(x = Date)) +
   geom_line(aes(y = pH, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = "Year",
        y = 'pH')+
   scale_y_continuous(limits = c(7.85, 8.1),
                      breaks = seq(7.85, 8.1, by = 0.05))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))

# Legend ####
legend <- ggplot(sub.environ.data, aes(x = Date)) +
  geom_line(aes(y = Temperature, color = Island), linewidth = 1,
            alpha = 0.7)+
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25))

legend <- cowplot:: get_legend(legend)

all.plots <- grid.arrange(temp.graph, sal.graph, chl.graph, fe.graph,
                          nit.graph, pho.graph, si.graph, oxy.graph,
                          ph.graph, legend, nrow = 5)

ggsave("Figures and Tables/Environmental_variables.tiff", all.plots,
       width = 7000, height = 6000, units = 'px', dpi = 320,
       bg= "white", compression = "lzw")


# Time series anomalies graphs ####
sub.environ.anom <- sub.environ.data %>% group_by(Island) %>%
  reframe(Date = fechas,
          Temperature = scale(Temperature),
          Salinity = scale(Salinity),
          Chlorophyll = scale(Chlorophyll),
          Iron = scale(Iron),
          Nitrate = scale(Nitrate),
          Phosphate = scale(Phosphate),
          Silicate = scale(Silicate),
          Oxygen = scale(Oxygen),
          pH = scale(pH)) %>% as.data.frame()

# --------------------------------------------------- Temperature ####
(temp.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Temperature, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = 'Standarized SST')+
   scale_y_continuous(limits = c(-2.5, 2),
                      breaks = seq(-2.5, 2, by = 0.75))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------- Salinity ####
(sal.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Salinity, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = 'Standarized Salinity')+
   scale_y_continuous(limits = c(-5.5, 2.5),
                      breaks = seq(-5.5, 2.5, by = 1))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------- Chlorophyll ####
(chl.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Chlorophyll, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Standarized~Chlorophyll))+
   scale_y_continuous(limits = c(-1.5, 9),
                      breaks = seq(-1.5, 9, by = 1.75))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------- Dissolved Iron ####
(fe.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Iron, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Standarized~Dissolved~Iron))+
   scale_y_continuous(limits = c(-2.5, 4),
                      breaks = seq(-2.5, 4, by = 1.25))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------- Nitrate ####
(nit.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Nitrate, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Standarized~Nitrate))+
   scale_y_continuous(limits = c(-1, 16.5),
                      breaks = seq(-1, 16.5, by = 2.5))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------ Phosphate ####
(pho.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Phosphate, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Standarized~Phosphate))+
   scale_y_continuous(limits = c(-2, 4.5),
                      breaks = seq(-2, 4.5, by = 1))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------ Silicate ####
(si.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Silicate, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Standarized~Silicate))+
   scale_y_continuous(limits = c(-3.5, 5.5),
                      breaks = seq(-3.5, 5.5, by = 3))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ----------------------------------------------- Dissolved Oxygen ####
(oxy.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = Oxygen, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = "Year",
        y = expression(Standarized~Dissolved~Oxygen))+
   scale_y_continuous(limits = c(-1.5, 3.5),
                      breaks = seq(-1.5, 3.5, by = 1))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------------- pH ####
(ph.graph <- ggplot(sub.environ.anom, aes(x = Date)) +
   geom_line(aes(y = pH, color = Island), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = "Year",
        y = 'Standarized pH')+
   scale_y_continuous(limits = c(-4, 2.5),
                      breaks = seq(-4, 2.5, by = 1))+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))

# Legend ####
legend <- ggplot(sub.environ.anom, aes(x = Date)) +
  geom_line(aes(y = Temperature, color = Island), linewidth = 1,
            alpha = 0.7)+
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25))

legend <- cowplot:: get_legend(legend)

all.plots <- grid.arrange(temp.graph, sal.graph, chl.graph, fe.graph,
                          nit.graph, pho.graph, si.graph, oxy.graph,
                          ph.graph, legend, nrow = 5)

ggsave("Figures and Tables/Environmental_anomalies.tiff", all.plots,
       width = 7000, height = 6000, units = 'px', dpi = 320,
       bg= "white", compression = "lzw")


# Time series trends at RBIM ####
# -------------------------------------------------- Temperature ####
RBIM.trends <- environ.data %>% group_by(Date) %>%
  reframe(Temperature = mean(Temperature), Salinity = mean(Salinity),
          Chlorophyll = mean(Chlorophyll), Iron = mean(Iron),
          Nitrate = mean(Nitrate), Phosphate = mean(Phosphate),
          Silicate = mean(Silicate), Oxygen = mean(Oxygen),
          pH = mean(pH)) %>% as.data.frame()

plot(sst.trend <- RBIM.trends %>% select(Temperature) %>%
  ts(., start = 1993, frequency = 12) %>% decompose())
sst.trend <- scale(sst.trend$trend)
sst.trend <- as.data.frame(sst.trend)


# -------------------------------------------------- Salinity ####
plot(sal.trend <- RBIM.trends %>% dplyr:: select(Salinity) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
sal.trend <- scale(sal.trend$trend)
sal.trend <- as.data.frame(sal.trend)


# -------------------------------------------------- Chlorophyll ####
plot(chl.trend <- RBIM.trends %>% dplyr:: select(Chlorophyll) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
chl.trend <- scale(chl.trend$trend)
chl.trend <- as.data.frame(chl.trend)


# ---------------------------------------------- Dissolved Iron ####
plot(fe.trend <- RBIM.trends %>% dplyr:: select(Iron) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
fe.trend <- scale(fe.trend$trend)
fe.trend <- as.data.frame(fe.trend)


# ----------------------------------------------------- Nitrate ####
plot(nit.trend <- RBIM.trends %>% dplyr:: select(Nitrate) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
nit.trend <- scale(nit.trend$trend)
nit.trend <- as.data.frame(nit.trend)


# ----------------------------------------------------- Phosphate ####
plot(pho.trend <- RBIM.trends %>% dplyr:: select(Phosphate) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
pho.trend <- scale(pho.trend$trend)
pho.trend <- as.data.frame(pho.trend)


# ----------------------------------------------------- Silicate ####
plot(si.trend <- RBIM.trends %>% dplyr:: select(Silicate) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
si.trend <- scale(si.trend$trend)
si.trend <- as.data.frame(si.trend)


# --------------------------------------------- Dissolved Oxygen ####
plot(oxy.trend <- RBIM.trends %>% dplyr:: select(Oxygen) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
oxy.trend <- scale(oxy.trend$trend)
oxy.trend <- as.data.frame(oxy.trend)


# ---------------------------------------------------------- pH ####
plot(ph.trend <- RBIM.trends %>% dplyr:: select(pH) %>%
       ts(., start = 1993, frequency = 12) %>% decompose(type = 'multi'))
ph.trend <- scale(ph.trend$trend)
ph.trend <- as.data.frame(ph.trend)


# Merge all datasets ####
trend.data <- data.frame(Date = RBIM.trends$Date,
                         sst.trend, sal.trend, chl.trend, fe.trend,
                         nit.trend, pho.trend, si.trend, oxy.trend,
                         ph.trend)

trend.data <- trend.data %>% na.omit() %>%
  rename(Date = Date, Temperature = V1, Salinity = V1.1,
         Chlorophyll = V1.2, Iron = V1.3, Nitrate = V1.4,
         Phosphate = V1.5, Silicate = V1.6, Oxygen = V1.7, pH = V1.8)


# Trend graphs ####
# --------------------------------------------------- Temperature ####
(temp.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Temperature), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = 'SST Trend')+
   scale_y_continuous(limits = c(-2.5, 2.5),
                      breaks = seq(-2.5, 2.5, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------- Salinity ####
(sal.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Salinity), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = 'Salinity Trend')+
   scale_y_continuous(limits = c(-3, 2.5),
                      breaks = seq(-3, 2.5, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------- Chlorophyll ####
(chl.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Chlorophyll), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Chlorophyll~Trend))+
   scale_y_continuous(limits = c(-1, 5.5),
                      breaks = seq(-1, 5.5, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------- Dissolved Iron ####
(fe.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Iron), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Dissolved~Iron~Trend))+
   scale_y_continuous(limits = c(-3, 2.5),
                      breaks = seq(-3, 2.5, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------- Nitrate ####
(nit.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Nitrate), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Nitrate~Trend))+
   scale_y_continuous(limits = c(-1, 5.5),
                      breaks = seq(-1, 5.5, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------ Phosphate ####
(pho.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Phosphate), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Phosphate~Trend))+
   scale_y_continuous(limits = c(-2, 3),
                      breaks = seq(-2, 3, by = 1))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ------------------------------------------------------ Silicate ####
(si.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Silicate), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = NULL,
        y = expression(Silicate~Trend))+
   scale_y_continuous(limits = c(-3.5, 4),
                      breaks = seq(-3.5, 4, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# ----------------------------------------------- Dissolved Oxygen ####
(oxy.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = Oxygen), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = "Year",
        y = expression(Dissolved~Oxygen~Trend))+
   scale_y_continuous(limits = c(-2, 3),
                      breaks = seq(-2, 3, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))


# --------------------------------------------------------- pH ####
(ph.graph <- ggplot(trend.data, aes(x = Date)) +
   geom_line(aes(y = pH), linewidth = 1,
             alpha = 0.7, show.legend = F)+
   scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")+
   labs(x = "Year",
        y = 'pH Trend')+
   scale_y_continuous(limits = c(-3, 2),
                      breaks = seq(-3, 2, by = 0.5))+
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(panel.grid = element_blank(),
         text = element_text(size = 20)))

all.plots <- grid.arrange(temp.graph, sal.graph, chl.graph, fe.graph,
                          nit.graph, pho.graph, si.graph, oxy.graph,
                          ph.graph, nrow = 3)

ggsave("Figures and Tables/Environmental_trends.tiff", all.plots,
       width = 10000, height = 6000, units = 'px', dpi = 320,
       bg= "white", compression = "lzw")
