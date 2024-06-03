# Open libraries

# Create a new project for easier management of files

# --------------------------------------------------------------------
# If the "pacman" package is not among your available packages,
# download it with the next line:
# instal.packages('pacman')
# --------------------------------------------------------------------

pacman:: p_load('tidyverse', # Data wrangling
                'ncdf4', # Read NetCDF files
                'raster') # Extracting NetCDF data

# Clean console
rm(list = ls())
shell('cls')


# NetCDF files ####
# Create the object of the files to be used
ncfiles<- c('Data/Blue Ocean 1993-2022.nc', 'Data/Salinity 2023-2024.nc',
            'Data/Temperature 2023-2024.nc', 'Data/Green Ocean 1993-2022.nc',
            'Data/Chlorophyll.nc', 'Data/Nutrients.nc', 'Data/Oxygen.nc', 'Data/pH.nc')


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

# Visualize temperature information
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
