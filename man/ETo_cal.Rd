% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ETo_cal.R
\name{ETo_cal}
\alias{ETo_cal}
\title{Compute Reference evapotranspiration (ETo)}
\usage{
ETo_cal(wth_data, lat, elev, ref_ht = 2, kRs = 0.175, ws_mean = 2)
}
\arguments{
\item{wth_data}{A data frame containing weather data with columns: date, tmax, tmin, rain, rhum, srad, wspd (optional)}

\item{lat}{Numeric. Latitude (decimal degrees)}

\item{elev}{Numeric. Elevation (meters above sea level)}

\item{ref_ht}{Numeric. Sensors reference height.}

\item{kRs}{Numeric. Hargreaves empirical coefficient, Coastal coefficient}

\item{ws_mean}{Numeric. Wind Speed Mean, default = 2 m/s}
}
\value{
This function returns a numeric vector representing ET0 values (mm).
}
\description{
Function to compute Reference evapotranspiration (ETo) estimates using different methods.
}
\examples{
# Compute ETo with provided weather data
ETo_cal(wth_data = weather, lat = 3.8, elev = 650)

}
