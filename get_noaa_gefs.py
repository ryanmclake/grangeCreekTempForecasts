

import openmeteo_requests

from openmeteo_sdk.Variable import Variable
from openmeteo_sdk.Aggregation import Aggregation

import pandas as pd
import requests_cache
from retry_requests import retry

# Setup the Open-Meteo API client with cache and retry on error
cache_session = requests_cache.CachedSession('.cache', expire_after = 3600)
retry_session = retry(cache_session, retries = 5, backoff_factor = 0.2)
openmeteo = openmeteo_requests.Client(session = retry_session)

# Make sure all required weather variables are listed here
# The order of variables in hourly or daily is important to assign them correctly below
url = "https://ensemble-api.open-meteo.com/v1/ensemble"
params = {
	"latitude": 39.8916,
	"longitude": -104.9371,
	"daily": "shortwave_radiation_sum",
	"hourly": ["temperature_2m", "relative_humidity_2m", "cloud_cover", "wind_speed_10m"],
	"models": "ncep_gefs025",
	"timezone": "GMT",
}
responses = openmeteo.weather_api(url, params=params)

# Process first location. Add a for-loop for multiple locations or weather models
response = responses[0]
print(f"Coordinates: {response.Latitude()}°N {response.Longitude()}°E")
print(f"Elevation: {response.Elevation()} m asl")
print(f"Timezone: {response.Timezone()}{response.TimezoneAbbreviation()}")
print(f"Timezone difference to GMT+0: {response.UtcOffsetSeconds()}s")

# Process hourly data. The order of variables needs to be the same as requested.
hourly = response.Hourly()
hourly_variables = list(map(lambda i: hourly.Variables(i), range(0, hourly.VariablesLength())))
hourly_temperature_2m = filter(lambda x: x.Variable() == Variable.temperature and x.Altitude() == 2, hourly_variables)
hourly_relative_humidity_2m = filter(lambda x: x.Variable() == Variable.relative_humidity and x.Altitude() == 2, hourly_variables)
hourly_cloud_cover = filter(lambda x: x.Variable() == Variable.cloud_cover, hourly_variables)
hourly_wind_speed_10m = filter(lambda x: x.Variable() == Variable.wind_speed and x.Altitude() == 10, hourly_variables)

hourly_data = {"date": pd.date_range(
	start = pd.to_datetime(hourly.Time(), unit = "s", utc = True),
	end =  pd.to_datetime(hourly.TimeEnd(), unit = "s", utc = True),
	freq = pd.Timedelta(seconds = hourly.Interval()),
	inclusive = "left"
)}

# Process all hourly members
for variable in hourly_temperature_2m:
	member = variable.EnsembleMember()
	hourly_data[f"temperature_2m_member{member}"] = variable.ValuesAsNumpy()
for variable in hourly_relative_humidity_2m:
	member = variable.EnsembleMember()
	hourly_data[f"relative_humidity_2m_member{member}"] = variable.ValuesAsNumpy()
for variable in hourly_cloud_cover:
	member = variable.EnsembleMember()
	hourly_data[f"cloud_cover_member{member}"] = variable.ValuesAsNumpy()
for variable in hourly_wind_speed_10m:
	member = variable.EnsembleMember()
	hourly_data[f"wind_speed_10m_member{member}"] = variable.ValuesAsNumpy()

hourly_dataframe = pd.DataFrame(data = hourly_data)
print("\nHourly data\n", hourly_dataframe)

# Process daily data. The order of variables needs to be the same as requested.
daily = response.Daily()
daily_variables = list(map(lambda i: daily.Variables(i), range(0, daily.VariablesLength())))
daily_shortwave_radiation_sum = filter(lambda x: x.Variable() == Variable.temperature and x.Altitude() == 2, daily_variables)	

daily_data = {"date": pd.date_range(
	start = pd.to_datetime(daily.Time(), unit = "s", utc = True),
	end =  pd.to_datetime(daily.TimeEnd(), unit = "s", utc = True),
	freq = pd.Timedelta(seconds = daily.Interval()),
	inclusive = "left"
)}

# Process all daily members
for variable in daily_shortwave_radiation_sum:
	member = variable.EnsembleMember()
	daily_data[f"shortwave_radiation_sum_member{member}"] = variable.ValuesAsNumpy()

daily_dataframe = pd.DataFrame(data = daily_data)
print("\nDaily data\n", daily_dataframe)

df = pd.DataFrame(hourly_dataframe)
df.to_csv("noaa_gefs_forecast.csv", index=False)