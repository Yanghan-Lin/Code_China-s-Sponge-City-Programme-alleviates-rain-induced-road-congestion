********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    05_main_figure5_projection_processing.do
* Purpose: Prepare historical and projected threshold-exceeding rainfall days
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off

********************************************************************************
* 0. User paths
********************************************************************************

* Run 00_data_preparation.do and the Python Fig. 5 script before this script.
global PROJECT_ROOT "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
global SUBMISSION    "${PROJECT_ROOT}/submission_code"
global PROCESSED     "${SUBMISSION}/data/processed"
global INTERMEDIATE  "${SUBMISSION}/data/intermediate"
global OUTPUT        "${SUBMISSION}/outputs"
global LOGS          "${SUBMISSION}/logs"

capture mkdir "${INTERMEDIATE}"
capture mkdir "${OUTPUT}"
capture mkdir "${LOGS}"

capture log close _all
log using "${LOGS}/05_main_figure5_projection_processing.log", replace text

********************************************************************************
* 1. Pilot and local-exposure city lists
********************************************************************************

use "${PROCESSED}/yearly_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    gen byte analysis_sample = 1
}

collapse (max) sponge_city = treat local_exposure = analysis_sample, by(city city_code)
replace local_exposure = 1 - local_exposure
keep city city_code sponge_city local_exposure
tempfile city_status_by_code city_status_by_name
save `city_status_by_code', replace

preserve
    keep city sponge_city local_exposure
    collapse (max) sponge_city local_exposure, by(city)
    save `city_status_by_name', replace
restore

********************************************************************************
* 2. Historical threshold-exceeding rainfall days, 2015-2024
********************************************************************************

use "${PROCESSED}/daily_panel_prepared.dta", clear
keep if inrange(year, 2015, 2024)

gen double valid_precip_day = !missing(precp)
foreach threshold in 20 30 50 80 {
    gen byte exceed_`threshold' = (precp > `threshold') if valid_precip_day == 1
}

preserve
    collapse (sum) annual_precip_mm = precp, by(city_code city year)
    collapse (mean) avg_prcp = annual_precip_mm, by(city_code city)
    tempfile historical_climate
    save `historical_climate', replace
restore

collapse (sum) exceed_20 exceed_30 exceed_50 exceed_80 ///
    valid_days = valid_precip_day, by(city_code city year)

merge m:1 city_code city using `historical_climate', keep(master match) nogen
merge m:1 city_code city using `city_status_by_code', keep(master match) nogen

replace sponge_city = 0 if missing(sponge_city)
replace local_exposure = 0 if missing(local_exposure)
gen str20 city_group = cond(sponge_city == 1, "sponge_cities", "others")

gen str20 climate_zone = ""
replace climate_zone = "semi_humid"  if avg_prcp >= 400 & avg_prcp < 800
replace climate_zone = "humid"       if avg_prcp >= 800 & avg_prcp <= 1500
replace climate_zone = "hyper_humid" if avg_prcp > 1500

reshape long exceed_, i(city_code city year city_group sponge_city ///
    local_exposure avg_prcp climate_zone valid_days) j(threshold)
rename exceed_ exceed_days

keep if !missing(exceed_days)
order city_code city year city_group sponge_city local_exposure avg_prcp ///
    climate_zone threshold exceed_days valid_days
sort city_code year threshold

save "${INTERMEDIATE}/fig5_historical_city_threshold_days.dta", replace
export delimited using "${OUTPUT}/fig5_historical_city_threshold_days.csv", ///
    replace

preserve
    keep if (climate_zone == "semi_humid"  & threshold == 20) | ///
            (climate_zone == "humid"       & threshold == 50) | ///
            (climate_zone == "hyper_humid" & threshold == 80)

    collapse (mean) mean_days = exceed_days ///
             (sd)   sd_days   = exceed_days ///
             (count) n_cities = exceed_days, ///
             by(year city_group climate_zone threshold)

    gen str12 scenario = "historical"
    gen str12 period = "Historical"
    gen ci_lower = mean_days - 1.96 * sd_days / sqrt(n_cities)
    gen ci_upper = mean_days + 1.96 * sd_days / sqrt(n_cities)
    replace ci_lower = mean_days if missing(ci_lower)
    replace ci_upper = mean_days if missing(ci_upper)

    tempfile historical_summary
    save `historical_summary', replace
restore

********************************************************************************
* 3. Future CMIP6 projection summaries
********************************************************************************

capture confirm file "${INTERMEDIATE}/fig5_cmip6_city_year_threshold_days.csv"
if _rc {
    display as error "Python output was not found:"
    display as error "${INTERMEDIATE}/fig5_cmip6_city_year_threshold_days.csv"
    display as error "Run submission_code/Python/05_main_figure5_cmip6_threshold_days.py first."
    exit 601
}

import delimited using "${INTERMEDIATE}/fig5_cmip6_city_year_threshold_days.csv", ///
    clear varnames(1) stringcols(_all) encoding("utf-8")

destring year threshold exceed_days annual_precip_mm, replace force
drop if missing(year) | missing(threshold) | missing(exceed_days)

replace scenario = lower(scenario)
keep if inlist(scenario, "ssp245", "ssp585")

preserve
    count if inrange(year, 2015, 2024) & !missing(annual_precip_mm)
    if r(N) > 0 {
        keep if inrange(year, 2015, 2024)
    }
    collapse (mean) avg_prcp = annual_precip_mm, by(city)
    tempfile projection_climate
    save `projection_climate', replace
restore

merge m:1 city using `projection_climate', keep(master match) nogen
merge m:1 city using `city_status_by_name', keep(master match) nogen

replace sponge_city = 0 if missing(sponge_city)
replace local_exposure = 0 if missing(local_exposure)

drop if sponge_city == 0 & local_exposure == 1

gen str20 city_group = cond(sponge_city == 1, "sponge_cities", "others")
gen str20 climate_zone = ""
replace climate_zone = "semi_humid"  if avg_prcp >= 400 & avg_prcp < 800
replace climate_zone = "humid"       if avg_prcp >= 800 & avg_prcp <= 1500
replace climate_zone = "hyper_humid" if avg_prcp > 1500

keep if (climate_zone == "semi_humid"  & threshold == 20) | ///
        (climate_zone == "humid"       & threshold == 50) | ///
        (climate_zone == "hyper_humid" & threshold == 80)

save "${INTERMEDIATE}/fig5_projection_city_model_threshold_days.dta", replace

preserve
    keep if year >= 2025

    collapse (mean) exceed_days, ///
        by(city year scenario threshold city_group climate_zone sponge_city avg_prcp)

    collapse (mean) mean_days = exceed_days ///
             (sd)   sd_days   = exceed_days ///
             (count) n_cities = exceed_days, ///
             by(year city_group climate_zone threshold scenario)

    gen str12 period = ""
    replace period = "Near-Future" if year >= 2025 & year <= 2060
    replace period = "Far-Future"  if year >= 2061 & year <= 2100

    gen ci_lower = mean_days - 1.96 * sd_days / sqrt(n_cities)
    gen ci_upper = mean_days + 1.96 * sd_days / sqrt(n_cities)
    replace ci_lower = mean_days if missing(ci_lower)
    replace ci_upper = mean_days if missing(ci_upper)

    tempfile projection_summary
    save `projection_summary', replace
restore

use `historical_summary', clear
append using `projection_summary'
order year period city_group climate_zone threshold scenario ///
    mean_days ci_lower ci_upper sd_days n_cities
sort climate_zone city_group threshold scenario year
save "${INTERMEDIATE}/fig5_projection_timeseries.dta", replace
export delimited using "${OUTPUT}/fig5_projection_timeseries.csv", replace

********************************************************************************
* 4. City-level output for the ArcGIS Pro map panels
********************************************************************************

use "${INTERMEDIATE}/fig5_projection_city_model_threshold_days.dta", clear
keep if year >= 2025

collapse (mean) exceed_days, ///
    by(city scenario threshold city_group climate_zone sponge_city avg_prcp)

reshape wide exceed_days, ///
    i(city threshold city_group climate_zone sponge_city avg_prcp) j(scenario) string

rename exceed_daysssp245 ssp245_mean_days
rename exceed_daysssp585 ssp585_mean_days
gen ensemble_mean_days = (ssp245_mean_days + ssp585_mean_days) / 2

order city city_group sponge_city climate_zone threshold avg_prcp ///
    ssp245_mean_days ssp585_mean_days ensemble_mean_days
sort climate_zone city_group city

export delimited using "${OUTPUT}/fig5_city_level_map_input.csv", replace

log close
