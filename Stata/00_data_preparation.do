********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    00_data_preparation.do
* Purpose: Prepare the analysis-ready panel datasets used in the manuscript
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off

********************************************************************************
* 0. User paths
********************************************************************************

* Edit this path if the replication folder is stored elsewhere.
global PROJECT_ROOT "C:/Users/Administrator/Documents/Sponge City and road congestion paper"

global RAW_DATA       "${PROJECT_ROOT}"
global SUBMISSION    "${PROJECT_ROOT}/submission_code"
global PROCESSED     "${SUBMISSION}/data/processed"
global INTERMEDIATE  "${SUBMISSION}/data/intermediate"
global OUTPUT        "${SUBMISSION}/outputs"
global LOGS          "${SUBMISSION}/logs"

capture mkdir "${SUBMISSION}"
capture mkdir "${SUBMISSION}/data"
capture mkdir "${PROCESSED}"
capture mkdir "${INTERMEDIATE}"
capture mkdir "${OUTPUT}"
capture mkdir "${LOGS}"

capture log close _all
log using "${LOGS}/00_data_preparation.log", replace text

********************************************************************************
* 1. Common definitions
********************************************************************************

* Cities with local or partial sponge-city exposure before national designation.
* These cities are excluded in the main national-pilot analysis.
local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"

* Procurement mechanism variables used in Fig. 4 and Extended Data Fig. 2.
local procurement_vars ///
    ln_proc_source_lid ///
    ln_proc_gray_drain ///
    ln_proc_keynode_om ///
    ln_proc_smart_monitor ///
    ln_proc_bluegreen ///
    ln_proc_plan_design

********************************************************************************
* 2. Prepare the annual panel
********************************************************************************

use "${RAW_DATA}/sponge_city_tcdi_yearly.dta", clear

* Optional: rebuild the six procurement mechanism variables from the
* contract-level classification output if they are not already in the annual
* panel. Run submission_code/R/00_procurement_contract_mechanism_processing.R
* before this step when starting from raw procurement contracts.
local procurement_csv "${OUTPUT}/procurement_contract_processing/procurement_mechanisms_city_year_for_panel.csv"
capture confirm variable ln_proc_source_lid
if _rc {
    capture confirm file "`procurement_csv'"
    if !_rc {
        preserve
            import delimited using "`procurement_csv'", clear varnames(1) stringcols(_all)
            foreach var in city_code year proc_source_lid proc_gray_drain proc_keynode_om ///
                proc_smart_monitor proc_bluegreen proc_plan_design n_source_lid ///
                n_gray_drain n_keynode_om n_smart_monitor n_bluegreen ///
                n_plan_design ln_proc_source_lid ln_proc_gray_drain ///
                ln_proc_keynode_om ln_proc_smart_monitor ln_proc_bluegreen ///
                ln_proc_plan_design {
                capture destring `var', replace ignore(" ")
            }
            tempfile procurement_mechanisms
            save `procurement_mechanisms', replace
        restore
        merge 1:1 city_code year using `procurement_mechanisms', nogen keep(master match)

        foreach var of local procurement_vars {
            capture confirm variable `var'
            if _rc {
                gen `var' = 0
            }
            else {
                replace `var' = 0 if missing(`var')
            }
        }
    }
}

* Mark the main analysis sample without deleting observations.
capture drop analysis_sample
gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
label variable analysis_sample "Main sample excluding local or partial SCP exposure"

* Baseline variables used for balance tests and selection checks.
capture label variable road_area_pc_2014     "Road area per capita in 2014"
capture label variable highway_density_2014  "Highway-route density in 2014"

* Procurement variables are stored as log(amount + 1), with amount measured in
* 10,000 RMB. Labels are added here for downstream tables and figures.
capture label variable ln_proc_source_lid    "log(Source-control LID procurement + 1)"
capture label variable ln_proc_gray_drain    "log(Grey drainage procurement + 1)"
capture label variable ln_proc_keynode_om    "log(Key-node O&M procurement + 1)"
capture label variable ln_proc_smart_monitor "log(Smart monitoring procurement + 1)"
capture label variable ln_proc_bluegreen     "log(Blue-green restoration procurement + 1)"
capture label variable ln_proc_plan_design   "log(Planning and design procurement + 1)"

compress
save "${PROCESSED}/yearly_panel_prepared.dta", replace

********************************************************************************
* 3. Prepare the monthly panel
********************************************************************************

use "${RAW_DATA}/sponge_city_tcdi_monthly.dta", clear

capture drop analysis_sample
gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
label variable analysis_sample "Main sample excluding local or partial SCP exposure"

* Create a running month index if it is not already available.
capture confirm variable month_number
if _rc {
    quietly summarize month_id, meanonly
    gen int month_number = month_id - r(min)
    label variable month_number "Running month index"
}

* Add annual procurement mechanisms to each month in the same city-year.
local needs_procurement 0
foreach v of local procurement_vars {
    capture confirm variable `v'
    if _rc local needs_procurement 1
}

if `needs_procurement' {
    preserve
        use "${RAW_DATA}/sponge_city_tcdi_yearly.dta", clear
        local available_procurement
        foreach v of local procurement_vars {
            capture confirm variable `v'
            if !_rc local available_procurement `available_procurement' `v'
        }
        keep city_code year `available_procurement'
        duplicates drop city_code year, force
        tempfile procurement_mechanisms
        save `procurement_mechanisms', replace
    restore

    merge m:1 city_code year using `procurement_mechanisms', keep(master match) nogen
}

capture label variable ln_proc_source_lid    "log(Source-control LID procurement + 1)"
capture label variable ln_proc_gray_drain    "log(Grey drainage procurement + 1)"
capture label variable ln_proc_keynode_om    "log(Key-node O&M procurement + 1)"
capture label variable ln_proc_smart_monitor "log(Smart monitoring procurement + 1)"
capture label variable ln_proc_bluegreen     "log(Blue-green restoration procurement + 1)"
capture label variable ln_proc_plan_design   "log(Planning and design procurement + 1)"

* Add time-varying traffic-supply controls if they are stored in the monthly panel.
capture confirm variable taxi_panel
if !_rc {
    capture confirm variable ln_taxi_panel
    if _rc gen double ln_taxi_panel = ln(taxi_panel) if taxi_panel > 0
    label variable ln_taxi_panel "log(Number of taxis)"
}

capture confirm variable bus_panel
if !_rc {
    capture confirm variable ln_bus_panel
    if _rc gen double ln_bus_panel = ln(bus_panel) if bus_panel > 0
    label variable ln_bus_panel "log(Number of public buses)"
}

capture confirm variable private_car_panel
if !_rc {
    capture confirm variable ln_private_car_panel
    if _rc gen double ln_private_car_panel = ln(private_car_panel) if private_car_panel > 0
}

capture confirm variable ln_private_car_panel
if !_rc {
    label variable ln_private_car_panel "log(Number of private cars)"
}

* Add annual urban construction investment controls if the auxiliary file exists.
local needs_transport_investment 0
capture confirm variable ln_public_transport_inv
if _rc local needs_transport_investment 1
capture confirm variable ln_road_bridge_inv
if _rc local needs_transport_investment 1

if `needs_transport_investment' {
    capture confirm file "${PROJECT_ROOT}/organized_code/outputs/urban_construction_transport_investment_2015_2024.dta"
    if !_rc {
        merge m:1 city_code year using ///
            "${PROJECT_ROOT}/organized_code/outputs/urban_construction_transport_investment_2015_2024.dta", ///
            keep(master match) nogen
    }
}

capture label variable ln_public_transport_inv "log(Public transport investment + 1)"
capture label variable ln_road_bridge_inv      "log(Road and bridge investment + 1)"

* Construct 2014 baseline weather variables used in robustness checks if needed.
capture confirm variable precp2014_sum
if _rc {
    local weather_required "precp snow temp humid visib wind"
    local missing_weather 0
    foreach v of local weather_required {
        capture confirm variable `v'
        if _rc local missing_weather 1
    }

    if `missing_weather' == 0 {
        preserve
            keep city city_code year precp snow temp humid visib wind
            keep if year == 2014
            collapse (sum) precp snow (mean) temp humid visib wind, by(city city_code)

            rename precp precp2014_sum
            rename snow  snow2014_sum
            rename temp  temp2014_avg
            rename humid humid2014_avg
            rename visib visib2014_avg
            rename wind  wind2014_avg

            tempfile weather2014
            save `weather2014', replace
        restore

        merge m:1 city city_code using `weather2014', nogen
    }
}

compress
save "${PROCESSED}/monthly_panel_prepared.dta", replace

********************************************************************************
* 4. Prepare the weekly panel
********************************************************************************

use "${RAW_DATA}/sponge_city_tcdi_weekly.dta", clear

capture drop analysis_sample
gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
label variable analysis_sample "Main sample excluding local or partial SCP exposure"

compress
save "${PROCESSED}/weekly_panel_prepared.dta", replace

********************************************************************************
* 5. Prepare the daily panel
********************************************************************************

use "${RAW_DATA}/sponge_city_tcdi_daily.dta", clear

capture drop analysis_sample
gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
label variable analysis_sample "Main sample excluding local or partial SCP exposure"

compress
save "${PROCESSED}/daily_panel_prepared.dta", replace

********************************************************************************
* 6. Finish
********************************************************************************

display as text "Data preparation completed."
display as text "Prepared files are stored in: ${PROCESSED}"

log close
