********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    13_supplementary_table8_cohort_robustness_checks.do
* Purpose: Reproduce Supplementary Table 8 robustness checks by SCP cohort
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off
set maxvar 32767

********************************************************************************
* 0. User paths and dependencies
********************************************************************************
global PROJECT_ROOT "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
global SUBMISSION    "${PROJECT_ROOT}/submission_code"
global PROCESSED     "${SUBMISSION}/data/processed"
global OUTPUT        "${SUBMISSION}/outputs"
global LOGS          "${SUBMISSION}/logs"

capture mkdir "${OUTPUT}"
capture mkdir "${LOGS}"
capture mkdir "${OUTPUT}/supplementary_tables"

capture log close _all
log using "${LOGS}/13_supplementary_table8_cohort_robustness_checks.log", replace text

foreach cmd in reghdfe esttab estadd eststo {
    capture which `cmd'
    if _rc {
        display as error "The user-written Stata command `cmd' is required."
        exit 111
    }
}

********************************************************************************
* 1. Load monthly panel and construct additional controls
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
}

foreach var in tcdi_wave12 tcdi_wave345 policy cohort city_code month_id ///
    year month covid_per100k {
    confirm numeric variable `var'
}

local MAIN_SAMPLE analysis_sample == 1
local PILOT_SAMPLE `MAIN_SAMPLE' & (inlist(cohort, 1, 2) | missing(cohort))
local DEMO_SAMPLE `MAIN_SAMPLE' & (inlist(cohort, 3, 4, 5) | missing(cohort))

* Use the group-specific outcomes and comparison cities from Fig. 2b-c.

* COVID-19 intensity is measured in 100 active cases per 100,000 people.
* Use observed-day monthly means and set the control to zero outside 2020-2022.
capture drop covid_intensity_100
gen double covid_intensity_100 = covid_per100k / 100
replace covid_intensity_100 = 0 if year < 2020
replace covid_intensity_100 = 0 if year > 2022
label variable covid_intensity_100 ///
    "COVID-19 intensity (100 active cases per 100,000 people)"

* Construct a centered calendar-month index for city-specific linear trends.
capture drop s8_time
gen double s8_time = ym(year, month)
quietly summarize s8_time if `MAIN_SAMPLE', meanonly
replace s8_time = s8_time - r(min)
label variable s8_time "Continuous calendar-month trend"

eststo clear

********************************************************************************
* 2. Pilot cities: waves 1-2
********************************************************************************

* (1) Controlling for COVID-19 intensity
quietly reghdfe tcdi_wave12 policy covid_intensity_100 ///
    if `PILOT_SAMPLE' & !missing(covid_intensity_100), ///
    absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local covid_control "Yes"
estadd local city_trend "No"
eststo S8_pilot_covid

* (2) Controlling for city-specific linear trends
quietly reghdfe tcdi_wave12 policy ///
    if `PILOT_SAMPLE', ///
    absorb(city_code##c.s8_time month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local covid_control "No"
estadd local city_trend "Yes"
eststo S8_pilot_trend

* (3) Controlling for both COVID-19 intensity and city-specific linear trends
quietly reghdfe tcdi_wave12 policy covid_intensity_100 ///
    if `PILOT_SAMPLE' & !missing(covid_intensity_100), ///
    absorb(city_code##c.s8_time month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local covid_control "Yes"
estadd local city_trend "Yes"
eststo S8_pilot_both

********************************************************************************
* 3. Demonstration cities: waves 3-5
********************************************************************************

* (4) Controlling for COVID-19 intensity
quietly reghdfe tcdi_wave345 policy covid_intensity_100 ///
    if `DEMO_SAMPLE' & !missing(covid_intensity_100), ///
    absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local covid_control "Yes"
estadd local city_trend "No"
eststo S8_demo_covid

* (5) Controlling for city-specific linear trends
quietly reghdfe tcdi_wave345 policy ///
    if `DEMO_SAMPLE', ///
    absorb(city_code##c.s8_time month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local covid_control "No"
estadd local city_trend "Yes"
eststo S8_demo_trend

* (6) Controlling for both COVID-19 intensity and city-specific linear trends
quietly reghdfe tcdi_wave345 policy covid_intensity_100 ///
    if `DEMO_SAMPLE' & !missing(covid_intensity_100), ///
    absorb(city_code##c.s8_time month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local covid_control "Yes"
estadd local city_trend "Yes"
eststo S8_demo_both

********************************************************************************
* 4. Export Supplementary Table 8
********************************************************************************

esttab ///
    S8_pilot_covid S8_pilot_trend S8_pilot_both ///
    S8_demo_covid S8_demo_trend S8_demo_both ///
    using "${OUTPUT}/supplementary_tables/supplementary_table8_cohort_robustness_checks.rtf", ///
    replace rtf label ///
    title("Robustness checks for pilot and demonstration cities") ///
    mgroups("Pilot cities (waves 1-2)" "Demonstration cities (waves 3-5)", ///
            pattern(1 0 0 1 0 0)) ///
    mtitles("COVID-19 intensity" "City trends" "Both controls" ///
            "COVID-19 intensity" "City trends" "Both controls") ///
    keep(policy covid_intensity_100) ///
    order(policy covid_intensity_100) ///
    coeflabels(policy "SCP" ///
               covid_intensity_100 "COVID-19 intensity (100 active cases per 100,000 people)") ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(city_fe ym_fe covid_control city_trend N r2_a, ///
          labels("City FE" "Year-month FE" "COVID-19 intensity control" ///
                 "City-specific linear trends" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

esttab ///
    S8_pilot_covid S8_pilot_trend S8_pilot_both ///
    S8_demo_covid S8_demo_trend S8_demo_both ///
    using "${OUTPUT}/supplementary_tables/supplementary_table8_cohort_robustness_checks.csv", ///
    replace csv label ///
    mgroups("Pilot cities (waves 1-2)" "Demonstration cities (waves 3-5)", ///
            pattern(1 0 0 1 0 0)) ///
    mtitles("COVID-19 intensity" "City trends" "Both controls" ///
            "COVID-19 intensity" "City trends" "Both controls") ///
    keep(policy covid_intensity_100) ///
    order(policy covid_intensity_100) ///
    coeflabels(policy "SCP" ///
               covid_intensity_100 "COVID-19 intensity (100 active cases per 100,000 people)") ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(city_fe ym_fe covid_control city_trend N r2_a, ///
          labels("City FE" "Year-month FE" "COVID-19 intensity control" ///
                 "City-specific linear trends" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

log close

