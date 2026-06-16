********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    12_supplementary_table8_precipitation_threshold_table.do
* Purpose: Reproduce Supplementary Table 8 precipitation-bin regressions
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off

********************************************************************************
* 0. User paths and dependencies
********************************************************************************

* Run 00_data_preparation.do before this script.
global PROJECT_ROOT "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
global SUBMISSION    "${PROJECT_ROOT}/submission_code"
global PROCESSED     "${SUBMISSION}/data/processed"
global OUTPUT        "${SUBMISSION}/outputs"
global LOGS          "${SUBMISSION}/logs"

capture mkdir "${OUTPUT}"
capture mkdir "${LOGS}"
capture mkdir "${OUTPUT}/supplementary_tables"

capture log close _all
log using "${LOGS}/12_supplementary_table8_precipitation_threshold_table.log", replace text

foreach cmd in reghdfe esttab {
    capture which `cmd'
    if _rc {
        display as error "The user-written Stata command `cmd' is required."
        exit 111
    }
}

********************************************************************************
* 1. Load daily panel and construct weather bins
********************************************************************************

use "${PROCESSED}/daily_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
}

* Temperature bins. The 15-20 C bin is the omitted reference category.
capture drop temp_bin
gen byte temp_bin = .
replace temp_bin = 1 if temp <= 0
replace temp_bin = 2 if temp > 0  & temp <= 5
replace temp_bin = 3 if temp > 5  & temp <= 10
replace temp_bin = 4 if temp > 10 & temp <= 15
replace temp_bin = 5 if temp > 15 & temp <= 20
replace temp_bin = 6 if temp > 20 & temp <= 25
replace temp_bin = 7 if temp > 25 & temp <= 30
replace temp_bin = 8 if temp > 30

capture label drop temp_bin_label
label define temp_bin_label ///
    1 "(-Inf, 0]" 2 "(0, 5]" 3 "(5, 10]" 4 "(10, 15]" ///
    5 "(15, 20]" 6 "(20, 25]" 7 "(25, 30]" 8 "(30, Inf)"
label values temp_bin temp_bin_label

* Humidity bins. The 0-10% bin is the omitted reference category.
capture drop humid_bin
gen byte humid_bin = .
replace humid_bin = 1  if humid <= 10
replace humid_bin = 2  if humid > 10 & humid <= 20
replace humid_bin = 3  if humid > 20 & humid <= 30
replace humid_bin = 4  if humid > 30 & humid <= 40
replace humid_bin = 5  if humid > 40 & humid <= 50
replace humid_bin = 6  if humid > 50 & humid <= 60
replace humid_bin = 7  if humid > 60 & humid <= 70
replace humid_bin = 8  if humid > 70 & humid <= 80
replace humid_bin = 9  if humid > 80 & humid <= 90
replace humid_bin = 10 if humid > 90

capture label drop humid_bin_label
label define humid_bin_label ///
    1 "(0, 10]" 2 "(10, 20]" 3 "(20, 30]" 4 "(30, 40]" 5 "(40, 50]" ///
    6 "(50, 60]" 7 "(60, 70]" 8 "(70, 80]" 9 "(80, 90]" 10 "(90, Inf)"
label values humid_bin humid_bin_label

* Daily precipitation bins. The no-rain bin is the reference category.
capture drop precp_bin
gen byte precp_bin = .
replace precp_bin = 1  if precp == 0
replace precp_bin = 2  if precp > 0   & precp <= 10
replace precp_bin = 3  if precp > 10  & precp <= 20
replace precp_bin = 4  if precp > 20  & precp <= 30
replace precp_bin = 5  if precp > 30  & precp <= 40
replace precp_bin = 6  if precp > 40  & precp <= 50
replace precp_bin = 7  if precp > 50  & precp <= 60
replace precp_bin = 8  if precp > 60  & precp <= 70
replace precp_bin = 9  if precp > 70  & precp <= 80
replace precp_bin = 10 if precp > 80  & precp <= 90
replace precp_bin = 11 if precp > 90  & precp <= 100
replace precp_bin = 12 if precp > 100

capture label drop precp_bin_label
label define precp_bin_label ///
    1 "0"        2 "(0, 10]"   3 "(10, 20]"  4 "(20, 30]" ///
    5 "(30, 40]" 6 "(40, 50]"  7 "(50, 60]"  8 "(60, 70]" ///
    9 "(70, 80]" 10 "(80, 90]" 11 "(90, 100]" 12 "(100, Inf)"
label values precp_bin precp_bin_label

********************************************************************************
* 2. Define hydroclimatic regions from city-level mean annual precipitation
********************************************************************************

preserve
    keep if analysis_sample == 1
    keep city_code year precp
    collapse (sum) annual_precp = precp, by(city_code year)
    collapse (mean) mean_annual_precp = annual_precp, by(city_code)

    gen byte precip_group = .
    replace precip_group = 1 if mean_annual_precp < 800
    replace precip_group = 2 if mean_annual_precp >= 800  & mean_annual_precp < 1500
    replace precip_group = 3 if mean_annual_precp >= 1500

    tempfile precip_groups
    save "`precip_groups'", replace
restore

merge m:1 city_code using "`precip_groups'", keep(master match) nogen

capture label drop precip_group_label
label define precip_group_label ///
    1 "Semi-humid" ///
    2 "Humid" ///
    3 "Hyper-humid"
label values precip_group precip_group_label

********************************************************************************
* 3. Estimate daily precipitation-bin regressions
********************************************************************************

eststo clear

local controls ib5.temp_bin ib1.humid_bin wind visib
local fixed_effects date city_code#month city_code#weekend

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1, ///
    absorb(`fixed_effects') vce(cluster city_code date)
eststo S8_all

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1 & precip_group == 1, ///
    absorb(`fixed_effects') vce(cluster city_code date)
eststo S8_semi

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1 & precip_group == 2, ///
    absorb(`fixed_effects') vce(cluster city_code date)
eststo S8_humid

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1 & precip_group == 3, ///
    absorb(`fixed_effects') vce(cluster city_code date)
eststo S8_hyper

********************************************************************************
* 4. Export Supplementary Table 8
********************************************************************************

local keep_order ///
    2.precp_bin#1.policy 3.precp_bin#1.policy 4.precp_bin#1.policy ///
    5.precp_bin#1.policy 6.precp_bin#1.policy 7.precp_bin#1.policy ///
    8.precp_bin#1.policy 9.precp_bin#1.policy 10.precp_bin#1.policy ///
    11.precp_bin#1.policy 12.precp_bin#1.policy ///
    1.policy ///
    2.precp_bin 3.precp_bin 4.precp_bin 5.precp_bin 6.precp_bin ///
    7.precp_bin 8.precp_bin 9.precp_bin 10.precp_bin 11.precp_bin 12.precp_bin ///
    1.temp_bin 2.temp_bin 3.temp_bin 4.temp_bin 6.temp_bin 7.temp_bin 8.temp_bin ///
    2.humid_bin 3.humid_bin 4.humid_bin 5.humid_bin 6.humid_bin ///
    7.humid_bin 8.humid_bin 9.humid_bin 10.humid_bin

esttab S8_all S8_semi S8_humid S8_hyper ///
    using "${OUTPUT}/supplementary_tables/supplementary_table8_precipitation_thresholds.rtf", ///
    replace rtf label ///
    title("Threshold responses of SCP effects to daily precipitation") ///
    mtitles("All samples" "Semi-humid" "Humid" "Hyper-humid") ///
    keep(`keep_order') ///
    order(`keep_order') ///
    coeflabels( ///
        2.precp_bin#1.policy  "SCP x Precipitation (0, 10]" ///
        3.precp_bin#1.policy  "SCP x Precipitation (10, 20]" ///
        4.precp_bin#1.policy  "SCP x Precipitation (20, 30]" ///
        5.precp_bin#1.policy  "SCP x Precipitation (30, 40]" ///
        6.precp_bin#1.policy  "SCP x Precipitation (40, 50]" ///
        7.precp_bin#1.policy  "SCP x Precipitation (50, 60]" ///
        8.precp_bin#1.policy  "SCP x Precipitation (60, 70]" ///
        9.precp_bin#1.policy  "SCP x Precipitation (70, 80]" ///
        10.precp_bin#1.policy "SCP x Precipitation (80, 90]" ///
        11.precp_bin#1.policy "SCP x Precipitation (90, 100]" ///
        12.precp_bin#1.policy "SCP x Precipitation (100, Inf]" ///
        1.policy              "SCP" ///
        2.precp_bin           "Precipitation (0, 10]" ///
        3.precp_bin           "Precipitation (10, 20]" ///
        4.precp_bin           "Precipitation (20, 30]" ///
        5.precp_bin           "Precipitation (30, 40]" ///
        6.precp_bin           "Precipitation (40, 50]" ///
        7.precp_bin           "Precipitation (50, 60]" ///
        8.precp_bin           "Precipitation (60, 70]" ///
        9.precp_bin           "Precipitation (70, 80]" ///
        10.precp_bin          "Precipitation (80, 90]" ///
        11.precp_bin          "Precipitation (90, 100]" ///
        12.precp_bin          "Precipitation (100, Inf]" ///
        1.temp_bin            "Temperature (-Inf, 0]" ///
        2.temp_bin            "Temperature (0, 5]" ///
        3.temp_bin            "Temperature (5, 10]" ///
        4.temp_bin            "Temperature (10, 15]" ///
        6.temp_bin            "Temperature (20, 25]" ///
        7.temp_bin            "Temperature (25, 30]" ///
        8.temp_bin            "Temperature (30, Inf)" ///
        2.humid_bin           "Relative humidity (10, 20]" ///
        3.humid_bin           "Relative humidity (20, 30]" ///
        4.humid_bin           "Relative humidity (30, 40]" ///
        5.humid_bin           "Relative humidity (40, 50]" ///
        6.humid_bin           "Relative humidity (50, 60]" ///
        7.humid_bin           "Relative humidity (60, 70]" ///
        8.humid_bin           "Relative humidity (70, 80]" ///
        9.humid_bin           "Relative humidity (80, 90]" ///
        10.humid_bin          "Relative humidity (90, Inf)" ///
    ) ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2_a, labels("Observations" "Adj. R-squared") fmt(%9.0f %9.4f)) ///
    compress nonotes

esttab S8_all S8_semi S8_humid S8_hyper ///
    using "${OUTPUT}/supplementary_tables/supplementary_table8_precipitation_thresholds.csv", ///
    replace csv label ///
    mtitles("All samples" "Semi-humid" "Humid" "Hyper-humid") ///
    keep(`keep_order') ///
    order(`keep_order') ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2_a, labels("Observations" "Adj. R-squared") fmt(%9.0f %9.4f)) ///
    compress nonotes

log close
