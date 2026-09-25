********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    15_supplementary_tables10_11_precipitation_sensitivity.do
* Purpose: Reproduce Supplementary Tables 10-11 precipitation sensitivity checks
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
log using "${LOGS}/15_supplementary_tables10_11_precipitation_sensitivity.log", replace text

foreach cmd in reghdfe esttab estadd eststo {
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
replace temp_bin = 8 if temp > 30 & !missing(temp)

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
replace humid_bin = 10 if humid > 90 & !missing(humid)

capture label drop humid_bin_label
label define humid_bin_label ///
    1 "(0, 10]" 2 "(10, 20]" 3 "(20, 30]" 4 "(30, 40]" 5 "(40, 50]" ///
    6 "(50, 60]" 7 "(60, 70]" 8 "(70, 80]" 9 "(80, 90]" 10 "(90, Inf)"
label values humid_bin humid_bin_label

* Table 11 uses 10-mm precipitation bins, with dry days as the reference.
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
replace precp_bin = 12 if precp > 100 & !missing(precp)

capture label drop precp_bin_label
label define precp_bin_label ///
    1 "0"        2 "(0, 10]"   3 "(10, 20]"  4 "(20, 30]" ///
    5 "(30, 40]" 6 "(40, 50]"  7 "(50, 60]"  8 "(60, 70]" ///
    9 "(70, 80]" 10 "(80, 90]" 11 "(90, 100]" 12 "(100, Inf)"
label values precp_bin precp_bin_label

********************************************************************************
* 2. Define hydroclimatic regions from city-level mean annual precipitation
********************************************************************************

capture drop precip_group
preserve
    keep if analysis_sample == 1
    keep city_code year precp
    collapse (sum) annual_precp = precp, by(city_code year)
    collapse (mean) mean_annual_precp = annual_precp, by(city_code)

    gen byte precip_group = .
    replace precip_group = 1 if mean_annual_precp < 800
    replace precip_group = 2 if mean_annual_precp >= 800  & mean_annual_precp < 1500
    replace precip_group = 3 if mean_annual_precp >= 1500 & !missing(mean_annual_precp)

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
* 3. Supplementary Table 10: national sensitivity to 5-mm precipitation bins
********************************************************************************

* Pool precipitation above 100 mm and retain dry days as the reference.
capture drop precp_bin5
gen byte precp_bin5 = .
replace precp_bin5 = 1 if precp == 0
capture label drop precp_bin5_label
label define precp_bin5_label 1 "0"
forvalues j = 1/20 {
    local lo = (`j' - 1) * 5
    local hi = `j' * 5
    local k = `j' + 1
    replace precp_bin5 = `k' if precp > `lo' & precp <= `hi'
    label define precp_bin5_label `k' "(`lo', `hi']", add
}
replace precp_bin5 = 22 if precp > 100 & !missing(precp)
label define precp_bin5_label 22 "(100, Inf]", add
label values precp_bin5 precp_bin5_label

eststo clear

local fixed_effects date city_code#month city_code#weekend

quietly reghdfe tcdi_im ///
    ib1.precp_bin5##i.policy ///
    ib5.temp_bin ib1.humid_bin wind visib ///
    if analysis_sample == 1, ///
    absorb(`fixed_effects') vce(cluster city_code date)
estadd local weather_controls "Yes"
estadd local date_fe "Yes"
estadd local city_month_fe "Yes"
estadd local city_weekend_fe "Yes"
eststo S10_all

* Report the interaction coefficients, standard errors and P values.
local s10_order
forvalues k = 2/22 {
    local s10_order `s10_order' `k'.precp_bin5#1.policy
}

esttab S10_all ///
    using "${OUTPUT}/supplementary_tables/supplementary_table10_precipitation_5mm.rtf", ///
    replace rtf label ///
    title("National 5-mm precipitation-bin sensitivity analysis") ///
    mtitles("All samples") ///
    cells("b(star fmt(4)) se(fmt(4)) p(fmt(3))") ///
    collabels("Coefficient" "Std. error" "P value") ///
    keep(`s10_order') ///
    order(`s10_order') ///
    coeflabels( ///
        2.precp_bin5#1.policy "SCP x Precipitation (0, 5]" ///
        3.precp_bin5#1.policy "SCP x Precipitation (5, 10]" ///
        4.precp_bin5#1.policy "SCP x Precipitation (10, 15]" ///
        5.precp_bin5#1.policy "SCP x Precipitation (15, 20]" ///
        6.precp_bin5#1.policy "SCP x Precipitation (20, 25]" ///
        7.precp_bin5#1.policy "SCP x Precipitation (25, 30]" ///
        8.precp_bin5#1.policy "SCP x Precipitation (30, 35]" ///
        9.precp_bin5#1.policy "SCP x Precipitation (35, 40]" ///
        10.precp_bin5#1.policy "SCP x Precipitation (40, 45]" ///
        11.precp_bin5#1.policy "SCP x Precipitation (45, 50]" ///
        12.precp_bin5#1.policy "SCP x Precipitation (50, 55]" ///
        13.precp_bin5#1.policy "SCP x Precipitation (55, 60]" ///
        14.precp_bin5#1.policy "SCP x Precipitation (60, 65]" ///
        15.precp_bin5#1.policy "SCP x Precipitation (65, 70]" ///
        16.precp_bin5#1.policy "SCP x Precipitation (70, 75]" ///
        17.precp_bin5#1.policy "SCP x Precipitation (75, 80]" ///
        18.precp_bin5#1.policy "SCP x Precipitation (80, 85]" ///
        19.precp_bin5#1.policy "SCP x Precipitation (85, 90]" ///
        20.precp_bin5#1.policy "SCP x Precipitation (90, 95]" ///
        21.precp_bin5#1.policy "SCP x Precipitation (95, 100]" ///
        22.precp_bin5#1.policy "SCP x Precipitation (100, Inf]" ///
    ) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(weather_controls date_fe city_month_fe city_weekend_fe N r2_a, ///
          labels("Meteorological controls" "Date FE" "City-month FE" ///
                 "City-weekend FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

esttab S10_all ///
    using "${OUTPUT}/supplementary_tables/supplementary_table10_precipitation_5mm.csv", ///
    replace csv label ///
    mtitles("All samples") ///
    cells("b(star fmt(4)) se(fmt(4)) p(fmt(3))") ///
    collabels("Coefficient" "Std. error" "P value") ///
    keep(`s10_order') ///
    order(`s10_order') ///
    coeflabels( ///
        2.precp_bin5#1.policy "SCP x Precipitation (0, 5]" ///
        3.precp_bin5#1.policy "SCP x Precipitation (5, 10]" ///
        4.precp_bin5#1.policy "SCP x Precipitation (10, 15]" ///
        5.precp_bin5#1.policy "SCP x Precipitation (15, 20]" ///
        6.precp_bin5#1.policy "SCP x Precipitation (20, 25]" ///
        7.precp_bin5#1.policy "SCP x Precipitation (25, 30]" ///
        8.precp_bin5#1.policy "SCP x Precipitation (30, 35]" ///
        9.precp_bin5#1.policy "SCP x Precipitation (35, 40]" ///
        10.precp_bin5#1.policy "SCP x Precipitation (40, 45]" ///
        11.precp_bin5#1.policy "SCP x Precipitation (45, 50]" ///
        12.precp_bin5#1.policy "SCP x Precipitation (50, 55]" ///
        13.precp_bin5#1.policy "SCP x Precipitation (55, 60]" ///
        14.precp_bin5#1.policy "SCP x Precipitation (60, 65]" ///
        15.precp_bin5#1.policy "SCP x Precipitation (65, 70]" ///
        16.precp_bin5#1.policy "SCP x Precipitation (70, 75]" ///
        17.precp_bin5#1.policy "SCP x Precipitation (75, 80]" ///
        18.precp_bin5#1.policy "SCP x Precipitation (80, 85]" ///
        19.precp_bin5#1.policy "SCP x Precipitation (85, 90]" ///
        20.precp_bin5#1.policy "SCP x Precipitation (90, 95]" ///
        21.precp_bin5#1.policy "SCP x Precipitation (95, 100]" ///
        22.precp_bin5#1.policy "SCP x Precipitation (100, Inf]" ///
    ) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(weather_controls date_fe city_month_fe city_weekend_fe N r2_a, ///
          labels("Meteorological controls" "Date FE" "City-month FE" ///
                 "City-weekend FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

********************************************************************************
* 4. Supplementary Table 11: national and regional models without visibility
********************************************************************************

local controls ib5.temp_bin ib1.humid_bin wind

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1, ///
    absorb(`fixed_effects') vce(cluster city_code date)
estadd local visibility_control "No"
estadd local date_fe "Yes"
estadd local city_month_fe "Yes"
estadd local city_weekend_fe "Yes"
eststo S11_all

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1 & precip_group == 1, ///
    absorb(`fixed_effects') vce(cluster city_code date)
estadd local visibility_control "No"
estadd local date_fe "Yes"
estadd local city_month_fe "Yes"
estadd local city_weekend_fe "Yes"
eststo S11_semi

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1 & precip_group == 2, ///
    absorb(`fixed_effects') vce(cluster city_code date)
estadd local visibility_control "No"
estadd local date_fe "Yes"
estadd local city_month_fe "Yes"
estadd local city_weekend_fe "Yes"
eststo S11_humid

quietly reghdfe tcdi_im ///
    ib1.precp_bin##i.policy ///
    `controls' ///
    if analysis_sample == 1 & precip_group == 3, ///
    absorb(`fixed_effects') vce(cluster city_code date)
estadd local visibility_control "No"
estadd local date_fe "Yes"
estadd local city_month_fe "Yes"
estadd local city_weekend_fe "Yes"
eststo S11_hyper

********************************************************************************
* 5. Export Supplementary Table 11
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
    7.humid_bin 8.humid_bin 9.humid_bin 10.humid_bin ///
    wind _cons

esttab S11_all S11_semi S11_humid S11_hyper ///
    using "${OUTPUT}/supplementary_tables/supplementary_table11_excluding_visibility.rtf", ///
    replace rtf label ///
    title("Sensitivity of the nonlinear SCP estimates to excluding visibility") ///
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
        wind                 "Wind speed" ///
        _cons                "Constant" ///
    ) ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(visibility_control date_fe city_month_fe city_weekend_fe N r2_a, ///
          labels("Visibility control" "Date FE" "City-month FE" ///
                 "City-weekend FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

esttab S11_all S11_semi S11_humid S11_hyper ///
    using "${OUTPUT}/supplementary_tables/supplementary_table11_excluding_visibility.csv", ///
    replace csv label ///
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
        wind                 "Wind speed" ///
        _cons                "Constant" ///
    ) ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(visibility_control date_fe city_month_fe city_weekend_fe N r2_a, ///
          labels("Visibility control" "Date FE" "City-month FE" ///
                 "City-weekend FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

log close

