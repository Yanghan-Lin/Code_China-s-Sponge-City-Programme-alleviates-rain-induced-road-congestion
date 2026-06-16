********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    03_main_figure3_precipitation_estimates.do
* Purpose: Estimate non-linear precipitation-response results for Fig. 3a-d
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
global INTERMEDIATE  "${SUBMISSION}/data/intermediate"
global OUTPUT        "${SUBMISSION}/outputs"
global LOGS          "${SUBMISSION}/logs"

capture mkdir "${INTERMEDIATE}"
capture mkdir "${OUTPUT}"
capture mkdir "${LOGS}"

capture log close _all
log using "${LOGS}/03_main_figure3_precipitation_estimates.log", replace text

capture which reghdfe
if _rc {
    display as error "The user-written Stata command reghdfe is required."
    display as error "Install it before running this script: ssc install reghdfe, replace"
    exit 111
}

********************************************************************************
* 1. Load daily panel and construct bins
********************************************************************************

use "${PROCESSED}/daily_panel_prepared.dta", clear

* Ensure the main analysis-sample marker exists.
capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
}

* Temperature bins. The 15-20 C bin is the reference category in regressions.
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

* Humidity bins. The 0-10% bin is the reference category in regressions.
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

* Daily precipitation bins. The no-rain bin is shown at x = -5 in the figure.
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

capture drop week_id
gen int week_id = wofd(date)
format week_id %tw

********************************************************************************
* 2. Define hydroclimatic groups from city-level mean annual precipitation
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
    save `precip_groups', replace
restore

merge m:1 city_code using `precip_groups', keep(master match) nogen

capture label drop precip_group_label
label define precip_group_label ///
    1 "Semi-humid regions" ///
    2 "Humid regions" ///
    3 "Hyper-humid regions"
label values precip_group precip_group_label

********************************************************************************
* 3. Helper program for non-linear precipitation margins
********************************************************************************

capture program drop estimate_precip_panel
program define estimate_precip_panel
    syntax, PANELID(string) PANELTITLE(string) [GROUP(integer)]

    local sample_condition "analysis_sample == 1"
    if "`group'" != "" {
        local sample_condition "`sample_condition' & precip_group == `group'"
    }

    reghdfe tcdi_im ///
        i.precp_bin##i.policy ///
        ib5.temp_bin ib1.humid_bin wind visib ///
        if `sample_condition', ///
        absorb(date city_code#month city_code#weekend) vce(cluster city_code date)

    local n_model = e(N)

    tempfile margins_raw
    margins precp_bin#policy, saving("`margins_raw'", replace)

    preserve
        use "`margins_raw'", clear

        * Standardize variable names across Stata versions.
        capture confirm variable precp_bin
        if _rc {
            capture confirm variable _m1
            if !_rc rename _m1 precp_bin
        }
        capture confirm variable policy
        if _rc {
            capture confirm variable _m2
            if !_rc rename _m2 policy
        }

        capture confirm variable _ci_lb
        if _rc {
            capture confirm variable _ci_l
            if !_rc rename _ci_l _ci_lb
        }
        capture confirm variable _ci_ub
        if _rc {
            capture confirm variable _ci_u
            if !_rc rename _ci_u _ci_ub
        }

        capture destring precp_bin, replace force
        capture destring policy, replace force

        gen str20 panel = "`panelid'"
        gen str40 panel_title = "`paneltitle'"
        gen long N = `n_model'

        gen double prec_mid = .
        replace prec_mid = -5  if precp_bin == 1
        replace prec_mid = 5   if precp_bin == 2
        replace prec_mid = 15  if precp_bin == 3
        replace prec_mid = 25  if precp_bin == 4
        replace prec_mid = 35  if precp_bin == 5
        replace prec_mid = 45  if precp_bin == 6
        replace prec_mid = 55  if precp_bin == 7
        replace prec_mid = 65  if precp_bin == 8
        replace prec_mid = 75  if precp_bin == 9
        replace prec_mid = 85  if precp_bin == 10
        replace prec_mid = 95  if precp_bin == 11
        replace prec_mid = 105 if precp_bin == 12

        gen str12 precipitation_bin = ""
        replace precipitation_bin = "0"          if precp_bin == 1
        replace precipitation_bin = "(0, 10]"    if precp_bin == 2
        replace precipitation_bin = "(10, 20]"   if precp_bin == 3
        replace precipitation_bin = "(20, 30]"   if precp_bin == 4
        replace precipitation_bin = "(30, 40]"   if precp_bin == 5
        replace precipitation_bin = "(40, 50]"   if precp_bin == 6
        replace precipitation_bin = "(50, 60]"   if precp_bin == 7
        replace precipitation_bin = "(60, 70]"   if precp_bin == 8
        replace precipitation_bin = "(70, 80]"   if precp_bin == 9
        replace precipitation_bin = "(80, 90]"   if precp_bin == 10
        replace precipitation_bin = "(90, 100]"  if precp_bin == 11
        replace precipitation_bin = "(100, Inf)" if precp_bin == 12

        gen str12 policy_label = cond(policy == 1, "With SCP", "Without SCP")

        rename _margin margin
        rename _se se
        rename _ci_lb ci_low
        rename _ci_ub ci_high

        keep panel panel_title N precp_bin precipitation_bin prec_mid ///
            policy policy_label margin se ci_low ci_high
        order panel panel_title N precp_bin precipitation_bin prec_mid ///
            policy policy_label margin se ci_low ci_high
        sort precp_bin policy

        save "${INTERMEDIATE}/fig3_`panelid'_precipitation_margins.dta", replace
        export delimited using "${OUTPUT}/fig3_`panelid'_precipitation_margins.csv", replace
    restore
end

********************************************************************************
* 4. Estimate full-sample and hydroclimatic-region panels
********************************************************************************

estimate_precip_panel, panelid("a_full")  paneltitle("Full sample")
estimate_precip_panel, panelid("b_semi")  paneltitle("Semi-humid regions") group(1)
estimate_precip_panel, panelid("c_humid") paneltitle("Humid regions") group(2)
estimate_precip_panel, panelid("d_hyper") paneltitle("Hyper-humid regions") group(3)

use "${INTERMEDIATE}/fig3_a_full_precipitation_margins.dta", clear
append using "${INTERMEDIATE}/fig3_b_semi_precipitation_margins.dta"
append using "${INTERMEDIATE}/fig3_c_humid_precipitation_margins.dta"
append using "${INTERMEDIATE}/fig3_d_hyper_precipitation_margins.dta"

export delimited using "${OUTPUT}/fig3_precipitation_margins.csv", replace

********************************************************************************
* 5. Finish
********************************************************************************

display as text "Fig. 3 precipitation margins exported to: ${OUTPUT}"
log close

