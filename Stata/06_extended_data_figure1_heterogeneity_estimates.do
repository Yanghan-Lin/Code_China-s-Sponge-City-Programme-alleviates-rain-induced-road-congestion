********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    06_extended_data_figure1_heterogeneity_estimates.do
* Purpose: Estimate heterogeneity results used in Extended Data Fig. 1
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

capture log close _all
log using "${LOGS}/06_extended_data_figure1_heterogeneity_estimates.log", replace text

capture which reghdfe
if _rc {
    display as error "The user-written Stata command reghdfe is required."
    display as error "Install it before running this script: ssc install reghdfe, replace"
    exit 111
}

********************************************************************************
* 1. Load monthly panel and build city-level heterogeneity groups
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    gen byte analysis_sample = 1
}

foreach var in light_2014 pop_2014 altitude_2014 slope_2014 {
    capture confirm variable `var'
    if _rc {
        display as error "Required baseline variable `var' was not found."
        exit 111
    }
}

capture confirm variable month_id
if _rc {
    display as error "Variable month_id was not found in the monthly panel."
    exit 111
}

preserve
    keep city_code light_2014 pop_2014 altitude_2014 slope_2014 analysis_sample
    bysort city_code: keep if _n == 1
    keep if analysis_sample == 1
    drop if missing(light_2014) | missing(pop_2014) | ///
        missing(altitude_2014) | missing(slope_2014)

    xtile g_light3 = light_2014, nq(3)
    xtile g_pop3   = pop_2014,   nq(3)

    gen byte g_alt3 = .
    replace g_alt3 = 1 if altitude_2014 <= 100
    replace g_alt3 = 2 if altitude_2014 > 100 & altitude_2014 <= 300
    replace g_alt3 = 3 if altitude_2014 > 300

    gen byte g_slope3 = .
    replace g_slope3 = 1 if slope_2014 <= 5
    replace g_slope3 = 2 if slope_2014 > 5 & slope_2014 <= 10
    replace g_slope3 = 3 if slope_2014 > 10

    keep city_code g_light3 g_pop3 g_alt3 g_slope3
    tempfile city_groups
    save `city_groups', replace
restore

merge m:1 city_code using `city_groups', keep(master match) nogen

********************************************************************************
* 2. Helper program for group-specific regressions
********************************************************************************

capture program drop estimate_group_effect
program define estimate_group_effect
    syntax, PANEL(string) DIMENSION(string) GROUPVAR(name) GROUP(integer) ///
        GROUPLABEL(string) HANDLE(name)

    quietly reghdfe tcdi_im policy ///
        if analysis_sample == 1 & `groupvar' == `group', ///
        absorb(city_code month_id) vce(cluster city_code)

    scalar b_tmp = _b[policy]
    scalar se_tmp = _se[policy]
    scalar p_tmp = 2 * ttail(e(df_r), abs(b_tmp / se_tmp))
    scalar lo_tmp = b_tmp - invnormal(0.975) * se_tmp
    scalar hi_tmp = b_tmp + invnormal(0.975) * se_tmp

    tempvar esample city_tag
    gen byte `esample' = e(sample)
    egen byte `city_tag' = tag(city_code) if `esample'
    quietly count if `esample'
    scalar n_obs_tmp = r(N)
    quietly count if `city_tag' == 1
    scalar n_city_tmp = r(N)

    local stars ""
    if p_tmp < 0.01 {
        local stars "***"
    }
    else if p_tmp < 0.05 {
        local stars "**"
    }
    else if p_tmp < 0.10 {
        local stars "*"
    }

    post `handle' ("`panel'") ("`dimension'") ("`groupvar'") (`group') ///
        ("`grouplabel'") (b_tmp) (se_tmp) (lo_tmp) (hi_tmp) ///
        (p_tmp) ("`stars'") (n_obs_tmp) (n_city_tmp)

    drop `esample' `city_tag'
end

********************************************************************************
* 3. Estimate Extended Data Fig. 1 panels
********************************************************************************

tempfile heterogeneity_results
tempname handle

postfile `handle' str4 panel str24 dimension str16 groupvar int group ///
    str20 group_label double beta se ci_lo ci_hi p_value str4 stars ///
    double observations cities using `heterogeneity_results', replace

estimate_group_effect, panel("a") dimension("Nighttime light") ///
    groupvar(g_light3) group(1) grouplabel("Low") handle(`handle')
estimate_group_effect, panel("a") dimension("Nighttime light") ///
    groupvar(g_light3) group(2) grouplabel("Mid") handle(`handle')
estimate_group_effect, panel("a") dimension("Nighttime light") ///
    groupvar(g_light3) group(3) grouplabel("High") handle(`handle')

estimate_group_effect, panel("b") dimension("Population") ///
    groupvar(g_pop3) group(1) grouplabel("Low") handle(`handle')
estimate_group_effect, panel("b") dimension("Population") ///
    groupvar(g_pop3) group(2) grouplabel("Mid") handle(`handle')
estimate_group_effect, panel("b") dimension("Population") ///
    groupvar(g_pop3) group(3) grouplabel("High") handle(`handle')

estimate_group_effect, panel("c") dimension("Elevation") ///
    groupvar(g_alt3) group(1) grouplabel("0-100m") handle(`handle')
estimate_group_effect, panel("c") dimension("Elevation") ///
    groupvar(g_alt3) group(2) grouplabel("100-300m") handle(`handle')
estimate_group_effect, panel("c") dimension("Elevation") ///
    groupvar(g_alt3) group(3) grouplabel(">300m") handle(`handle')

estimate_group_effect, panel("d") dimension("Slope") ///
    groupvar(g_slope3) group(1) grouplabel("0-5 deg") handle(`handle')
estimate_group_effect, panel("d") dimension("Slope") ///
    groupvar(g_slope3) group(2) grouplabel("5-10 deg") handle(`handle')
estimate_group_effect, panel("d") dimension("Slope") ///
    groupvar(g_slope3) group(3) grouplabel(">10 deg") handle(`handle')

postclose `handle'

use `heterogeneity_results', clear
order panel dimension groupvar group group_label beta se ci_lo ci_hi ///
    p_value stars observations cities
sort panel group

export delimited using "${OUTPUT}/extended_data_fig1_heterogeneity.csv", replace

log close
