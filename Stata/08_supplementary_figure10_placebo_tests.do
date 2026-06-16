********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    08_supplementary_figure10_placebo_tests.do
* Purpose: Draw Supplementary Fig. 10 placebo tests using didplacebo
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
log using "${LOGS}/08_supplementary_figure10_placebo_tests.log", replace text

foreach cmd in reghdfe didplacebo {
    capture which `cmd'
    if _rc {
        display as error "The user-written Stata command `cmd' is required."
        exit 111
    }
}

graph set window fontface "Helvetica"
set scheme s1color
set seed 1

********************************************************************************
* 1. Baseline DID model used as the reference for placebo tests
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear
keep if analysis_sample == 1

gen double t_month = month_id
replace t_month = mofd(month_id) if month_id > 5000 & !missing(month_id)
format t_month %tm

xtset city_code t_month

reghdfe tcdi_im policy, absorb(city_code t_month) vce(cluster city_code)
estimates store did_bbb

********************************************************************************
* 2. Panel a: in-time placebo
********************************************************************************

didplacebo did_bbb, treatvar(policy) pbotime(1(1)10) seed(1)
graph save "${OUTPUT}/supplementary_figure10a_in_time_placebo.gph", replace
graph export "${OUTPUT}/supplementary_figure10a_in_time_placebo.png", ///
    as(png) replace width(2200)

********************************************************************************
* 3. Panel b: in-space placebo
********************************************************************************

didplacebo did_bbb, treatvar(policy) pbounit rep(500) seed(1)
graph save "${OUTPUT}/supplementary_figure10b_in_space_placebo.gph", replace
graph export "${OUTPUT}/supplementary_figure10b_in_space_placebo.png", ///
    as(png) replace width(2200)

********************************************************************************
* 4. Panel c: unrestricted mixed placebo
********************************************************************************

didplacebo did_bbb, treatvar(policy) pbotime(1(1)10) ///
    pbounit pbomix(2) seed(1)
graph save "${OUTPUT}/supplementary_figure10c_mixed_placebo_unrestricted.gph", replace
graph export "${OUTPUT}/supplementary_figure10c_mixed_placebo_unrestricted.png", ///
    as(png) replace width(2200)

********************************************************************************
* 5. Panel d: restricted mixed placebo preserving the cohort structure
********************************************************************************

didplacebo did_bbb, treatvar(policy) pbotime(1(1)10) ///
    pbounit pbomix(3) seed(1)
graph save "${OUTPUT}/supplementary_figure10d_mixed_placebo_restricted.gph", replace
graph export "${OUTPUT}/supplementary_figure10d_mixed_placebo_restricted.png", ///
    as(png) replace width(2200)

log close
