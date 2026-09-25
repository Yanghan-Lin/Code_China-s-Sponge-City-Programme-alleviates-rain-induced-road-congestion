********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    07_extended_data_figure2_gelbach_estimates.do
* Purpose: Estimate and export the Gelbach decomposition in Extended Data Fig. 2
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
log using "${LOGS}/07_extended_data_figure2_gelbach_estimates.log", replace text

capture which reghdfe
if _rc {
    display as error "The user-written Stata command reghdfe is required."
    display as error "Install it before running this script: ssc install reghdfe, replace"
    exit 111
}

********************************************************************************
* 1. Match annual procurement variables to the monthly analysis sample
********************************************************************************

local mechanisms ln_proc_source_lid ln_proc_gray_drain ln_proc_keynode_om ///
    ln_proc_smart_monitor ln_proc_bluegreen ln_proc_plan_design

use "${PROCESSED}/monthly_panel_prepared.dta", clear
keep if analysis_sample == 1

* Assign log(amount + 1) from the prepared annual panel to each city-year's months.
foreach var of local mechanisms {
    capture drop `var'
}
merge m:1 city_code year using "${PROCESSED}/yearly_panel_prepared.dta", ///
    keepusing(`mechanisms') keep(master match) nogen

********************************************************************************
* 2. Estimate the full and short models on an identical sample
********************************************************************************

* Equations (8a)-(8b): city and year-by-month fixed effects, as in the baseline.
quietly reghdfe tcdi_im policy `mechanisms', ///
    absorb(city_code month_id) vce(cluster city_code) tolerance(1e-12)

matrix full_coefficients = e(b)
scalar theta_full = _b[policy]
scalar se_full = _se[policy]
scalar n_obs = e(N)
scalar n_cities = e(N_clust)

foreach var in policy `mechanisms' {
    if missing(_se[`var']) | _se[`var'] == 0 {
        display as error "The full-model coefficient on `var' is not identified."
        exit 459
    }
}

* Lock the sample after complete-case selection and removal of singleton groups.
keep if e(sample)

quietly reghdfe tcdi_im policy, ///
    absorb(city_code month_id) vce(cluster city_code) tolerance(1e-12)
assert e(sample)
scalar theta_short = _b[policy]
scalar se_short = _se[policy]
scalar coefficient_change = theta_short - theta_full

if abs(coefficient_change) < 1e-12 | abs(theta_short) < 1e-12 {
    display as error "A zero coefficient change or baseline effect prevents share calculation."
    exit 459
}

scalar explained_pct = 100 * coefficient_change / theta_short

********************************************************************************
* 3. Decompose the SCP coefficient change using auxiliary regressions
********************************************************************************

* Gelbach (2016), Equations (8c)-(8e): C_j = pi_j * gamma_j.
* Shares are 100*C_j/(theta_short-theta_full). For the reported negative,
* attenuating coefficients this equals -100*C_j/(abs(theta_short)-abs(theta_full))
* in Equation (8f). Signed contributions are retained.

tempfile decomposition_results
tempname handle
postfile `handle' int mechanism_order str32 mechanism str60 mechanism_label ///
    double pi gamma contribution share_pct using `decomposition_results', replace

scalar contribution_sum = 0
local j = 0
foreach var of local mechanisms {
    local ++j
    local label ""
    if `j' == 1 local label "Source-control LID procurement"
    if `j' == 2 local label "Grey drainage procurement"
    if `j' == 3 local label "Key-node O&M procurement"
    if `j' == 4 local label "Smart monitoring procurement"
    if `j' == 5 local label "Blue-green restoration procurement"
    if `j' == 6 local label "Planning/design procurement"

    quietly reghdfe `var' policy, ///
        absorb(city_code month_id) vce(cluster city_code) tolerance(1e-12)
    assert e(sample)

    scalar pi_j = _b[policy]
    scalar gamma_j = full_coefficients[1, colnumb(full_coefficients, "`var'")]
    scalar contribution_j = pi_j * gamma_j
    scalar share_j = 100 * contribution_j / coefficient_change
    scalar contribution_sum = contribution_sum + contribution_j

    post `handle' (`j') ("`var'") ("`label'") ///
        (pi_j) (gamma_j) (contribution_j) (share_j)
}
postclose `handle'

scalar identity_error = contribution_sum - coefficient_change
if abs(identity_error) > 1e-8 * max(1, abs(coefficient_change)) {
    display as error "Gelbach contributions do not sum to the SCP coefficient change."
    exit 459
}

********************************************************************************
* 4. Export mechanism contributions and model summary for R
********************************************************************************

use `decomposition_results', clear
sort mechanism_order
format pi gamma contribution %16.10f
format share_pct %12.6f
export delimited using "${OUTPUT}/extended_data_fig2_gelbach_components.csv", replace

clear
set obs 1
gen double baseline_att = theta_short
gen double controlled_att = theta_full
gen double baseline_se = se_short
gen double controlled_se = se_full
gen double coefficient_change = scalar(coefficient_change)
gen double explained_pct = scalar(explained_pct)
gen double contribution_sum = scalar(contribution_sum)
gen double identity_error = scalar(identity_error)
gen long observations = n_obs
gen int cities = n_cities
format baseline_att controlled_att baseline_se controlled_se ///
    coefficient_change contribution_sum identity_error %16.10f
format explained_pct %12.6f
export delimited using "${OUTPUT}/extended_data_fig2_gelbach_summary.csv", replace

list baseline_att controlled_att explained_pct observations cities, noobs
log close
