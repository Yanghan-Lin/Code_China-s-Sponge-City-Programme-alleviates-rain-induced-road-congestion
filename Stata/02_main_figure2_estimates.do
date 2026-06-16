********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    02_main_figure2_estimates.do
* Purpose: Estimate and export the results used in Fig. 2a-d
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
log using "${LOGS}/02_main_figure2_estimates.log", replace text

capture which reghdfe
if _rc {
    display as error "The user-written Stata command reghdfe is required."
    display as error "Install it before running this script: ssc install reghdfe, replace"
    exit 111
}

********************************************************************************
* 1. Helper programs
********************************************************************************

capture program drop export_event_study
program define export_event_study
    syntax using/, PANEL(string) PREFIX(string) MIN(integer) MAX(integer)

    tempfile event_results
    tempname handle
    postfile `handle' str20 panel int k double beta se ci_lo ci_hi using `event_results', replace

    forvalues k = `min'/`max' {
        if `k' < -1 {
            local abs_k = abs(`k')
            local v "`prefix'_pre_`abs_k'"
        }
        else if `k' == -1 {
            local v "reference"
        }
        else if `k' == 0 {
            local v "`prefix'_current"
        }
        else {
            local v "`prefix'_las_`k'"
        }

        if "`v'" == "reference" {
            post `handle' ("`panel'") (`k') (0) (.) (0) (0)
        }
        else {
            capture confirm matrix e(b)
            if !_rc {
                capture scalar b_tmp = _b[`v']
                capture scalar se_tmp = _se[`v']
                if _rc {
                    post `handle' ("`panel'") (`k') (.) (.) (.) (.)
                }
                else {
                    scalar lo_tmp = b_tmp - invnormal(0.975) * se_tmp
                    scalar hi_tmp = b_tmp + invnormal(0.975) * se_tmp
                    post `handle' ("`panel'") (`k') (b_tmp) (se_tmp) (lo_tmp) (hi_tmp)
                }
            }
        }
    }

    postclose `handle'
    preserve
        use `event_results', clear
        export delimited using "`using'", replace
    restore
end

capture program drop post_att
program define post_att
    syntax, HANDLE(name) PANEL(string) CONTRIBUTION(string)

    scalar b_tmp  = _b[policy]
    scalar se_tmp = _se[policy]
    scalar df_tmp = e(df_r)
    if missing(df_tmp) scalar p_tmp = 2 * normal(-abs(b_tmp / se_tmp))
    else scalar p_tmp = 2 * ttail(df_tmp, abs(b_tmp / se_tmp))

    local stars ""
    if p_tmp < 0.10 local stars "*"
    if p_tmp < 0.05 local stars "**"
    if p_tmp < 0.01 local stars "***"

    post `handle' ("`panel'") (b_tmp) (se_tmp) (p_tmp) ("`stars'") ("`contribution'")
end

********************************************************************************
* 2. Load analysis-ready monthly panel
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear

* Keep the national-pilot analysis sample used in the main specification.
keep if analysis_sample == 1

********************************************************************************
* 3. Fig. 2a: full-sample monthly event study
********************************************************************************

capture drop fig2a_rel fig2a_pre_* fig2a_current fig2a_las_*
gen int fig2a_rel = mofd(month_id) - mofd(cohort_month) if treat == 1
replace fig2a_rel = -36 if fig2a_rel < -36 & !missing(fig2a_rel)
replace fig2a_rel =  36 if fig2a_rel >  36 & !missing(fig2a_rel)

forvalues i = 36(-1)1 {
    gen byte fig2a_pre_`i' = (fig2a_rel == -`i' & treat == 1)
}
gen byte fig2a_current = (fig2a_rel == 0 & treat == 1)
forvalues j = 1/36 {
    gen byte fig2a_las_`j' = (fig2a_rel == `j' & treat == 1)
}

local fig2a_leads
forvalues i = 36(-1)2 {
    local fig2a_leads `fig2a_leads' fig2a_pre_`i'
}
local fig2a_lags
forvalues j = 1/36 {
    local fig2a_lags `fig2a_lags' fig2a_las_`j'
}

reghdfe tcdi_im `fig2a_leads' fig2a_current `fig2a_lags', ///
    absorb(city_code month_id) vce(cluster city_code)

export_event_study using "${OUTPUT}/fig2_panel_a_event_study.csv", ///
    panel("a_full_sample") prefix("fig2a") min(-36) max(36)

********************************************************************************
* 4. Fig. 2b: waves 1-2 event study
********************************************************************************

preserve
    keep if inlist(cohort, 1, 2) | missing(cohort)

    capture drop fig2b_rel fig2b_pre_* fig2b_current fig2b_las_*
    gen int fig2b_rel = mofd(month_id) - mofd(cohort_month) if treat == 1
    replace fig2b_rel = -15 if fig2b_rel < -15 & !missing(fig2b_rel)
    replace fig2b_rel =  57 if fig2b_rel >  57 & !missing(fig2b_rel)

    forvalues i = 15(-1)1 {
        gen byte fig2b_pre_`i' = (fig2b_rel == -`i' & treat == 1)
    }
    gen byte fig2b_current = (fig2b_rel == 0 & treat == 1)
    forvalues j = 1/57 {
        gen byte fig2b_las_`j' = (fig2b_rel == `j' & treat == 1)
    }

    local fig2b_leads
    forvalues i = 15(-1)2 {
        local fig2b_leads `fig2b_leads' fig2b_pre_`i'
    }
    local fig2b_lags
    forvalues j = 1/57 {
        local fig2b_lags `fig2b_lags' fig2b_las_`j'
    }

    reghdfe tcdi_wave12 `fig2b_leads' fig2b_current `fig2b_lags', ///
        absorb(city_code month_id) vce(cluster city_code)

    export_event_study using "${OUTPUT}/fig2_panel_b_event_study.csv", ///
        panel("b_waves_1_2") prefix("fig2b") min(-15) max(57)
restore

********************************************************************************
* 5. Fig. 2c: waves 3-5 event study
********************************************************************************

preserve
    keep if inlist(cohort, 3, 4, 5) | missing(cohort)

    capture drop fig2c_cohort_month fig2c_rel fig2c_pre_* fig2c_current fig2c_las_*
    gen double fig2c_cohort_month = mofd(cohort_month)
    replace fig2c_cohort_month = ym(year(cohort_month), 6) if inlist(cohort, 4, 5)
    format fig2c_cohort_month %tm

    gen int fig2c_rel = mofd(month_id) - fig2c_cohort_month if treat == 1
    replace fig2c_rel = -36 if fig2c_rel < -36 & !missing(fig2c_rel)
    replace fig2c_rel =  36 if fig2c_rel >  36 & !missing(fig2c_rel)

    forvalues i = 36(-1)1 {
        gen byte fig2c_pre_`i' = (fig2c_rel == -`i' & treat == 1)
    }
    gen byte fig2c_current = (fig2c_rel == 0 & treat == 1)
    forvalues j = 1/36 {
        gen byte fig2c_las_`j' = (fig2c_rel == `j' & treat == 1)
    }

    local fig2c_leads
    forvalues i = 36(-1)2 {
        local fig2c_leads `fig2c_leads' fig2c_pre_`i'
    }
    local fig2c_lags
    forvalues j = 1/36 {
        local fig2c_lags `fig2c_lags' fig2c_las_`j'
    }

    reghdfe tcdi_wave345 `fig2c_leads' fig2c_current `fig2c_lags', ///
        absorb(city_code month_id) vce(cluster city_code)

    export_event_study using "${OUTPUT}/fig2_panel_c_event_study.csv", ///
        panel("c_waves_3_5") prefix("fig2c") min(-36) max(36)
restore

********************************************************************************
* 6. Fig. 2d: calendar-month-specific policy effects
********************************************************************************

preserve
    statsby beta = _b[policy] se = _se[policy], by(month) clear: ///
        reghdfe tcdi_im policy, absorb(city_code) vce(cluster city_code month_id)

    gen double ci_lo = beta - invnormal(0.975) * se
    gen double ci_hi = beta + invnormal(0.975) * se
    gen str10 panel = "d_month"
    order panel month beta se ci_lo ci_hi
    sort month
    export delimited using "${OUTPUT}/fig2_panel_d_month_effects.csv", replace
restore

********************************************************************************
* 7. Export ATT estimates reported in Fig. 2a-c
********************************************************************************

tempfile att_results
tempname att_handle
postfile `att_handle' str20 panel double beta se p_value str5 stars str10 contribution ///
    using `att_results', replace

reghdfe tcdi_im policy, absorb(city_code month_id) vce(cluster city_code)
post_att, handle(`att_handle') panel("a_full_sample") contribution("1.46%")

preserve
    keep if inlist(cohort, 1, 2) | missing(cohort)
    reghdfe tcdi_wave12 policy, absorb(city_code month_id) vce(cluster city_code)
    post_att, handle(`att_handle') panel("b_waves_1_2") contribution("1.59%")
restore

preserve
    keep if inlist(cohort, 3, 4, 5) | missing(cohort)
    reghdfe tcdi_wave345 policy, absorb(city_code month_id) vce(cluster city_code)
    post_att, handle(`att_handle') panel("c_waves_3_5") contribution("1.59%")
restore

postclose `att_handle'

preserve
    use `att_results', clear
    export delimited using "${OUTPUT}/fig2_att_summary.csv", replace
restore

********************************************************************************
* 8. Finish
********************************************************************************

display as text "Fig. 2 estimates exported to: ${OUTPUT}"
log close

