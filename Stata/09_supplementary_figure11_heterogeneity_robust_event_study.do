********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    09_supplementary_figure11_heterogeneity_robust_event_study.do
* Purpose: Draw Supplementary Fig. 11 using heterogeneity-robust event-study
*          estimators for staggered DID
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off
set maxvar 32767

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
log using "${LOGS}/09_supplementary_figure11_heterogeneity_robust_event_study.log", replace text

foreach cmd in reghdfe did_imputation csdid eventstudyinteract event_plot {
    capture which `cmd'
    if _rc {
        display as error "The user-written Stata command `cmd' is required."
        exit 111
    }
}

graph set window fontface "Helvetica"
set scheme s1color

********************************************************************************
* 1. Load monthly panel and normalize treatment timing
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear
keep if analysis_sample == 1

gen double t_month = month_id
replace t_month = mofd(month_id) if month_id > 5000 & !missing(month_id)

gen double cohort_for_est = cohort_month
replace cohort_for_est = mofd(cohort_month) if ///
    cohort_month > 5000 & !missing(cohort_month)

format t_month cohort_for_est %tm

gen double first_treat = cohort_for_est
replace first_treat = 0 if missing(cohort_for_est)

gen double rel_month = t_month - cohort_for_est if treat == 1
gen citycode = city_code

********************************************************************************
* 2. Borusyak et al. imputation estimator
********************************************************************************

did_imputation tcdi_im city_code t_month cohort_for_est, ///
    autosample fe(city_code t_month) cluster(city_code) ///
    pretrends(36) horizons(0/36)

estimates store imput_tcdi_im
matrix imput_tcdi_im_b = e(b)
matrix imput_tcdi_im_v = e(V)

********************************************************************************
* 3. Callaway and Sant'Anna group-time ATT estimator
********************************************************************************

csdid tcdi_im, i(city_code) t(t_month) gvar(first_treat) ///
    method(drimp) agg(event) cluster(citycode) long2
estat event, window(-36 36) estore(cs_tcdi_im)

********************************************************************************
* 4. Sun and Abraham interaction-weighted estimator
********************************************************************************

gen F36event = (rel_month <= -36 & treat == 1)
forvalues i = 35(-1)2 {
    gen F`i'event = (rel_month == -`i' & treat == 1)
}
forvalues i = 0/36 {
    gen L`i'event = (rel_month == `i' & treat == 1)
}
replace L36event = (rel_month >= 36 & treat == 1)

gen control_cohort = missing(cohort_for_est)

eventstudyinteract tcdi_im F*event L*event, ///
    cohort(cohort_for_est) control_cohort(control_cohort) ///
    absorb(city_code t_month) vce(cluster city_code)

estimates store sa_tcdi_im
matrix sa_tcdi_im_b = e(b_iw)
matrix sa_tcdi_im_v = e(V_iw)

********************************************************************************
* 5. Conventional TWFE event-study estimator
********************************************************************************

gen M_36 = (rel_month <= -36 & treat == 1)
forvalues i = 35(-1)2 {
    gen M_`i' = (rel_month == -`i' & treat == 1)
}
forvalues j = 0/35 {
    gen N_`j' = (rel_month == `j' & treat == 1)
}
gen N_36 = (rel_month >= 36 & treat == 1)

reghdfe tcdi_im M_* N_*, absorb(city_code t_month) vce(cluster city_code)
estimates store reghdfe_tcdi_im

********************************************************************************
* 6. Plot the four estimators together
********************************************************************************

event_plot ///
    reghdfe_tcdi_im ///
    imput_tcdi_im_b#imput_tcdi_im_v ///
    cs_tcdi_im ///
    sa_tcdi_im_b#sa_tcdi_im_v, ///
    stub_lag(N_# tau# Tp# L#event) ///
    stub_lead(M_# pre# Tm# F#event) ///
    plottype(scatter) ciplottype(rcap) trimlead(36) trimlag(36) ///
    together perturb(-0.3(0.15)0.3) noautolegend ///
    graph_opt( ///
        xtitle("Relative year-month") ytitle("SCP effects on TCDI") ///
        xlabel(-36(3)36) ylabel(-0.1(0.05)0.1, angle(horizontal)) ///
        legend(order(1 "TWFE" 3 "Borusyak et al. (2024)" ///
                     5 "Callaway & Sant'Anna (2021)" ///
                     7 "Sun & Abraham (2021)") ///
               size(*0.5) ring(0) rows(3) pos(8)) ///
        xline(-1, lcolor(gs8) lpattern(dash)) ///
        yline(0, lcolor(gs8) lpattern(dash)) ///
        graphregion(color(white)) bgcolor(white) ///
    ) ///
    lag_opt1(msymbol(O) color("213 94 0") msize(vsmall)) ///
    lag_ci_opt1(color("213 94 0") lwidth(vthin)) ///
    lag_opt2(msymbol(D) color("0 158 115") msize(vsmall)) ///
    lag_ci_opt2(color("0 158 115") lwidth(vthin)) ///
    lag_opt3(msymbol(S) color("0 114 178") msize(vsmall)) ///
    lag_ci_opt3(color("0 114 178") lwidth(vthin)) ///
    lag_opt4(msymbol(T) color("230 159 0") msize(vsmall)) ///
    lag_ci_opt4(color("230 159 0") lwidth(vthin))

graph save "${OUTPUT}/supplementary_figure11_heterogeneity_robust_event_study.gph", replace
graph export "${OUTPUT}/supplementary_figure11_heterogeneity_robust_event_study.png", ///
    as(png) replace width(3000)
graph export "${OUTPUT}/supplementary_figure11_heterogeneity_robust_event_study.pdf", ///
    as(pdf) replace

log close
