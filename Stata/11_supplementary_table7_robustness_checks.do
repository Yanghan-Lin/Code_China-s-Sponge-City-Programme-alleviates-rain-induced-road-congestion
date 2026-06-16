********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    11_supplementary_table7_robustness_checks.do
* Purpose: Reproduce Supplementary Table 7 robustness checks
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
capture mkdir "${OUTPUT}/supplementary_tables"

capture log close _all
log using "${LOGS}/11_supplementary_table7_robustness_checks.log", replace text

foreach cmd in reghdfe esttab estadd did_imputation csdid eventstudyinteract {
    capture which `cmd'
    if _rc {
        display as error "The user-written Stata command `cmd' is required."
        exit 111
    }
}

********************************************************************************
* 1. Helper for storing an average post-treatment ATT from event-study matrices
********************************************************************************

capture program drop store_average_att
program define store_average_att, eclass
    syntax , BMAT(name) VMAT(name) STUB(string) FIRST(integer) LAST(integer) NOBS(integer)

    tempname b_work v_work c_work b_att v_att b_post v_post
    matrix `b_work' = `bmat'
    matrix `v_work' = `vmat'

    local ncoef = colsof(`b_work')
    matrix `c_work' = J(1, `ncoef', 0)

    local used = 0
    forvalues h = `first'/`last' {
        local cname = subinstr("`stub'", "#", "`h'", .)
        local pos = colnumb(`b_work', "`cname'")
        if `pos' < . {
            matrix `c_work'[1, `pos'] = 1
            local used = `used' + 1
        }
    }

    if `used' == 0 {
        display as error "No post-treatment coefficients were found for stub `stub'."
        exit 111
    }

    matrix `c_work' = `c_work' / `used'
    matrix `b_att' = `c_work' * `b_work''
    matrix `v_att' = `c_work' * `v_work' * `c_work''

    scalar __att = `b_att'[1,1]
    scalar __var = `v_att'[1,1]

    matrix `b_post' = (__att)
    matrix colnames `b_post' = policy

    matrix `v_post' = (__var)
    matrix rownames `v_post' = policy
    matrix colnames `v_post' = policy

    ereturn post `b_post' `v_post', obs(`nobs')
    ereturn scalar N = `nobs'
    ereturn local cmd "average_event_att"
end

********************************************************************************
* 2. Monthly robustness specifications
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
}

local MAIN_SAMPLE analysis_sample == 1

* Control-group cities with documented local or provincial SCP-related pilots.
capture drop local_scp_control
gen byte local_scp_control = 0
local LOCAL_SCP_CONTROL_CODES ///
    130100 ///
    140100 ///
    220100 ///
    320100 ///
    320300 ///
    320400 ///
    320600 ///
    320700 ///
    321200 ///
    330600 ///
    331000 ///
    340100 ///
    341100 ///
    350500 ///
    360700 ///
    370800 ///
    370900 ///
    410100 ///
    410300 ///
    430100 ///
    440700 ///
    440900 ///
    441900 ///
    510100 ///
    511300

foreach code of local LOCAL_SCP_CONTROL_CODES {
    replace local_scp_control = 1 if treat == 0 & city_code == `code'
}

eststo clear

* (1) Baseline
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_baseline

* (2) Excluding COVID-19 years
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE' & !inlist(year, 2020, 2021, 2022), ///
    absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_covid

* (3) Excluding local or provincial SCP pilots from the control group
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE' & local_scp_control == 0, ///
    absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_local_scp

* (4) Controlling for concurrent climate-adaptation policies
quietly reghdfe tcdi_im policy climate_policy ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_confounding

* (5) Accounting for non-random policy selection
quietly reghdfe tcdi_im ///
    policy ///
    c.precp2014_sum#i.year c.snow2014_sum#i.year ///
    c.temp2014_avg#i.year c.humid2014_avg#i.year ///
    c.visib2014_avg#i.year c.wind2014_avg#i.year ///
    c.light_2014#i.year c.pop_2014#i.year ///
    c.altitude_2014#i.year c.slope_2014#i.year ///
    c.road_area_pc_2014#i.year c.highway_density_2014#i.year ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
estadd local pred_year_fe "Yes"
eststo S7_nonrandom

* (6) Stricter traffic and transport controls
quietly reghdfe tcdi_im ///
    policy ln_taxi_panel ln_bus_panel ln_private_car_panel ///
    ln_public_transport_inv ln_road_bridge_inv ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_stricter_controls

* (7) Stricter fixed effects
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE', absorb(city_code month_id city_code#month) vce(cluster city_code)
estadd local city_month_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_stricter_fe

* (8) City-by-year clustered standard errors
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster city_code year)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_cityyear_se

* (9) Province clustered standard errors
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster province_code)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_province_se

* (10) Province-by-year clustered standard errors
quietly reghdfe tcdi_im policy ///
    if `MAIN_SAMPLE', absorb(city_code month_id) vce(cluster province_code year)
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_provinceyear_se

********************************************************************************
* 3. Weekly and daily temporal-aggregation checks
********************************************************************************

use "${PROCESSED}/weekly_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
}

capture confirm variable year
if _rc {
    gen int year = yofd(dofw(week_id))
}

quietly reghdfe tcdi_im policy ///
    if analysis_sample == 1, ///
    absorb(city_code#month week_id) vce(cluster city_code)
estadd local city_fe "Yes"
estadd local city_month_fe "Yes"
estadd local year_week_fe "Yes"
eststo S7_weekly

use "${PROCESSED}/daily_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
}

quietly reghdfe tcdi_im policy ///
    if analysis_sample == 1, ///
    absorb(date city_code#month city_code#weekend) vce(cluster city_code)
estadd local city_month_fe "Yes"
estadd local date_fe "Yes"
estadd local city_weekend_fe "Yes"
eststo S7_daily

********************************************************************************
* 4. Heterogeneity-robust DID ATT estimates
********************************************************************************

use "${PROCESSED}/monthly_panel_prepared.dta", clear
keep if analysis_sample == 1
quietly count
local n_monthly = r(N)

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

* Sun and Abraham interaction-weighted estimator
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

matrix sa_b = e(b_iw)
matrix sa_v = e(V_iw)
store_average_att, bmat(sa_b) vmat(sa_v) stub("L#event") first(0) last(36) nobs(`n_monthly')
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_sun_abraham

* Borusyak et al. imputation estimator
did_imputation tcdi_im city_code t_month cohort_for_est, ///
    autosample fe(city_code t_month) cluster(city_code) ///
    pretrends(36) horizons(0/36)

matrix im_b = e(b)
matrix im_v = e(V)
store_average_att, bmat(im_b) vmat(im_v) stub("tau#") first(0) last(36) nobs(`n_monthly')
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_borusyak

* Callaway and Sant'Anna group-time ATT estimator
csdid tcdi_im, i(city_code) t(t_month) gvar(first_treat) ///
    method(drimp) agg(event) cluster(citycode) long2
estat event, window(-36 36) estore(cs_event)

estimates restore cs_event
matrix cs_b = e(b)
matrix cs_v = e(V)
store_average_att, bmat(cs_b) vmat(cs_v) stub("Tp#") first(0) last(36) nobs(`n_monthly')
estadd local city_fe "Yes"
estadd local ym_fe "Yes"
eststo S7_callaway_santanna

********************************************************************************
* 5. Export Supplementary Table 7
********************************************************************************

esttab ///
    S7_baseline S7_covid S7_local_scp S7_confounding ///
    S7_nonrandom S7_stricter_controls S7_stricter_fe ///
    S7_cityyear_se S7_province_se S7_provinceyear_se ///
    S7_weekly S7_daily S7_sun_abraham S7_borusyak S7_callaway_santanna ///
    using "${OUTPUT}/supplementary_tables/supplementary_table7_robustness_checks.rtf", ///
    replace rtf label ///
    title("Robustness checks for the baseline result") ///
    mtitles("Baseline" "Excluding COVID-19" "Excluding local SCP pilots" ///
            "Confounding policies" "Accounting for non-random policy selection" ///
            "Stricter controls" "Stricter fixed effects" "City-year SE" ///
            "Province SE" "Province-year SE" "Weekly data" "Daily data" ///
            "Sun & Abraham (2021)" "Borusyak et al. (2024)" ///
            "Callaway & Sant'Anna (2021)") ///
    keep(policy climate_policy ln_taxi_panel ln_bus_panel ln_private_car_panel ///
         ln_public_transport_inv ln_road_bridge_inv) ///
    order(policy climate_policy ln_taxi_panel ln_bus_panel ln_private_car_panel ///
          ln_public_transport_inv ln_road_bridge_inv) ///
    coeflabels(policy "SCP" ///
               climate_policy "CRCC" ///
               ln_taxi_panel "log(Taxis)" ///
               ln_bus_panel "log(Public buses)" ///
               ln_private_car_panel "log(Private cars)" ///
               ln_public_transport_inv "log(Public transport investment)" ///
               ln_road_bridge_inv "log(Road and bridge investment)") ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(city_fe ym_fe city_month_fe pred_year_fe year_week_fe date_fe city_weekend_fe N r2_a, ///
          labels("City FE" "Year-month FE" "City-month FE" ///
                 "Predetermined x Year FE" "Year-week FE" "Date FE" ///
                 "City-weekend FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

esttab ///
    S7_baseline S7_covid S7_local_scp S7_confounding ///
    S7_nonrandom S7_stricter_controls S7_stricter_fe ///
    S7_cityyear_se S7_province_se S7_provinceyear_se ///
    S7_weekly S7_daily S7_sun_abraham S7_borusyak S7_callaway_santanna ///
    using "${OUTPUT}/supplementary_tables/supplementary_table7_robustness_checks.csv", ///
    replace csv label ///
    mtitles("Baseline" "Excluding COVID-19" "Excluding local SCP pilots" ///
            "Confounding policies" "Accounting for non-random policy selection" ///
            "Stricter controls" "Stricter fixed effects" "City-year SE" ///
            "Province SE" "Province-year SE" "Weekly data" "Daily data" ///
            "Sun & Abraham (2021)" "Borusyak et al. (2024)" ///
            "Callaway & Sant'Anna (2021)") ///
    keep(policy climate_policy ln_taxi_panel ln_bus_panel ln_private_car_panel ///
         ln_public_transport_inv ln_road_bridge_inv) ///
    order(policy climate_policy ln_taxi_panel ln_bus_panel ln_private_car_panel ///
          ln_public_transport_inv ln_road_bridge_inv) ///
    coeflabels(policy "SCP" ///
               climate_policy "CRCC" ///
               ln_taxi_panel "log(Taxis)" ///
               ln_bus_panel "log(Public buses)" ///
               ln_private_car_panel "log(Private cars)" ///
               ln_public_transport_inv "log(Public transport investment)" ///
               ln_road_bridge_inv "log(Road and bridge investment)") ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(city_fe ym_fe city_month_fe pred_year_fe year_week_fe date_fe city_weekend_fe N r2_a, ///
          labels("City FE" "Year-month FE" "City-month FE" ///
                 "Predetermined x Year FE" "Year-week FE" "Date FE" ///
                 "City-weekend FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9s %9s %9s %9s %9s %9.0f %9.4f)) ///
    compress nonotes

log close
