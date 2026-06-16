********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    10_supplementary_tables4_6_selection_checks.do
* Purpose: Reproduce Supplementary Tables 4-6 on baseline balance and selection
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
log using "${LOGS}/10_supplementary_tables4_6_selection_checks.log", replace text

capture which reghdfe
if _rc {
    display as error "The user-written Stata command reghdfe is required."
    display as error "Install it before running this script: ssc install reghdfe, replace"
    exit 111
}

capture which esttab
if _rc {
    display as error "The user-written Stata package estout is required."
    display as error "Install it before running this script: ssc install estout, replace"
    exit 111
}

capture which estadd
if _rc {
    display as error "The user-written Stata package estout is required."
    display as error "Install it before running this script: ssc install estout, replace"
    exit 111
}

********************************************************************************
* 1. Load annual panel and define the selection-check sample
********************************************************************************

use "${PROCESSED}/yearly_panel_prepared.dta", clear

capture confirm variable analysis_sample
if _rc {
    local partial_city_codes "130200, 320500, 610100, 610400, 520100, 520400, 632800"
    gen byte analysis_sample = !inlist(city_code, `partial_city_codes')
    label variable analysis_sample "Main sample excluding local or partial SCP exposure"
}

local required_vars ///
    city_code city year treat policy analysis_sample ///
    precp temp wind visib humid ///
    light_2014 pop_2014 slope_2014 altitude_2014 ///
    road_area_pc_2014 highway_density_2014

foreach var of local required_vars {
    capture confirm variable `var'
    if _rc {
        display as error "Required variable `var' was not found in yearly_panel_prepared.dta."
        exit 111
    }
}

label variable precp                "Precipitation 2014"
label variable temp                 "Temperature 2014"
label variable wind                 "Wind speed 2014"
label variable visib                "Visibility 2014"
label variable humid                "Relative humidity 2014"
label variable light_2014           "Nighttime light 2014"
label variable pop_2014             "Population density 2014"
label variable slope_2014           "Slope 2014"
label variable altitude_2014        "Elevation 2014"
label variable road_area_pc_2014    "Road area per capita 2014"
label variable highway_density_2014 "Highway-route density 2014"

local main_sample analysis_sample == 1

********************************************************************************
* 2. Supplementary Table 4: baseline balance
********************************************************************************

* Weather variables are taken from the first annual-panel year. Static city
* characteristics and traffic-development variables are measured in 2014. We do
* not impose a common non-missing sample across all variables, because road area
* per capita and highway-route density are unavailable for Yili and Hong Kong.

local balance_vars ///
    precp temp wind visib humid ///
    light_2014 pop_2014 slope_2014 altitude_2014 ///
    road_area_pc_2014 highway_density_2014

tempname s4
tempfile s4_data
postfile `s4' ///
    str60 variable ///
    str20 unit ///
    double n_treated ///
    double n_control ///
    double treated_mean ///
    double control_mean ///
    double difference ///
    double p_value ///
    using "`s4_data'", replace

foreach var of local balance_vars {
    local var_label : variable label `var'
    local var_unit "-"

    if "`var'" == "precp" {
        local var_unit "100mm"
    }
    if "`var'" == "temp" {
        local var_unit "deg C"
    }
    if "`var'" == "wind" {
        local var_unit "m/s"
    }
    if "`var'" == "visib" {
        local var_unit "km"
    }
    if "`var'" == "humid" {
        local var_unit "%"
    }
    if "`var'" == "pop_2014" {
        local var_unit "persons/km2"
    }
    if "`var'" == "slope_2014" {
        local var_unit "degree"
    }
    if "`var'" == "altitude_2014" {
        local var_unit "m"
    }
    if "`var'" == "road_area_pc_2014" {
        local var_unit "m2/person"
    }
    if "`var'" == "highway_density_2014" {
        local var_unit "km/km2"
    }

    quietly summarize `var' if year == 2014 & `main_sample' & treat == 1
    local n_treated = r(N)
    local treated_mean = r(mean)

    quietly summarize `var' if year == 2014 & `main_sample' & treat == 0
    local n_control = r(N)
    local control_mean = r(mean)

    quietly ttest `var' if year == 2014 & `main_sample', by(treat) unequal
    local difference = `treated_mean' - `control_mean'
    local p_value = r(p)

    post `s4' ///
        ("`var_label'") ///
        ("`var_unit'") ///
        (`n_treated') ///
        (`n_control') ///
        (`treated_mean') ///
        (`control_mean') ///
        (`difference') ///
        (`p_value')
}
postclose `s4'

preserve
    use "`s4_data'", clear
    format treated_mean control_mean difference %9.3f
    format p_value %9.4f

    label variable variable      "Variable"
    label variable unit          "Unit"
    label variable n_treated     "N treated"
    label variable n_control     "N control"
    label variable treated_mean  "Treated mean"
    label variable control_mean  "Control mean"
    label variable difference    "Difference"
    label variable p_value       "P value"

    export delimited using "${OUTPUT}/supplementary_tables/supplementary_table4_baseline_balance.csv", ///
        replace
    export excel using "${OUTPUT}/supplementary_tables/supplementary_table4_baseline_balance.xlsx", ///
        firstrow(varlabels) replace
restore

********************************************************************************
* 3. Supplementary Table 5: treatment assignment tests
********************************************************************************

* The table reports OLS and logistic models for the treatment-group indicator.
* Columns (1) and (4) include meteorological variables; columns (2) and (5)
* add geographic variables; columns (3) and (6) add socioeconomic and
* road-transport baseline variables.

preserve
    keep if year == 2014 & `main_sample'

    eststo clear

    quietly reg treat precp temp wind visib humid, vce(cluster city_code)
    eststo s5_ols_1

    quietly reg treat precp temp wind visib humid ///
        slope_2014 altitude_2014, vce(cluster city_code)
    eststo s5_ols_2

    quietly reg treat precp temp wind visib humid ///
        slope_2014 altitude_2014 ///
        light_2014 pop_2014 road_area_pc_2014 highway_density_2014, ///
        vce(cluster city_code)
    eststo s5_ols_3

    quietly logit treat precp temp wind visib humid, vce(cluster city_code)
    eststo s5_logit_1

    quietly logit treat precp temp wind visib humid ///
        slope_2014 altitude_2014, vce(cluster city_code)
    eststo s5_logit_2

    quietly logit treat precp temp wind visib humid ///
        slope_2014 altitude_2014 ///
        light_2014 pop_2014 road_area_pc_2014 highway_density_2014, ///
        vce(cluster city_code)
    eststo s5_logit_3

    esttab s5_ols_1 s5_ols_2 s5_ols_3 s5_logit_1 s5_logit_2 s5_logit_3 ///
        using "${OUTPUT}/supplementary_tables/supplementary_table5_assignment_tests.rtf", ///
        replace rtf label ///
        title("Testing the random assignment of SCP implementation") ///
        mtitles("OLS" "OLS" "OLS" "Logistic" "Logistic" "Logistic") ///
        keep(precp temp wind visib humid slope_2014 altitude_2014 ///
             light_2014 pop_2014 road_area_pc_2014 highway_density_2014) ///
        order(precp temp wind visib humid slope_2014 altitude_2014 ///
              light_2014 pop_2014 road_area_pc_2014 highway_density_2014) ///
        b(%9.4f) se(%9.4f) ///
        star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2 r2_p, ///
              labels("Observations" "R-squared" "Pseudo R-squared") ///
              fmt(%9.0f %9.4f %9.4f)) ///
        compress nonotes

    esttab s5_ols_1 s5_ols_2 s5_ols_3 s5_logit_1 s5_logit_2 s5_logit_3 ///
        using "${OUTPUT}/supplementary_tables/supplementary_table5_assignment_tests.csv", ///
        replace csv label ///
        mtitles("OLS" "OLS" "OLS" "Logistic" "Logistic" "Logistic") ///
        keep(precp temp wind visib humid slope_2014 altitude_2014 ///
             light_2014 pop_2014 road_area_pc_2014 highway_density_2014) ///
        order(precp temp wind visib humid slope_2014 altitude_2014 ///
              light_2014 pop_2014 road_area_pc_2014 highway_density_2014) ///
        b(%9.4f) se(%9.4f) ///
        star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2 r2_p, ///
              labels("Observations" "R-squared" "Pseudo R-squared") ///
              fmt(%9.0f %9.4f %9.4f)) ///
        compress nonotes
restore

********************************************************************************
* 4. Supplementary Table 6: policy timing tests
********************************************************************************

* This table tests whether the timing of SCP implementation is associated with
* lagged city-year covariates. All explanatory variables are measured at t-1.

xtset city_code year

capture drop ///
    precp_l1 temp_l1 wind_l1 visib_l1 humid_l1 ///
    light_l1 pop_l1 road_area_pc_l1 highway_density_l1

gen double precp_l1           = L.precp
gen double temp_l1            = L.temp
gen double wind_l1            = L.wind
gen double visib_l1           = L.visib
gen double humid_l1           = L.humid
gen double light_l1           = L.light
gen double pop_l1             = L.pop
gen double road_area_pc_l1    = L.road_area_pc
gen double highway_density_l1 = L.highway_density

label variable precp_l1           "Precipitation t-1"
label variable temp_l1            "Temperature t-1"
label variable wind_l1            "Wind speed t-1"
label variable visib_l1           "Visibility t-1"
label variable humid_l1           "Relative humidity t-1"
label variable light_l1           "Nighttime light t-1"
label variable pop_l1             "Population density t-1"
label variable road_area_pc_l1    "Road area per capita t-1"
label variable highway_density_l1 "Highway-route density t-1"

eststo clear

* (1) Meteorological lagged covariates
quietly reghdfe policy ///
    precp_l1 temp_l1 wind_l1 visib_l1 humid_l1 ///
    if `main_sample', absorb(city_code year) vce(cluster city_code)
estadd local year_fe "Yes"
estadd local city_fe "Yes"
eststo s6_1

* (2) Meteorological + socioeconomic lagged covariates
quietly reghdfe policy ///
    precp_l1 temp_l1 wind_l1 visib_l1 humid_l1 ///
    light_l1 pop_l1 ///
    if `main_sample', absorb(city_code year) vce(cluster city_code)
estadd local year_fe "Yes"
estadd local city_fe "Yes"
eststo s6_2

* (3) Meteorological + socioeconomic + road-transport lagged covariates
quietly reghdfe policy ///
    precp_l1 temp_l1 wind_l1 visib_l1 humid_l1 ///
    light_l1 pop_l1 road_area_pc_l1 highway_density_l1 ///
    if `main_sample', absorb(city_code year) vce(cluster city_code)
estadd local year_fe "Yes"
estadd local city_fe "Yes"
eststo s6_3

esttab s6_1 s6_2 s6_3 ///
    using "${OUTPUT}/supplementary_tables/supplementary_table6_policy_timing_tests.rtf", ///
    replace rtf label ///
    title("Further tests on the random timing of SCP implementation") ///
    mtitles("(1)" "(2)" "(3)") ///
    keep(precp_l1 temp_l1 wind_l1 visib_l1 humid_l1 ///
         light_l1 pop_l1 road_area_pc_l1 highway_density_l1) ///
    order(precp_l1 temp_l1 wind_l1 visib_l1 humid_l1 ///
          light_l1 pop_l1 road_area_pc_l1 highway_density_l1) ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(year_fe city_fe N r2_a, ///
          labels("Year FE" "City FE" "Observations" "Adj. R-squared") ///
          fmt(%9s %9s %9.0f %9.4f)) ///
    compress nonotes

log close
