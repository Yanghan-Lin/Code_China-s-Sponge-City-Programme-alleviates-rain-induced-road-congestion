********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    07_supplementary_figure8_topography_light.do
* Purpose: Draw Supplementary Fig. 8, relating topographic characteristics to
*          nighttime light intensity
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off

********************************************************************************
* 0. User paths
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
log using "${LOGS}/07_supplementary_figure8_topography_light.log", replace text

graph set window fontface "Helvetica"
set scheme s1color

********************************************************************************
* 1. Load city-level baseline variables
********************************************************************************

use "${PROCESSED}/yearly_panel_prepared.dta", clear

keep city_code city light_2014 slope_2014 altitude_2014
duplicates drop city_code, force
drop if missing(light_2014) | missing(slope_2014) | missing(altitude_2014)

gen double altitude_100m = altitude_2014 / 100
label variable light_2014   "Nighttime light intensity"
label variable slope_2014   "Slope"
label variable altitude_100m "Elevation"

tempfile fig8_data
save `fig8_data', replace

********************************************************************************
* 2. Panel a: slope and nighttime light intensity
********************************************************************************

use `fig8_data', clear

reg light_2014 slope_2014
local r2_slope : display %4.3f e(r2)
scalar p_slope = 2 * ttail(e(df_r), abs(_b[slope_2014] / _se[slope_2014]))
local p_slope_text "P < 0.001"
if p_slope >= 0.001 {
    local p_slope_value : display %4.3f p_slope
    local p_slope_text "P = `p_slope_value'"
}

predict double yhat_slope, xb
predict double se_slope, stdp
gen double ci_lo_slope = yhat_slope - invttail(e(df_r), 0.025) * se_slope
gen double ci_hi_slope = yhat_slope + invttail(e(df_r), 0.025) * se_slope
sort slope_2014

twoway ///
    (rarea ci_hi_slope ci_lo_slope slope_2014, sort color(red%18) lcolor(none)) ///
    (line yhat_slope slope_2014, sort lcolor(red) lwidth(medthin)) ///
    (scatter light_2014 slope_2014, msymbol(O) msize(small) mcolor(navy%55)), ///
    title("a", position(11) ring(0) size(medium)) ///
    xtitle("Slope (degrees)") ///
    ytitle("Nighttime light intensity") ///
    note("R-squared = `r2_slope'; `p_slope_text'", size(vsmall)) ///
    legend(off) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(fig8a_slope, replace)

graph save "${OUTPUT}/supplementary_figure8a_slope_light.gph", replace
graph export "${OUTPUT}/supplementary_figure8a_slope_light.png", ///
    as(png) replace width(1800)

********************************************************************************
* 3. Panel b: elevation and nighttime light intensity
********************************************************************************

use `fig8_data', clear

reg light_2014 altitude_100m
local r2_altitude : display %4.3f e(r2)
scalar p_altitude = 2 * ttail(e(df_r), abs(_b[altitude_100m] / _se[altitude_100m]))
local p_altitude_text "P < 0.001"
if p_altitude >= 0.001 {
    local p_altitude_value : display %4.3f p_altitude
    local p_altitude_text "P = `p_altitude_value'"
}

predict double yhat_altitude, xb
predict double se_altitude, stdp
gen double ci_lo_altitude = yhat_altitude - invttail(e(df_r), 0.025) * se_altitude
gen double ci_hi_altitude = yhat_altitude + invttail(e(df_r), 0.025) * se_altitude
sort altitude_100m

twoway ///
    (rarea ci_hi_altitude ci_lo_altitude altitude_100m, sort color(red%18) lcolor(none)) ///
    (line yhat_altitude altitude_100m, sort lcolor(red) lwidth(medthin)) ///
    (scatter light_2014 altitude_100m, msymbol(O) msize(small) mcolor(navy%55)), ///
    title("b", position(11) ring(0) size(medium)) ///
    xtitle("Elevation (100 m)") ///
    ytitle("Nighttime light intensity") ///
    note("R-squared = `r2_altitude'; `p_altitude_text'", size(vsmall)) ///
    legend(off) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(fig8b_altitude, replace)

graph save "${OUTPUT}/supplementary_figure8b_elevation_light.gph", replace
graph export "${OUTPUT}/supplementary_figure8b_elevation_light.png", ///
    as(png) replace width(1800)

********************************************************************************
* 4. Combine panels
********************************************************************************

graph combine fig8a_slope fig8b_altitude, ///
    cols(2) imargin(tiny) graphregion(color(white))

graph save "${OUTPUT}/supplementary_figure8_topography_light.gph", replace
graph export "${OUTPUT}/supplementary_figure8_topography_light.png", ///
    as(png) replace width(3000)
graph export "${OUTPUT}/supplementary_figure8_topography_light.pdf", ///
    as(pdf) replace

log close
