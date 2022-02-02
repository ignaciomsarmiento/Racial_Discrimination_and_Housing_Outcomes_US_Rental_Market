/*
Replication files  
*/

clear all


*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"




use "../../Oportunity_Data/table_1-2.dta"


local psize 0.5

ds kfr*
/* foreach var in `r(varlist)' {
	replace `var' = 100 * `var'
}
 */
* Set races
local races white black  hisp 

* Loop over all and native
foreach kfr_var in kfr  {

	* Get slopes and intercept terms
	foreach race in `races' {
		reg `kfr_var'_`race'_pooled par_pctile
		local slope_`race' : di %4.2f _b[par_pctile]
		local intercept_`race' : di %4.2f _b[_cons]
	}

	tw ///
		(scatter `kfr_var'_white_pooled par_pctile, msize(*`psize') mcolor(navy) msymbol($sym_white)) ///
		(scatter `kfr_var'_black_pooled par_pctile, msize(*`psize') mcolor(maroon) msymbol($sym_black)) ///
		(scatter `kfr_var'_hisp_pooled par_pctile, msize(*`psize') mcolor(dkorange) msymbol($sym_hisp)) ///
		(lfit `kfr_var'_white_pooled par_pctile, lcolor(navy)) ///
		(lfit `kfr_var'_black_pooled par_pctile, lcolor(maroon)) ///
		(lfit `kfr_var'_hisp_pooled par_pctile, lcolor(dkorange)) ///
		,xtitle("Parent Household Income Rank", color(black)) ///
		ytitle("Mean Child Household Income Rank", color(black) margin(t=3)) ///
		ylabel(20(20)80, gmax tlcolor(black) labcolor(black)) ///
		xlabel(0(20)100, tlcolor(black) labcolor(black)) plotregion(margin(zero)) ///
		legend(order	( ///
						 1 "White (Intercept: `intercept_white'; Slope: `slope_white')" ///
						 2 "Black (Intercept: `intercept_black'; Slope: `slope_black')" ///
						 3 "Hispanic (Intercept: `intercept_hisp'; Slope: `slope_hisp')" ///
						 ) position(5) cols(1) ring(0) size(vsmall) bm(b=3) color(black)) ///
		title(${title_size}) ///
		xscale(range(0 101) lcolor(black)) ///
		yscale(lcolor(black))
	graph export "views/bin_`kfr_var'_par_rank_all_race.pdf", as(pdf) name("Graph") replace
}
