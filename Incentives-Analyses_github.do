********************************************************************************
* Effects of changing the incentive strategy on panel performance:
* Andreas Ette, Jean Décieux, Sabine Zinn, 1 March 2025
********************************************************************************

********************************************************************************
* Preparation of data
* Merging information about response and consent from wave 1 and 2
********************************************************************************
use "...\SUF\ukr-pbrutto.dta", clear
keep  pid paapor alter sex incentive gkpol familienstand zuzug gkz syear panel
reshape wide paapor alter sex incentive gkpol familienstand zuzug gkz panel, i(pid) j(syear)
drop zuzug2023 sex2023 incentive2023 gkz2023 gkpol2023 familienstand2023 alter2023 panel2023
rename alter2022 age
label variable age "age"
rename paapor2022 paaporw1
label variable paaporw1 "AAPOR (w1)"
rename paapor2023 paaporw2
label variable paaporw2 "AAPOR (w2)"
rename familienstand2022 familienstand
rename sex2022 sex
label variable sex "sex"
rename zuzug2022 zuzug
label variable zuzug "zuzugsdatum"
rename incentive2022 incentive
rename panel2022 panel
label variable panel "panel consent"
rename gkpol2022 gkpolsuf
rename gkz2022 gkzsuf
save "$OUTPUT\bruttow1w2.dta", replace


********************************************************************************
**# Bookmark #1
* Dependent variables
********************************************************************************
* Response: Wave 1
use "$OUTPUT\bruttow1w2.dta", clear
fre paaporw1
recode paaporw1 (1100 = 1 "participation w1") (else = 0 "no participation w1"), gen(tw1)
label variable tw1 "response w1"
tab paaporw1 tw1, m

recode paaporw1 (1100 = 1 "participation w1") (else = 0 "no participation w1"), gen(tw1_34)
replace tw1_34 = . if paaporw1 == 4100 | paaporw1 == 4200 | paaporw1 == 3180
label variable tw1_34 "response w1 (without uneligible and unknown eligibility)"
tab paaporw1 tw1_34, m

* Panelconsent: Wave 1
fre panel
gen tw1c = tw1
replace tw1c = 0 if panel == 2
label variable tw1c "response w1 (without consent)"
fre tw1c

gen tw1c_34 = tw1c
replace tw1c_34 = . if tw1_34 == .
label variable tw1c_34 "response w1 (without uneligible and unknown eligibility)" 
fre tw1c_34

gen tw1c_part = 0 if tw1_34 == 1
replace tw1c_part = 1 if paaporw2 != .
label variable tw1c_part "panel consent w1"
fre tw1c_part

* Response: Wave 2
fre paaporw2
recode paaporw2 (1100 = 1 "participation w2") (2111/4300 = 0 "no participation w2"), gen(tw2)
label variable tw2 "participation w2 (without consent)"
fre tw2

gen tw2consent = .
replace tw2consent = 0 if tw1c_part == 1
replace tw2consent = 1 if paaporw2 == 1100
label variable tw2consent "participation w2 (with consent)"
fre tw2consent

* Overall response
gen toverall = 0
replace toverall = . if tw1_34 == .
replace toverall = 1 if tw2 == 1
label variable toverall "participation overall"
fre toverall
save "$OUTPUT\bruttow1w2.dta", replace


********************************************************************************
**# Bookmark #2
* Independent variables
********************************************************************************
use "$OUTPUT\bruttow1w2.dta", clear

* Treatment: incentive change
label variable incentive "incentive change"
recode incentive (2=0)
label define incentive ///
0 "Control" ///
1 "Treated", modify
fre incentive

* Age
fre age
egen age10 = cut(age), at(0,30,40,50,60,71)
replace age10 = 6 if age == -1
recode age10 (0=1) (30=2) (40=3) (50=4) (60=5)
label define age10 ///
1 "18-29" ///
2 "30-39" ///
3 "40-49" ///
4 "50-59" ///
5 "60-70" ///
6 "unknown", modify
label values age10 age10
label variable age10 "age"
tab age age10, m

* Municipality
fre gkpol
recode gkpol (4=5)
label define gkpol ///
5 "<100k" ///
6 "100-500k" ///
7 ">500k", modify
label values gkpol gkpol
fre gkpol

* Marital status
fre familienstand
gen marriage = familienstand
recode marriage (-2/-1=6) (2=3) (1=2)
label define marriage ///
2 "single" ///
3 "married" ///
4 "divorced" ///
5 "widowed" ///
6 "unknown", modify
label values marriage marriage
label variable marriage "marital status"
tab familienstand marriage, m
drop familienstand

* Duration of stay
fre zuzug
recode zuzug (-8=.)
gen durationstayd = td(25aug2022) - zuzug
gen durationstaym = durationstayd/30.4
gen durationstaym1 = 6 if durationstaym == .
replace durationstaym1 = 5 if durationstaym >= 5 & durationstaym < 6
replace durationstaym1 = 4 if durationstaym >= 4 & durationstaym < 5
replace durationstaym1 = 3 if durationstaym >= 3 & durationstaym < 4
replace durationstaym1 = 2 if durationstaym >= 0 & durationstaym < 3
label define durationstaym1 ///
2 "< 3" ///
3 "3" ///
4 "4" ///
5 ">= 5" ///
6 "unknown", modify
label values durationstaym1 durationstaym1
tab durationstayd durationstaym1, m 

* Sex
fre sex
replace sex=3 if sex==-1
label define sex ///
1 "male" ///
2 "female" ///
3 "unknown", modify
label values sex sex 
tab sex, m

save "$OUTPUT\bruttow1w2.dta", replace


********************************************************************************
**# Bookmark #3
* Figure 1
********************************************************************************
use "$OUTPUT\bruttow1w2.dta", clear
	
* Descriptives: Participation rate wave 1
tab tw1_34 incentive, col chi V

* Descriptives: Panel consent wave 1
tab tw1c_34 incentive, col chi V

* Descriptives: Participation rate wave 2
tab toverall incentive, col chi V	
	
logit tw1_34 i.incentive
estpost margins (incentive)
eststo tw1_34

logit tw1c_part i.incentive
estpost margins (incentive)
eststo tw1c_part

logit tw2consent i.incentive
estpost margins (incentive)
eststo tw2consent

logit toverall i.incentive
estpost margins (incentive)
eststo toverall

esttab 	tw1_34 tw1c_part tw2consent toverall ///
		using "$RESULTS\Fig_UKR_Response.csv", ///
		replace label nodepvar modelwidth(5) plain ///
		cells("b(fmt(4)) ci(fmt(4))") ///
		collabels(Response CI) compress nonum

clear all		
import delimited "$RESULTS\Fig_UKR_Response.csv"	
drop if v1 == "Observations"
rename v2 tw1_34p
rename v4 tw1c_partp
rename v6 tw2consent_p
rename v8 toverall_p
drop in 1/2

split v3, p(,)
rename v31 tw1_34l
rename v32 tw1_34u

split v5, p(,)
rename v51 tw1c_partl
rename v52 tw1c_partu

split v7, p(,)
rename v71 tw2consentl
rename v72 tw2consentu

split v9, p(,)
rename v91 toverall_l
rename v92 toverall_u

drop v3 v5 v7 v9

destring tw1_34p, replace
destring tw1_34l, replace
destring tw1_34u, replace

destring tw1c_partp, replace
destring tw1c_partl, replace
destring tw1c_partu, replace

destring tw2consent_p, replace
destring tw2consentl, replace
destring tw2consentu, replace

destring toverall_p, replace
destring toverall_l, replace
destring toverall_u, replace

encode v1, gen(incentive)

drop v1

order incentive tw1_34p tw1_34l tw1_34u tw1c_partp tw1c_partl tw1c_partu ///
tw2consent_p tw2consentl tw2consentu toverall_p toverall_l toverall_u

replace tw1_34p = tw1_34p*100
replace tw1_34l = tw1_34l*100
replace tw1_34u = tw1_34u*100
replace tw1c_partp = tw1c_partp*100
replace tw1c_partl = tw1c_partl*100
replace tw1c_partu = tw1c_partu*100
replace tw2consent_p = tw2consent_p*100
replace tw2consentl = tw2consentl*100
replace tw2consentu = tw2consentu*100
replace toverall_p = toverall_p*100
replace toverall_l = toverall_l*100
replace toverall_u = toverall_u*100

format tw1_34p tw1_34l tw1_34u tw1c_partp tw1c_partl tw1c_partu ///
tw2consent_p tw2consentl tw2consentu toverall_p toverall_l toverall_u %8.0f

set scheme stcolor

twoway bar tw1_34p incentive, barw(0.6) bfcolor(gs5*0.2) blcolor(gs5*0.2)  ///
yscale(range(0 1)) ylabel(0 "0" 20 "20" 40 "40" 60 "60" 80 "80" 100 "100", glcolor(gs8)) ///
xlabel(1 "Control" 2 "Treated", nogrid) xtitle("N = 37,741") legend(off) mlabel(tw1_34p) mlabcolor(black) mlabsize(medsmall) mlabposition(6) mlabgap(3) graphregion(color(white)) ///
|| rcap tw1_34l tw1_34u incentive, lcolor(gs2) ///
title("Response, W1", size(medsmall)) ///
saving(wave1, replace)

twoway bar tw1c_partp incentive, barw(0.6) bfcolor(gs5*0.2) blcolor(gs5*0.2)  ///
yscale(range(0 1) lstyle(none))  ylabel(0 "" 20 "" 40 "" 60 "" 80 "" 100 "", glcolor(gs8) tstyle(none)) ///
xlabel(1 "Control" 2 "Treated", nogrid) xtitle("N = 11,754") legend(off) mlabel(tw1c_partp) mlabcolor(black) mlabsize(medsmall) mlabposition(6) mlabgap(3) graphregion(color(white)) ///
|| rcap tw1c_partl tw1c_partu incentive, lcolor(gs2) ///
title("Consent, W1", size(medsmall)) ///
saving(panelconsent, replace)

twoway bar tw2consent_p incentive, barw(0.6) bfcolor(gs5*0.2) blcolor(gs5*0.2)  ///
yscale(range(0 1) lstyle(none))  ylabel(0 "" 20 "" 40 "" 60 "" 80 "" 100 "", glcolor(gs8) tstyle(none)) ///
xlabel(1 "Control" 2 "Treated", nogrid) xtitle("N = 10,394") legend(off) mlabel(tw2consent_p) mlabcolor(black) mlabsize(medsmall) mlabposition(6) mlabgap(3) graphregion(color(white)) ///
|| rcap tw2consentl tw2consentu incentive, lcolor(gs2) ///
title("Response, W2", size(medsmall)) ///
saving(wave2, replace)

twoway bar toverall_p incentive, barw(0.6) bfcolor(gs5*0.2) blcolor(gs5*0.2)  ///
yscale(range(0 1) lstyle(none))  ylabel(0 "" 20 "" 40 "" 60 "" 80 "" 100 "", glcolor(gs8) tstyle(none)) ///
xlabel(1 "Control" 2 "Treated", nogrid) xtitle("N = 37,741") legend(off) mlabel(toverall_p) mlabcolor(black) mlabsize(medsmall) mlabposition(6) mlabgap(3) graphregion(color(white)) ///
|| rcap toverall_l toverall_u incentive, lcolor(gs4) ///
title("Cumulative Response", size(medsmall)) ///
saving(overall, replace)

graph combine wave1.gph panelconsent.gph wave2.gph overall.gph, rows(1)
graph export "$FIGURES\fig1.pdf", replace
graph export "$FIGURES\fig1.jpg", replace


********************************************************************************
**# Bookmark #4
* Table A1
********************************************************************************
use "$OUTPUT\bruttow1w2.dta", clear
fre sex
replace sex=2 if sex==3

putexcel set "$RESULTS\Tab_A1_Descriptives.xlsx", sheet(Freq) replace
putexcel (A1:D1), merge
putexcel A1="Table A1: Descriptive statistics for the gross sample, wave 1 sample and wave 2 sample" , txtwrap bold
putexcel A3="" B3="Gross sample" C3="W1 sample" D3="W2 sample", txtwrap bold border(bottom)
tabulate sex if tw1_34 != ., matcell(freq) 
putexcel B5=matrix(freq)
putexcel A4="Sex" A5="   Male" A6="   Female" 
tabulate age10 if tw1_34 != ., matcell(freq) 
putexcel B8=matrix(freq)
putexcel A7="Age" A8="   18-29" A9="   30-39" A10="   40-49" A11="   50-59" A12="   60-70" A13="   unknown" 
tabulate marriage if tw1_34 != ., matcell(freq) 
putexcel B15=matrix(freq)
putexcel A14="Marital status" A15="   single" A16="   married" A17="   divorced" A18="   widowed" A19="   unknown" 
tabulate gkpol if tw1_34 != ., matcell(freq) 
putexcel B21=matrix(freq)
putexcel A20="Municipality" A21="   <100k" A22="   100-500k" A23="   >500k" 
tabulate durationstaym1 if tw1_34 != ., matcell(freq) 
putexcel B25=matrix(freq)
putexcel A24="Duration of stay" A25="   <3" A26="   3" A27="   4" A28="   >=5" A29="   unknown"
tabulate sex if tw1c_part != ., matcell(freq) 
putexcel C5=matrix(freq)
tabulate age10 if tw1c_part != ., matcell(freq) 
putexcel C8=matrix(freq)
tabulate marriage if tw1c_part != ., matcell(freq) 
putexcel C15=matrix(freq)
tabulate gkpol if tw1c_part != ., matcell(freq) 
putexcel C21=matrix(freq)
tabulate durationstaym1 if tw1c_part != ., matcell(freq) 
putexcel C25=matrix(freq)
tabulate sex if tw2consent != ., matcell(freq) 
putexcel D5=matrix(freq)
tabulate age10 if tw2consent != ., matcell(freq) 
putexcel D8=matrix(freq)
tabulate marriage if tw2consent != ., matcell(freq) 
putexcel D15=matrix(freq)
tabulate gkpol if tw2consent != ., matcell(freq) 
putexcel D21=matrix(freq)
tabulate durationstaym1 if tw2consent != ., matcell(freq) 
putexcel D25=matrix(freq)
putexcel A30="Total" B30=formula(SUM(B5:B6)) C30=formula(SUM(C5:C6)) D30=formula(SUM(D5:D6)) 

putexcel set "$RESULTS\Tab_A1_Descriptives.xlsx", sheet(Percent) modify 
putexcel (A1:D1), merge
putexcel A1="Table A1: Descriptive statistics for the gross sample, wave 1 sample and wave 2 sample" , txtwrap bold
putexcel A3="" B3="Gross sample" C3="W1 sample" D3="W2 sample", txtwrap bold border(bottom)
putexcel A4="Sex" A5="   Male" A6="   Female" 
putexcel A7="Age" A8="   18-29" A9="   30-39" A10="   40-49" A11="   50-59" A12="   60-70" A13="   unknown" 
putexcel A14="Marital status" A15="   single" A16="   married" A17="   divorced" A18="   widowed" A19="   unknown" 
putexcel A20="Municipality" A21="   <100k" A22="   100-500k" A23="   >500k" 
putexcel A24="Duration of stay" A25="   <3" A26="   3" A27="   4" A28="   >=5" A29="   unknown"
putexcel B5=formula(Freq!B5/Freq!B30*100) C5=formula(Freq!C5/Freq!C30*100)  D5=formula(Freq!D5/Freq!D30*100)  
putexcel B6=formula(Freq!B6/Freq!B30*100) C6=formula(Freq!C6/Freq!C30*100)  D6=formula(Freq!D6/Freq!D30*100)  
putexcel B8=formula(Freq!B8/Freq!B30*100) C8=formula(Freq!C8/Freq!C30*100)  D8=formula(Freq!D8/Freq!D30*100)  
putexcel B9=formula(Freq!B9/Freq!B30*100) C9=formula(Freq!C9/Freq!C30*100)  D9=formula(Freq!D9/Freq!D30*100)  
putexcel B10=formula(Freq!B10/Freq!B30*100) C10=formula(Freq!C10/Freq!C30*100)  D10=formula(Freq!D10/Freq!D30*100)  
putexcel B11=formula(Freq!B11/Freq!B30*100) C11=formula(Freq!C11/Freq!C30*100)  D11=formula(Freq!D11/Freq!D30*100)  
putexcel B12=formula(Freq!B12/Freq!B30*100) C12=formula(Freq!C12/Freq!C30*100)  D12=formula(Freq!D12/Freq!D30*100)  
putexcel B13=formula(Freq!B13/Freq!B30*100) C13=formula(Freq!C13/Freq!C30*100)  D13=formula(Freq!D13/Freq!D30*100)  
putexcel B15=formula(Freq!B15/Freq!B30*100) C15=formula(Freq!C15/Freq!C30*100)  D15=formula(Freq!D15/Freq!D30*100)  
putexcel B16=formula(Freq!B16/Freq!B30*100) C16=formula(Freq!C16/Freq!C30*100)  D16=formula(Freq!D16/Freq!D30*100)  
putexcel B17=formula(Freq!B17/Freq!B30*100) C17=formula(Freq!C17/Freq!C30*100)  D17=formula(Freq!D17/Freq!D30*100)  
putexcel B18=formula(Freq!B18/Freq!B30*100) C18=formula(Freq!C18/Freq!C30*100)  D18=formula(Freq!D18/Freq!D30*100)  
putexcel B19=formula(Freq!B19/Freq!B30*100) C19=formula(Freq!C19/Freq!C30*100)  D19=formula(Freq!D19/Freq!D30*100)
putexcel B21=formula(Freq!B21/Freq!B30*100) C21=formula(Freq!C21/Freq!C30*100)  D21=formula(Freq!D21/Freq!D30*100)  
putexcel B22=formula(Freq!B22/Freq!B30*100) C22=formula(Freq!C22/Freq!C30*100)  D22=formula(Freq!D22/Freq!D30*100)  
putexcel B23=formula(Freq!B23/Freq!B30*100) C23=formula(Freq!C23/Freq!C30*100)  D23=formula(Freq!D23/Freq!D30*100)  
putexcel B25=formula(Freq!B25/Freq!B30*100) C25=formula(Freq!C25/Freq!C30*100)  D25=formula(Freq!D25/Freq!D30*100)  
putexcel B26=formula(Freq!B26/Freq!B30*100) C26=formula(Freq!C26/Freq!C30*100)  D26=formula(Freq!D26/Freq!D30*100)  
putexcel B27=formula(Freq!B27/Freq!B30*100) C27=formula(Freq!C27/Freq!C30*100)  D27=formula(Freq!D27/Freq!D30*100)  
putexcel B28=formula(Freq!B28/Freq!B30*100) C28=formula(Freq!C28/Freq!C30*100)  D28=formula(Freq!D28/Freq!D30*100)  
putexcel B29=formula(Freq!B29/Freq!B30*100) C29=formula(Freq!C29/Freq!C30*100)  D29=formula(Freq!D29/Freq!D30*100)  
putexcel (B5:D29), nformat("0.0") right
putexcel A30="Total" B30=formula(Freq!B30) C30=formula(Freq!C30) D30=formula(Freq!D30) 


********************************************************************************
**# Bookmark #5
* Table 2
********************************************************************************
use "$OUTPUT\bruttow1w2.dta", clear
fre gkz
fre sex
replace sex=2 if sex==3

fre tw1_34
fre incentive
fre sex
fre age10
fre marriage
fre gkpol
fre durationstaym1
logit tw1_34  i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1, vce(cluster gkz)
estpost margins, dydx (*)
eststo tw1_34
 
eststo clear
logit tw1_34  i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1, vce(cluster gkz)
estpost margins, dydx (*)
eststo tw1_34

logit tw1c_part  i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1, vce(cluster gkz)
estpost margins, dydx (*)
eststo tw1c_part

logit tw2consent i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1, vce(cluster gkz)
estpost margins, dydx (*)
eststo tw2consent

logit toverall i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1, vce(cluster gkz)
estpost margins, dydx (*)
eststo toverall

esttab 	tw1_34 tw1c_part tw2consent toverall ///
		using "$RESULTS\Tab_2_Regression-Response.rtf", ///
		replace label nodepvar /*modelwidth(5) onecell*/ b(%9.3f) se(%9.3f) ///
		nonumbers mtitles("Response, W1" "Consent, W1" "Response, W2" "Cumulative Response") ///
		star(* 0.05 ** 0.01 *** 0.001) nogap compress nobaselevels nonotes ///
		coeflabels(2.sex "female (ref. male)" 1.incentive "incentive change (ref. constant post-paid)") ///
		refcat(2.age10 "age (ref: 18-29)" ///
		3.marriage "marital status (ref. single)" ///
		5.gkpol "municipality size (ref. > 500k)" ///
		2.durationstaym1 "duration of stay (ref. >= 5 months)") ///
		title({\b Table 2: Average marginal effects of logistic regressions on the effect of incentive change on response and consent}) ///
		addnotes("Source: IAB-BiB/FReDA-BAMF-SOEP survey 2022/23." ///
		"Notes: Cluster-robust standard errors on the municipality level have been computed to account for possible effects of the decentralized population registers in Germany. * p < 0.05, ** p < 0.01, *** p < 0.001.")


	
	
********************************************************************************
**# Bookmark #6
* Figure 2
********************************************************************************

use "$OUTPUT\bruttow1w2.dta", clear
replace sex=2 if sex==3	
logit tw1_34  i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1 , vce(cluster gkz) or
margins i.sex#i.incentive, atmeans saving("$OUTPUT\w1_sex.dta", replace)
margins i.age10#i.incentive, atmeans saving("$OUTPUT\w1_age.dta", replace)
margins i.marriage#i.incentive, atmeans saving("$OUTPUT\w1_marriage.dta", replace)
margins i.gkpol#i.incentive, atmeans saving("$OUTPUT\w1_gkpol.dta", replace)
margins i.durationstaym1#i.incentive, atmeans saving("$OUTPUT\w1_duration.dta", replace)				

use "$OUTPUT\w1_sex.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin 
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "sex"
save "$OUTPUT\w1_sex.dta", replace

use "$OUTPUT\w1_age.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "age"
save "$OUTPUT\w1_age.dta", replace

use "$OUTPUT\w1_marriage.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "marital status"
save "$OUTPUT\w1_marriage.dta", replace

use "$OUTPUT\w1_gkpol.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "municipality"
save "$OUTPUT\w1_gkpol.dta", replace

use "$OUTPUT\w1_duration.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "duration of stay"
save "$OUTPUT\w1_duration.dta", replace

use "$OUTPUT\w1_sex.dta", clear
append using "$OUTPUT\w1_age.dta"
append using "$OUTPUT\w1_marriage.dta"
append using "$OUTPUT\w1_gkpol.dta"
append using "$OUTPUT\w1_duration.dta"
gen outcome = "Response, W1"
gen varnum = sum(variable != variable[_n-1])
gen order = _n + varnum - 1
save "$OUTPUT\w1.dta", replace




use "$OUTPUT\bruttow1w2.dta", clear
replace sex=2 if sex==3	
logit toverall  i.incentive##i.sex i.incentive##i.age10 i.incentive##i.marriage i.incentive##ib7.gkpol i.incentive##ib5.durationstaym1 , vce(cluster gkz) or
margins i.sex#i.incentive, atmeans saving("$OUTPUT\all_sex.dta", replace)
margins i.age10#i.incentive, atmeans saving("$OUTPUT\all_age.dta", replace)
margins i.marriage#i.incentive, atmeans saving("$OUTPUT\all_marriage.dta", replace)
margins i.gkpol#i.incentive, atmeans saving("$OUTPUT\all_gkpol.dta", replace)
margins i.durationstaym1#i.incentive, atmeans saving("$OUTPUT\all_duration.dta", replace)				

use "$OUTPUT\all_sex.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin 
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "sex"
save "$OUTPUT\all_sex.dta", replace

use "$OUTPUT\all_age.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "age"
save "$OUTPUT\all_age.dta", replace

use "$OUTPUT\all_marriage.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "marital status"
save "$OUTPUT\all_marriage.dta", replace

use "$OUTPUT\all_gkpol.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "municipality"
save "$OUTPUT\all_gkpol.dta", replace

use "$OUTPUT\all_duration.dta", clear
drop _deriv _term _predict _at _statistic _atopt _pvalue _at* _se_margin
reshape wide _margin _ci_lb _ci_ub, i(_m1) j(_m2)
rename _margin0 b_control
rename _ci_lb0 lb_control
rename _ci_ub0 ub_control
rename _margin1 b_treated
rename _ci_lb1 lb_treated
rename _ci_ub1 ub_treated
decode _m1, generate(group)
drop _m1
gen variable = "duration of stay"
save "$OUTPUT\all_duration.dta", replace

use "$OUTPUT\all_sex.dta", clear
append using "$OUTPUT\all_age.dta"
append using "$OUTPUT\all_marriage.dta"
append using "$OUTPUT\all_gkpol.dta"
append using "$OUTPUT\all_duration.dta"
gen outcome = "Cumulative Response"
gen varnum = sum(variable != variable[_n-1])
gen order = _n + varnum - 1
append using "$OUTPUT\w1.dta"
gsort order -outcome

foreach x in ub_control ub_treated lb_control lb_treated b_control b_treated {
	replace `x' = `x' * 100 
}
save "$OUTPUT\w1all.dta", replace

use "$OUTPUT\w1all.dta", clear
replace outcome = "(a) Response, W1" if outcome == "Response, W1"
replace outcome = "(b) Cumulative Response" if outcome == "Cumulative Response"
*labmask order, values(group)
*levelsof order, local(K)
graph twoway ///
  || rcap ub_control lb_control order , lcolor(gs10) horizontal ///  
  || rcap ub_treated lb_treated order , lcolor(black) horizontal ///  
  || scatter order b_control , mfcolor(gs10) mlcolor(gs10) ms(O) ///  
  || scatter order b_treated , mfcolor(black) mlcolor(black) ms(O) ///  
  subtitle(, bcolor(white) size(medsmall)) by(outcome, note("") ) ///
  legend(order(3 "Control" 4 "Treated") rows(1) size(small) region(lstyle(none))) ///
  ylab(1 "male" 2 "female" 4 "18-29" 5 "30-39" 6 "40-49" ///
		7 "50-59" 8 "60-70" 9 "unknown age" 11 "single" 12 "married" ///
		13 "divorced" 14 "widowed" 15 "unknown marital status" ///
		17 "municipality < 100,000" 18 "municipality 100,000-500,000" ///
		19 "municipality > 500,000" 21 "< 3 months" 22 "3 months" ///
		23 "4 months" 24 ">= 5 months" 25 "unknown duration of stay", ///
		valuelabel angle(0) labsize(small) grid gstyle(dot)) ytitle("") yscale(lwidth(thin) reverse) ///
  xlab(, labsize(small) grid gstyle(dot) nogextend) scheme(s1mono)  plotregion(lcolor(black) lwidth(thin))
graph export "$FIGURES\Fig_2.pdf", replace
graph export "$FIGURES\Fig_2.jpg", replace



	
	
	
********************************************************************************
**# Bookmark #7
* Figure 3: Itemnonresponse
********************************************************************************
* Wave 1
use "$OUTPUT\bruttow1w2.dta", clear
rename pid lfd
keep lfd incentive
save "$OUTPUT\bruttow1_incentive.dta", replace
* The analysis includes incomplete interviews not available in the published SUF
* Additional data available on request from the authors
use "$DATA\IAB-BIB-BAMF-SOEP\SUF\ukr-pl.dta", clear
drop if syear == 2023
rename pid lfd
merge 1:1 lfd using "...\ukr-pl-w1-incomplete.dta"
drop _merge
merge 1:1 lfd using "$OUTPUT\bruttow1_incentive.dta"
drop if _merge == 2
drop _merge
save "$OUTPUT\itemnonresponsew1.dta", replace

use "$OUTPUT\itemnonresponsew1.dta", clear
recode pukr7_1 (-2=9)
egen itemmissw1all = anycount(pukr1 pukr3y pukr4y pukr5 pukr6 pukr7_1 pukr10 ///
pukr11_1 pukr13 pukr15 pukr18 pukr19 pukr21 pukr24_1 pukr27 pukr28 pukr30 ///
pukr31 pukr32 pukr39 pukr49 pukr56 pukr57 pukr62 pukr71 pukr72 pukr74 pukr75 ///
pukr77 pukr78 pukr79_1 pukr79_2 pukr79_3 pukr79_4 pukr80 pukr82 pukr84 ///
pukr85 pukr86 pukr88 pukr89_1 pukr89_2 pukr89_3), values(-1 -2 -3 -4)
fre itemmissw1all
keep lfd itemmissw1all incentive
save "$OUTPUT\itemnonresponsew1.dta", replace

* Wave 2: Analysis includes only respondents living in Germany
use "$DATA\IAB-BIB-BAMF-SOEP\SUF\ukr-pl.dta", clear
drop if syear == 2022
rename pid lfd
* The analysis includes incomplete interviews not available in the published SUF
* Additional data available on request from the authors
merge 1:1 lfd using "ukr-pl-w2-incomplete.dta"
drop _merge
drop if pukr202 >= 3 & pukr202 <= 4
egen itemmissw2all = anycount(pukr201 pukr214 pukr215 pukr216 pukr21 ///
pukr13_v1 pukr23_1 pukr27 pukr28 pukr231 pukr10 pukr1_v1 pukr236 pukr237 ///
pukr56 pukr239 pukr245 pukr62 pukr259 pukr65_2 pukr84 pukr85 pukr86 pukr263 ///
pukr30 pukr32 pukr280 pukr74 pukr75 pukr77_v1 pukr78 pukr79_1 pukr79_2 ///
pukr79_3 pukr79_4 pukr88 pukr89_1 pukr89_2 pukr89_3 pukrp001), values(-1 -2 -3 -4)
fre itemmissw2all
keep lfd itemmissw2all
save "$OUTPUT\itemnonresponsew2.dta", replace

use "$OUTPUT\itemnonresponsew1.dta", clear
merge 1:1 lfd using "$OUTPUT\itemnonresponsew2.dta"
cap drop _merge
save "$OUTPUT\itemnonresponsew1w2.dta", replace

* Dependent variables
use "$OUTPUT\itemnonresponsew1w2.dta", clear
label variable itemmissw1all "Wave 1"
label variable itemmissw2all "Wave 2"
gen itemmissw1w2all = itemmissw1all + itemmissw2all
label variable itemmissw1w2all "Wave 1 + Wave 2"

tab1 itemmissw1all itemmissw2all
fre incentive

poisson itemmissw1all i.incentive
eststo itemmissw1all
poisson itemmissw2all i.incentive
eststo itemmissw2all
poisson itemmissw1w2all i.incentive
eststo itemmissw1w2all

coefplot 	(itemmissw1all \ itemmissw2all \ itemmissw1w2all), ///
			drop(_cons) xline(0) byopts(compact cols(1)) asequation swapnames ///
			legend(off) ciopts(color(black)) mfcolor(black) mlcolor(black) ///
			xtitle("Poisson regression coefficients") ///
			xlabel(-0.20(0.1)0.20) xscale(range(-0.20(0.10)0.20)) ysize(5) xsize(9)
graph export "$FIGURES\Fig_3.jpg", replace
graph export "$FIGURES\Fig_3.pdf", as(pdf) replace