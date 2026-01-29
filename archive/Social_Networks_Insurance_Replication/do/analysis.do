clear matrix 
clear
set more off
set mem 300m
set matsize 800
set seed 123456789
capture log close
log using "C:\Documents\disk D\insurance exp\do-files_round2_data\analysis.log", replace 
cd "C:\Documents\disk D\insurance exp\data_round2\final\"

clear

use 0422analysis, replace
egen vilid = group(village)
sum vilid, d
xi i.vilid


**********************************************************1. Summary statistics************************************************************
*1. Table 1, Mean and std 
tabstat male age agpop educ ricearea_2010 rice_inc disaster_yes disaster_loss risk_averse disaster_prob understanding, stats (count mean sd)
tabstat network_obs network_rate_preintensive network_twoside network_second, stats (count mean sd)
tabstat indegree path_out_ind eigenvector, stats (count mean sd)
tabstat takeup_survey, stats (count mean sd)
gen session = 0
replace session = 11 if delay == 0 & intensive == 0
replace session = 12 if delay == 0 & intensive == 1
replace session = 21 if delay == 1 & intensive == 0
replace session = 22 if delay == 1 & intensive == 1
tab session
tabstat takeup_survey if info_none == 1, stats (count mean sd) by(session) 

*2. Table A1, Check randomization by sessions
foreach x in male age agpop educ ricearea_2010 rice_inc disaster_yes disaster_loss{
	oneway `x' session, b t
} 

**********************************************************2.Intensive session effect************************************************************
gen inter_intensive_educ = intensive*educ_good
gen inter_intensive_age = intensive*age
gen inter_intensive_exp = intensive*insurance_repay
gen inter_intensive_risk = intensive*risk_averse
gen inter_intensive_day = intensive*day 

local outputfile0 Table2.xls
*intensive session effect: table2, col 1
reg takeup_survey intensive male age agpop ricearea_2010 literacy _Ivilid* if delay == 0, cluster(address)
outreg2 using `outputfile0'
local outputfile1 TableA3.xls
*heterogeneity: table A3
reg takeup_survey intensive inter_intensive_age male age agpop ricearea_2010 literacy _Ivilid* if delay == 0, cluster(address)
test intensive inter_intensive_age
outreg2 using `outputfile1'
reg takeup_survey intensive inter_intensive_educ male age agpop ricearea_2010 educ_good _Ivilid* if delay == 0, cluster(address)
test intensive inter_intensive_educ
outreg2 using `outputfile1'
reg takeup_survey intensive inter_intensive_exp insurance_repay male age agpop ricearea_2010 literacy _Ivilid* if delay == 0, cluster(address)
test intensive inter_intensive_exp
outreg2 using `outputfile1'
reg takeup_survey intensive inter_intensive_risk risk_averse male age agpop ricearea_2010 literacy _Ivilid* if delay == 0, cluster(address)
test intensive inter_intensive_risk
outreg2 using `outputfile1'
reg takeup_survey intensive inter_intensive_day day male age agpop ricearea_2010 literacy _Ivilid* if delay == 0, cluster(address)
test intensive inter_intensive_day
outreg2 using `outputfile1'

**********************************************************3. Social Network effect************************************************************
gen inter_network_educ = network_rate_preintensive*intensive
gen inter_network_educ1 = network_onlyone*intensive
gen inter_network_educ2 = network_onlytwo*intensive
gen inter_network_educ3 = network_twomore*intensive
gen inter_inten_delay = intensive*delay

*2.1. Network rate - all sample
*table 2, columns 2-5
**check robustness by adding spillover effect from simple session**
reg takeup_survey network_rate_preintensive male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile0'
reg takeup_survey network_rate_preintensive network_rate_presimple intensive friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile0'
reg takeup_survey network_rate_preintensive intensive inter_network_educ male age agpop ricearea_2010 literacy risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile0'
test network_rate_preintensive inter_network_educ
test intensive inter_network_educ
reg takeup_survey network_onlyone network_onlytwo network_twomore intensive inter_network_educ1 inter_network_educ2 inter_network_educ3 male age agpop ricearea_2010 literacy risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile0'

gen nofriend = 1
replace nofriend = 0 if delay == 1 & info_none == 1 & network_yes == 1
*2.2. test spillover effect from non-friends
*table2, column 6
reg takeup_survey intensive delay inter_inten_delay male age agpop ricearea_2010 literacy risk_averse disaster_prob _Ivilid* if delay == 0 | (delay == 1 & info_none == 1 & nofriend == 1), cluster(address)
outreg2 using `outputfile0'
test intensive inter_inten_delay

*2.3. Alternative measures and nonlinear effect
*table 3
local outputfile2 Table3.xls
reg takeup_survey network_twoside male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile2'
reg takeup_survey network_second male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile2'
reg takeup_survey network_onlyone network_onlytwo network_twomore male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile2'
save temp, replace

**********************************************************4. Price effect************************************************************
use 0422price, replace
*Table A2, check validity of price randomization
xi i.address
gen pricesquare = price*price
local outputfile3 TableA2.xls
foreach x in male age agpop literacy earlyarea{
	reg `x' price _Iaddress*, cluster(address)
	outreg2 using `outputfile3'
}

*Figure 2, Plot insurance demand curve, by having above or below median share of friends in first round intensive session
bysort price: egen takeup_mean1 = mean(takeup) if preintens_high == 0
bysort price: egen takeup_mean2 = mean(takeup) if preintens_high == 1
bysort price: egen takeup_sd1 = sd(takeup) if preintens_high == 0
bysort price: egen takeup_sd2 = sd(takeup) if preintens_high == 1
bysort price: gen counttotal = _N
bysort price: egen count2 = sum(preintens_high) 
gen count1 = counttotal - count2
gen ci1 = takeup_mean1 + invttail(count1, 0.025)*takeup_sd1/sqrt(count1) if preintens_high == 0
replace ci1 = takeup_mean2 + invttail(count2, 0.025)*takeup_sd2/sqrt(count2) if preintens_high == 1
gen ci2 = takeup_mean1 - invttail(count1, 0.025)*takeup_sd1/sqrt(count1) if preintens_high == 0
replace ci2 = takeup_mean2 - invttail(count2, 0.025)*takeup_sd2/sqrt(count2) if preintens_high == 1
twoway (line takeup_mean1 takeup_mean2 price, clpattern("dash")) (rcap ci1 ci2 price), xtitle("Price") ytitle("Take-up") legend(order(1 "%Network financially educated = Low" 2 "%Network financially educated = High" 3 "95% CI") colfirst)

*Define dummies of the number of friends
xi i.address
forvalues i = 1(1)8{
  gen friend`i'=0 if network_obs ~= .
  replace friend`i' = 1 if network_obs == `i'
}

*Table 4, price effect analysis
gen inter_price_network = price*network_rate_preintensive
local outputfile4 Table4.xls
reg takeup price network_rate_preintensive intensive male age agpop earlyarea literacy friend1 friend2 friend3 friend4 friend5 friend6 friend7 friend8 _Iaddress*, cluster(address)
outreg2 using `outputfile4'
reg takeup price network_rate_preintensive inter_price_network intensive male age agpop earlyarea literacy friend1 friend2 friend3 friend4 friend5 friend6 friend7 friend8 _Iaddress*, cluster(address)
outreg2 using `outputfile4'
test price inter_price_network
test network_rate_preintensive inter_price_network
reg takeup price network_rate_preintensive inter_price_network price_high_ratio price_low_ratio intensive male age agpop earlyarea literacy friend1 friend2 friend3 friend4 friend5 friend6 friend7 friend8 _Iaddress*, cluster(address)
outreg2 using `outputfile4'
test price inter_price_network
test network_rate_preintensive inter_price_network
*Table 7, column 9, estimate effect of friends' decisions using the price sample
local outputfile7 Table7.xls
ivregress 2sls takeup price (network_rate_pretakeup = network_avgprice) intensive male age agpop earlyarea literacy friend1 friend2 friend3 friend4 friend5 friend6 friend7 friend8 _Iaddress*, cluster(address)
estat firststage
outreg2 using `outputfile7'


**********************************************************5. Social learning of insurance benefits****************************************
use temp, replace
gen inter_net_knowledge = network_rate_preintensive*knowledge_network

*3. Test learning of insurance benefits
*table 5
local outputfile5 Table5.xls 
reg understanding intensive delay inter_inten_delay male age agpop ricearea_2010 literacy risk_averse disaster_prob _Ivilid* if info_none == 1, cluster(address)
outreg2 using `outputfile5'
test intensive inter_inten_delay
gen inter_inten_netyes = intensive*network_yes
reg understanding intensive network_yes inter_inten_netyes male age agpop ricearea_2010 literacy risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile5'
reg understanding network_rate_preintensive male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile5'
ivreg understanding (knowledge_network=network_rate_preintensive) male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile5'


**********************************************************6. Effect of default************************************************************
*6.1 Table A5: characteristics of insurance takers by default option
preserve
keep if takeup_survey == 1 & delay == 0
foreach x in male age agpop ricearea_2010 literacy general_trust understanding disaster_prob reveal{
   ttest `x', by(default)
}
restore

*6.2 Table 6, column 1: Default effect
local outputfile6 Table6.xls
reg takeup_survey default male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 0, cluster(address)
outreg2 using `outputfile6'

*6.3 Table A4, check heterogeneity of the default effect
gen inter_default_network = network_rate_preintensive*default
gen inter_default_intensive = intensive*default
gen inter_default_trust = default*general_trust 

local outputfile62 TableA4.xls
reg takeup_survey network_rate_preintensive default inter_default_network intensive male age agpop ricearea_2010 literacy risk_averse disaster_prob friend* _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile62'
test network_rate_preintensive inter_default_network
test default inter_default_network
reg takeup_survey default inter_default_intensive male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 0, cluster(address)
test default inter_default_intensive
test intensive inter_default_intensive
outreg2 using `outputfile62'
reg takeup_survey default inter_default_trust general_trust male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 0, cluster(address)
test default inter_default_trust
outreg2 using `outputfile62'
reg insurance_buy default male age agpop ricearea_2010 literacy _Ivilid* if delay == 0, cluster(address)
outreg2 using `outputfile62'


**********************************************************7. Effect of overall take-up************************************************************
bysort address: egen pre_takeup_ratetemp = mean(takeup_survey) if delay == 0
bysort address: egen pre_takeup_rate = mean(pre_takeup_ratetemp)
gen inter_pre_none = pre_takeup_rate*info_none
gen inter_pre_none_iv = default*info_none

*Table 6, OLS and IV estimation, columns 2,3,5,7
reg takeup_survey pre_takeup_rate info_none inter_pre_none male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1, cluster(address)
test pre_takeup_rate inter_pre_none
outreg2 using `outputfile6'
ivreg takeup_survey (pre_takeup_rate inter_pre_none = default inter_pre_none_iv) info_none male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1, cluster(address)
test pre_takeup_rate inter_pre_none
outreg2 using `outputfile6'
ivregress 2sls takeup_survey (pre_takeup_rate = default) male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 0, cluster(address)
outreg2 using `outputfile6'
estat firststage
ivregress 2sls takeup_survey (pre_takeup_rate = default) male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile6'
estat firststage

*Table 6, reduced form estimation, columns 4,6,8
reg takeup_survey default info_none inter_pre_none_iv male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1, cluster(address)
outreg2 using `outputfile6'
reg takeup_survey default male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 0, cluster(address)
outreg2 using `outputfile6'
reg takeup_survey default male age agpop ricearea_2010 literacy intensive risk_averse disaster_prob _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile6'

**********************************************************8. Effect of friends' take-up************************************************************
gen inter_pre_takeup = network_rate_presession*pre_takeup_rate
gen inter_pre_takeup_iv = network_rate_presession*default

gen inter_prenetwork_none = network_rate_pretakeup*info_none
gen inter_prenetwork_none_iv1 = inter_pre_takeup_iv*info_none

*Table 7, OLS and IV estimation, columns 1,2,3,5,7
reg network_rate_pretakeup inter_pre_takeup_iv male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_takeup_rate ~= 1, cluster(address)
outreg2 using `outputfile7'
reg takeup_survey pre_takeup_rate network_rate_pretakeup info_none inter_pre_none inter_prenetwork_none male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_takeup_rate ~= 1, cluster(address)
outreg2 using `outputfile7'
test pre_takeup_rate inter_pre_none
test network_rate_pretakeup inter_prenetwork_none
ivreg takeup_survey (network_rate_pretakeup pre_takeup_rate inter_pre_none inter_prenetwork_none = inter_pre_takeup_iv network_rate_presession default inter_pre_none_iv inter_prenetwork_none_iv1) info_none male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_takeup_rate ~= 1, cluster(address)
outreg2 using `outputfile7'
test pre_takeup_rate inter_pre_none
test network_rate_pretakeup inter_prenetwork_none
ivregress 2sls takeup_survey (network_rate_pretakeup pre_takeup_rate = inter_pre_takeup_iv default) male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_takeup_list == 1, cluster(address)
outreg2 using `outputfile7'
ivregress 2sls takeup_survey (network_rate_pretakeup pre_takeup_rate = inter_pre_takeup_iv default) male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile7'

*Table 7, reduced form estimation, columns 4,6,8
reg takeup_survey inter_pre_takeup_iv default inter_pre_none_iv inter_prenetwork_none_iv1 info_none male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_takeup_rate ~= 1, cluster(address)
outreg2 using `outputfile7'
reg takeup_survey inter_pre_takeup_iv default male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_takeup_list == 1, cluster(address)
outreg2 using `outputfile7'
reg takeup_survey inter_pre_takeup_iv default male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile7'


**********************************************************9. Network Structure************************************************************
local outputfile8 Table8.xls
gen inter_net_allindegree = mean_allnetindegree*network_rate_preintensive
gen inter_net_allpathout = mean_allnetpath_out_ind*network_rate_preintensive
gen inter_net_alleigenvector = mean_allneteigenvector*network_rate_preintensive

gen inter_net_educindegree = mean_1steduc_indegree*network_rate_preintensive
gen inter_net_educpathout = mean_1steduc_path_out_ind*network_rate_preintensive
gen inter_net_educeigenvector = mean_1steduc_eigenvector*network_rate_preintensive

gen inter_net_indegree = indegree*network_rate_preintensive
gen inter_net_pathin = path_in_ind*network_rate_preintensive
gen inter_net_eigenvector = eigenvector*network_rate_preintensive

*1) degree
reg takeup_survey network_rate_preintensive mean_1steduc_indegree inter_net_educindegree indegree inter_net_indegree mean_allnetindegree mean_allnetpath_out_ind inter_net_allindegree mean_allneteigenvector male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_none == 1, cluster(address)
test network_rate_preintensive inter_net_educindegree
test mean_1steduc_indegree inter_net_educindegree
test network_rate_preintensive inter_net_indegree
test indegree inter_net_indegree
outreg2 using `outputfile8'

*2) path
reg takeup_survey network_rate_preintensive mean_1steduc_path_out_ind inter_net_educpathout path_in_ind inter_net_pathin mean_allnetindegree mean_allnetpath_out_ind inter_net_allpathout mean_allneteigenvector male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_none == 1, cluster(address)
test network_rate_preintensive inter_net_educpathout
test mean_1steduc_path_out_ind inter_net_educpathout
test network_rate_preintensive inter_net_pathin
test path_in_ind inter_net_pathin
outreg2 using `outputfile8'

*3)centrality
reg takeup_survey network_rate_preintensive mean_1steduc_eigenvector inter_net_educeigenvector eigenvector inter_net_eigenvector mean_allnetindegree mean_allnetpath_out_ind mean_allneteigenvector inter_net_alleigenvector male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_none == 1, cluster(address)
test network_rate_preintensive inter_net_educeigenvector
test mean_1steduc_eigenvector inter_net_educeigenvector
test network_rate_preintensive inter_net_eigenvector
test eigenvector inter_net_eigenvector
outreg2 using `outputfile8'

*4) all together
reg takeup_survey network_rate_preintensive mean_1steduc_indegree inter_net_educindegree indegree inter_net_indegree mean_1steduc_path_out_ind inter_net_educpathout path_in_ind inter_net_pathin mean_1steduc_eigenvector inter_net_educeigenvector eigenvector inter_net_eigenvector mean_allnetindegree mean_allnetpath_out_ind mean_allneteigenvector inter_net_allindegree inter_net_allpathout inter_net_alleigenvector male age agpop ricearea_2010 literacy intensive _Ivilid* if delay == 1 & info_none == 1, cluster(address)
outreg2 using `outputfile8'

