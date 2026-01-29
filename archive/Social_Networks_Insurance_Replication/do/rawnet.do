*This do-file: 
*1. Define network variables: basic measure, alternative measure, and network structural variables
*2. Merge network variables back to individual information data

*************************1. Generate basic social network variables*********************
cd C:\Users\18313\Desktop\Social_Networks_Insurance_Replication\do
use 0422allinforawnet, replace
*dropped 331 households
drop if network_missname == 1 & network_id == .
d,s
codebook id

*Define Network size
bysort id: gen network_obs = _N
tab network_obs
*Define friends in first round intensive session
gen network_pre_intensive = .
replace network_pre_intensive = 1 if delay == 0 & intensive == 1
*Define friends in first round session
gen network_presession = .
replace network_presession = 1 if delay == 0
bysort id: egen network_sum_preintensive = sum(network_pre_intensive)
bysort id: egen network_sum_presession = sum(network_presession)
*Define fiends' take-up rate in first round sessions
gen network_pretakeup = .
replace network_pretakeup = 1 if network_presession == 1 & takeup_survey == 1
gen flag = 1 if network_presession == 1 & takeup_survey ~= .
bysort id: egen flag_sum = sum(flag)
bysort id: egen network_sum_pretakeup = sum(network_pretakeup)
replace network_sum_pretakeup = . if flag_sum == 0
gen network_rate_preintensive = network_sum_preintensive/network_obs
gen network_rate_presession = network_sum_presession/network_obs
gen network_rate_pretakeup = network_sum_pretakeup/network_sum_presession
*Define friends in first round simple session
gen network_rate_presimple = network_rate_presession - network_rate_preintensive
sum network_rate_presimple, d
*Define first-round friends' average insurance knowledge
bysort id: egen knowledge_network_temp = mean(understanding) if delay == 0
bysort id: egen knowledge_network = mean(knowledge_network_temp)

*Generate dummies of number of friends
forvalues i = 1(1)5{
  gen friend`i'=0 if network_obs ~= .
  replace friend`i' = 1 if network_obs == `i'
}
*l friend1 friend2 friend3 friend4 friend5 network_obs in 1/10

*Define number of friends in first round intensive session
gen network_onlyone = 0 if network_rate_preintensive ~= .
gen network_onlytwo = 0 if network_rate_preintensive ~= .
gen network_twomore = 0 if network_rate_preintensive ~= .
gen number_preintensive = network_rate_preintensive*network_obs
tab number_preintensive
replace network_onlyone = 1 if number_preintensive == 1
replace network_onlytwo = 1 if number_preintensive == 2
replace network_twomore = 1 if number_preintensive ~= . & number_preintensive > 1

*Define network - yes or no
gen network_yes = 1 if network_rate_preintensive ~= .
replace network_yes = 0 if network_rate_preintensive == 0

keep id network_obs network_rate_preintensive network_rate_presimple network_rate_presession network_rate_pretakeup knowledge_network friend* network_onlyone network_onlytwo network_twomore network_yes
duplicates drop
codebook id
d,s
sort id
*4662 households
save 0422basicnetworkvar, replace


*************************2. Generate alternative social network measures: strong and weak*********************
************a.define two-sided links************
use 0422allinforawnet, replace
*dropped 331 households
drop if network_missname == 1 & network_id == .
*22912 obs, 4662 hhs
codebook id
d,s
keep id network_id
duplicates drop
sort id network_id
*22801 obs
save 0422data1, replace
d,s

rename id id_temp
rename network_id id
rename id_temp network_id
sort id network_id
save 0422data2, replace

use 0422data1, replace
merge id network_id using 0422data2
tab _m
drop if _m == 2
gen twoside = 0 
replace twoside = 1 if _m == 3
drop _m
keep id network_id twoside
duplicates drop
sort id network_id
save 0422twoside, replace

*************b.define second-order links***********
use 0422allinforawnet, replace
*dropped 331 households
drop if network_missname == 1 & network_id == .
*22912 obs, 4662 hhs
codebook id
keep id network_id
drop if network_id == 99
duplicates drop
save 0422data1, replace

rename network_id secondnet_id
rename id network_id
bysort network_id (secondnet_id): gen obs = _n
reshape wide secondnet_id, i(network_id) j(obs)
sort network_id
isid network_id
save 0422data2, replace

use 0422data1, replace
d
sort network_id
merge network_id using 0422data2
tab _m
drop if _m == 2
drop _m
reshape long secondnet_id, i(id network_id) j(secondnet_obs)
drop secondnet_obs network_id
drop if secondnet_id == .
duplicates drop
codebook id 
drop if id == secondnet_id
bysort id: gen secondnet_obs = _N
tab secondnet_obs
d
sort secondnet_id
save 0422secondnet, replace


*************c.define strong social network measure***********
*Merge two-sided link information into raw network data
use 0422allinforawnet, replace
drop if network_missname == 1 & network_id == .
sort id network_id
merge id network_id using 0422twoside
tab _m
drop _m
*Define two-sided network
bysort id: gen network_obs = _N
gen twoside_pre_intensive = .
replace twoside_pre_intensive = 1 if delay == 0 & intensive == 1 & twoside == 1
bysort id: egen twoside_sum_preintensive = sum(twoside_pre_intensive)
gen network_twoside = twoside_sum_preintensive/network_obs
keep id network_twoside
duplicates drop
sort id
save 0422strongnetworkvar, replace

*************d.define weak social network measure***********
use 0422allinforawnet, replace
keep network_id delay intensive
duplicates drop
sort network_id
save 0422allnetinfo, replace
use 0422secondnet, replace
rename secondnet_id network_id
sort network_id
merge network_id using 0422allnetinfo
tab _m
drop _m

gen second_pre_intensive = .
replace second_pre_intensive = 1 if delay == 0 & intensive == 1
bysort id: egen second_sum_preintensive = sum(second_pre_intensive)
gen network_second = second_sum_preintensive/secondnet_obs
keep id network_second secondnet_obs
duplicates drop
drop if id == .
sort id
save 0422weaknetworkvar, replace

*************e.merge all network measures together***********
use 0422basicnetworkvar, replace
sort id
merge id using 0422strongnetworkvar
tab _m
drop _m
sort id
merge id using 0422weaknetworkvar
tab _m
drop _m
sort id
save 0422networkvar, replace

*************************3. Generate social network structural measures: indegree, path length, and eigenvector centrality*********************
*individual social network structural measures are calculated using UCINET
use 0422structure_all, replace
rename id network_id
sort network_id
save 0422structure_network, replace

use 0422allinforawnet, replace
*dropped 331 households
drop if network_missname == 1 & network_id == .
d,s
codebook id
sort network_id
merge network_id using 0422structure_network
tab _m
drop if _m == 2
drop _m

*Define average structural measures of friends
foreach x in indegree path_out_ind path_in_ind eigenvector{
	bysort id: egen mean_allnet`x' = mean(`x')	
	bysort id: egen mean_1steduc_`x'temp = mean(`x') if delay == 0 & intensive == 1
	bysort id: egen mean_1steduc_`x' = mean(mean_1steduc_`x'temp)
	replace mean_1steduc_`x' = 0 if mean_1steduc_`x' == .
	replace mean_allnet`x' = 0 if mean_allnet`x' == .
}
keep id mean_allnetindegree mean_1steduc_indegree mean_allnetpath_out_ind mean_1steduc_path_out_ind mean_allnetpath_in_ind mean_1steduc_path_in_ind mean_allneteigenvector mean_1steduc_eigenvector
duplicates drop
sort id
save 0422netstructure, replace

*************************4. Merge social network variables into analysis data*********************
use 0422survey, replace
*Merge into network measures
sort id
merge id using 0422networkvar
tab _m
drop if _m == 2
drop _m
sort id
*Merge into friends' network structural measures
merge id using 0422netstructure
tab _m
drop if _m == 2
drop _m
sort id
*Merge into own network structural measures
merge id using 0422structure_all
tab _m
drop if _m == 2
drop _m
codebook id
d,s
sort id
save 0422analysis, replace
