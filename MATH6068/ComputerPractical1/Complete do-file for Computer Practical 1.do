*MATH6068 Computer Practical 1
clear all

*Loading in your data***********************************************************
*Change directory to the location where the dataset and ado files are stored
cd "C:\Users\ldw1c13\OneDrive - University of Southampton\Desktop\Computer Practical 1" 

*Load in data
use "jranramp.dta", clear 

*List variables in the dataset
describe

*Explore variables in more detail
codebook

*List the first five observations for the most important variables
list id caco nramp_1 nramp_2 d2s1471_1 d2s1471_2 in 1/5
 
 
* Testing Hardy-Weinberg Equilibrium*******************************************
*HWE assumption does not hold for this marker among the whole sample
gtab nramp_1 nramp_2

*HWE assumption does not hold for this marker among the controls
gtab nramp* if caco==0


* Analysis at the person level************************************************

*1) Load data 
use "jranramp.dta", clear 

*2) Create a single variable for the nramp genotype
egen nrg = gtype(nramp_1 nramp_2)
codebook nrg
table nrg caco 
tab nrg caco, row exact

*3) Generate variables to count number of times each allele occurs in a genotype
gtab nramp_1 nramp_2, gen(NR_)
rename NR_1 NR_a
rename NR_2 NR_b
rename NR_3 NR_c

list NR_a NR_b NR_c nramp_1 nramp_2 in 1/5
tab caco NR_b, chi2 col

*4) Calculate odds ratios
mhodds caco NR_b, compare(1,0) 
mhodds caco NR_b, compare(2,1)

*5) Model assuming odds of case increase by fixed amount per extra 'b' allele:
mhodds caco NR_b

*6) Similar results with logistic regression
logistic caco NR_b

*7) Treat number of `b' alleles as a categorical variable: 
logistic caco i.NR_b

*8) Generate variables which separate the d2s1471 variables into two groups  
egen d2simp_1 = cut(d2s1471_1), at(1,8,17)
table d2simp_1 d2s1471_1
egen d2simp_2 = cut(d2s1471_2), at(1,8,17)
table d2simp_2 d2s1471_2

*Group the rare alleles of this marker:
grprare d2s1471_1 d2s1471_2, gen(g_1 g_2)
table g_1 d2s1471_1
table g_2 d2s1471_2

*Perform multiple degree of freedom test based upon all the alleles of D2S1471
gtab d2s1471_1 d2s1471_2, gen(Allele_)

*Allele_5 counts the number of 'e' alleles in d2s1471_1 and d2s1471_2
logistic caco Allele_*
testparm Allele_*


* Analysis at the chromosome level*********************************************


*1) *Load in and reshape data and label nramp variable:  
use "jranramp.dta", clear 
greshape, id(id) gen(chr)
label define nramplab 1 "a" 2 "b" 3 "c"
label values nramp nramplab
desc

*2) Examine NRAMP alleles in cases and controls 
table caco nramp
tab caco nramp, col exact

*3) Compute odds ratio for the 'b' allele compared to the 'a' allele:
mhodds caco nramp, co(2,1)
mhodds caco nramp, compare(1,2)
