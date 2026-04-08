clear all

*Question 2
*Change the directory, clear data in memory and load the dataset
cd "C:\Users\ldw1c13\OneDrive - University of Southampton\Desktop\Computer Practical 2"
use exercise, clear

tdt allele*, emin(0)

*Question 3
*Clear data in memory and load a different dataset
use mhc3iddm.dta,clear
describe

*For this marker, allele '2' is transmitted to the affected offspring
*more frequent than we would expect by chance
tdt bat2_*, 
tdt bat2_*, robust
tdt bat2_*, cluster(pedigree)

help tdt

gtrr bat2_*

help gtrr

gtrr bat2_*, robust ref(1/1)


*Clear data in memory and load a different dataset
use exercise.dta, clear

*Create a new dataset called 'casecon.dta' and then load this new dataset
pseudocc allele1 allele2, saving(casecon) replace
use casecon, clear


*****Question 4 
*Clear the data in memory and load a different dataset
use mhc3iddm.dta, clear

*Create new datasets and then load the new dataset 'mhccc.dta'
pseudocc bat*, saving(mhccc) replace
pseudocc bat*, saving(mhcccph) phase replace 
use mhccc, clear

quietly gtab bat2_*, gen(B2_)
*this command has created two variables B2_1 (showing the number of 1 alleles at B2)
*... and B2_2 (showing the number of 2 alleles at B2)

list B2_1 B2_2 bat2* in 1/5 

quietly gtab bat3_*, gen(B3_)

clogit case B2_*, group(set) or

clogit case B3_*, group(set) or

clogit case B2_* B3_*, group(set) or

clogit case B2_*, group(set) cluster(pedigree) or

*Clear the data in memory and load the dataset 'mhcccph.dta'
use mhcccph, clear

egen mat = htype(bat*_1)
egen pat = htype(bat*_2), co(mat)

quietly gtab mat pat, gen(hap_)

clogit case hap_2, group(set) or
clogit case hap_3, group(set) or
clogit case hap_*, group(set) or

clogit case hap_2, group(set) cluster(pedigree) or
clogit case hap_3, group(set) cluster(pedigree) or
clogit case hap_*, group(set) cluster(pedigree) or