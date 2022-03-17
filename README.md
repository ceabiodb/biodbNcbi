# biodbNcbi

An R package for accessing NCBI databases, based on R
package/framework [biodb](https://github.com/pkrog/biodb/).

## Introduction

This package is an extension of [biodb](https://github.com/pkrog/biodb/) that
implements connectors to NCBI databases.

## Installation

Install the latest version of this package by running the following commands:
```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install('biodbNcbi')
```

## Examples

To instantiate a connector to, for instance, NCBI CCDS run:
```r
mybiodb <- biodb::newInst()
conn <- mybiodb$getFactory()$createConn('ncbi.ccds')
mybiodb$terminate()
```

## Documentation

To get documentation on the implemented connector, run the following command in
R:
```r
?biodbNcbi::NcbiCcdsConn
```


## Citations

<https://www.ncbi.nlm.nih.gov/gene>

 * Gene [Internet]. Bethesda (MD): National Library of Medicine (US), National Center for Biotechnology Information; 2004 – [cited 2018 Feb 08]. Available from: <https://www.ncbi.nlm.nih.gov/gene/>.

### NCBI CCDS

 <https://www.ncbi.nlm.nih.gov/projects/CCDS/CcdsBrowse.cgi>

 * Pruitt KD, Harrow J, Harte RA, Wallin C, Diekhans M, Maglott DR, Searle S, Farrell CM, Loveland JE, Ruef BJ, Hart E, Suner MM, Landrum MJ, Aken B, Ayling S, Baertsch R, Fernandez-Banet J, Cherry JL, Curwen V, Dicuccio M, Kellis M, Lee J, Lin MF, Schuster M, Shkeda A, Amid C, Brown G, Dukhanina O, Frankish A, Hart J, Maidak BL, Mudge J, Murphy MR, Murphy T, Rajan J, Rajput B, Riddick LD, Snow C, Steward C, Webb D, Weber JA, Wilming L, Wu W, Birney E, Haussler D, Hubbard T, Ostell J, Durbin R, Lipman D. (2009) The consensus coding sequence (CCDS) project: Identifying a common protein-coding gene set for the human and mouse genomes. Genome Res. 2009 Jul;19(7):1316-23, <https://doi.org/10.1101/gr.080531.108>.
 * Harte RA, Farrell CM, Loveland JE, Suner MM, Wilming L, Aken B, Barrell D, Frankish A, Wallin C, Searle S, Diekhans M, Harrow J, Pruitt KD. (2012) Tracking and coordinating an international curation effort for the CCDS Project. Database 2012 Mar 20;2012:bas008. doi: 10.1093/database/bas008, <https://doi.org/10.1093/database/bas008>.
 * Farrell CM, O'Leary NA, Harte RA, Loveland JE, Wilming LG, Wallin C, Diekhans M, Barrell D, Searle SM, Aken B, Hiatt SM, Frankish A, Suner MM, Rajput B, Steward CA, Brown GR, Bennett R, Murphy M, Wu W, Kay MP, Hart J, Rajan J, Weber J, Snow C, Riddick LD, Hunt T, Webb D, Thomas M, Tamez P, Rangwala SH, McGarvey KM, Pujar S, Shkeda A, Mudge JM, Gonzalez JM, Gilbert JG, Trevanion SJ, Baertsch R, Harrow JL, Hubbard T, Ostell JM, Haussler D, Pruitt KD. (2014) Current status and new features of the Consensus Coding Sequence database. Nucleic Acids Res. 2014 Jan 1;42(1):D865-72, <https://doi.org/10.1093/nar/gkt1059>.

### NCBI Pubchem

<https://pubchem.ncbi.nlm.nih.gov>

 * Kim S, Thiessen PA, Bolton EE, Chen J, Fu G, Gindulyte A, Han L, He J, He S, Shoemaker BA, Wang J, Yu B, Zhang J, Bryant SH. PubChem Substance and Compound databases. Nucleic Acids Res. 2016 Jan 4; 44(D1):D1202-13. Epub 2015 Sep 22, <https://doi.org/10.1093/nar/gkv951>.

