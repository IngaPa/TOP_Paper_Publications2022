# TOP_Paper_Publications2022_

#IDEA
This project was about analyzing policies of two thousand journals within the framework of eight TOP standards: 
data citation, transparency of data, material, code and design and analysis, replication, plan and study pre-registration, 
and two effective interventions: “Registered reports” and “Open science badges”. 

# MATERIALS & METHODS
We downloaded the TOP Factor (v33, 2022-08-29 3:12 PM) metric from the https://osf.io/kgnva/files/osfstorage/5e13502257341901c3805317 
website and analyzed its content with an in-house R script (in this repo):
1) SCRIPT: fig1_Analyzing_journals_policies_and_TOP_guidelines.R
2) SCRIPT: Figure2a_b_TOP_impl_journal_statistist_0_1_piechart_barplot.R
In order to get statistics about implementation of the TOP guidelines across discipline-specific journals, 
we extracted information about journal’s disciplines from the Scopus content database. 
We downloaded SCOPUS content coverage from the https://www.elsevier.com/solutions/scopus/how-scopus-works/content?dgcid=RN_AGCM_Sourced_300005030 (existJuly2022.xlsx)
and used the first Sheet.
We identified match between those 2 tables: 
3) SCRIPT: Rscript_overlapping_TOP_dataframe_and_SCOPUS_db.R
And performed visualization and statistics:
4) SCRIPT: Fig_3_Tab2_Defining_science_disciplines_plus_plot.R
