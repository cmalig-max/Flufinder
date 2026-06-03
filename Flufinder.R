upload_fasta <- function(fasta_filename) {
  #Opening seqinr library for handling FASTA files; make sure you have seqinr installed
  library(seqinr)
  #Reading the fasta file
  read.fasta(fasta_filename, seqtype = "AA", as.string = TRUE,
             set.attributes = FALSE)
}
upload_fasta("testfasta.txt")
upload_fasta("testfasta.txt")$A




split_peptides <- function(peptides) {
  # Opening stringr for simple string manipulation
  library(stringr)
  #Splitting peptides into individual amino acids using str_split;generates a list of lists of amino acids for each peptide
  lapply(peptides, str_split, pattern="")
}
peptides <- list(A=c("LVK","LHHIIFESMLK","DMQR","R","HR","VW"),
                 B=c("ADEFQGSMQK","IEACWQSYDVQF"),
                 C=c("MINEPFSWR","LEFHLSER","K","YDEIM"))
split_peptides(peptides)
split_peptides(peptides)$B




count_matching_masses <- function(protein_masses, sample) {
  #Virus masses is a list of masses for each protein so we use sapply to iterate over the list; sum (of TRUEs) is used to count the number of times a mass in the sample is found (%in%) among the masses of each of the proteins (virus_masses); note that masses are converted into strings (as.character) because %in% is not very reliable with numbers
  df <- as.data.frame(sapply(protein_masses, function (x)
    sum(as.character(sample) %in% as.character(x))))
  # Adding peptide_counts as the column name of the counts column
  names(df) <- "peptide_counts"
  return(df)
}
masses_list <- list(A=c(340.246, 1348.728, 530.225), B=c(1121.476,
                                                         1469.624), C=c(1160.540, 1011.511, 651.255))
sample <- c(340.246, 530.225, 1348.728)
count_matching_masses(masses_list, sample)


