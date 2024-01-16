#Assignment 2 (B. Jannuzzi)

#Task 4: Find the csv files and save as object
csv_files <- list.files(path = "Data", pattern = ".csv", recursive = TRUE)

#Task 5: Find how many files match description with length function
length(csv_files)

#Task 6: Open the wingspan_vs_mass
df <- read.csv(list.files(pattern = "wingspan_vs_mass.csv", recursive = TRUE))

#Task 7: Inspect the first 5 lines of the data
head(df, 5)

#Task 8: Find any files (recursively) in the Data/directory that begin with the letter "b".
list.files(path = "Data", pattern = "^b", recursive = TRUE)

#Task 9: Write a command that displays the first line of each of those “b” files
bfiles <- list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)

for(i in bfiles){
print(readLines(i, n = 1))
}

#Task 10: Do the same thing for all files that end in “.csv”
allcsv <- list.files(path = "Data", pattern = ".csv", recursive = TRUE, full.names = TRUE)

for(i in allcsv){
print(readLines(i, n = 1))
}