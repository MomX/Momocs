#update
currDir <- getwd() # this should be the top level directory of the git repo
currDCF <- read.dcf("DESCRIPTION")
currVersion <- currDCF[1,"Version"]
splitVersion <- strsplit(currVersion, ".", fixed=TRUE)[[1]]
nVer <- length(splitVersion)
currEndVersion <- as.integer(splitVersion[nVer])
newEndVersion <- as.character(currEndVersion + 1)
splitVersion[nVer] <- newEndVersion
newVersion <- paste(splitVersion, collapse=".")
currDCF[1,"Version"] <- newVersion
currDCF[1, "Date"] <- strftime(as.POSIXlt(Sys.Date()), "%Y-%m-%d")
write.dcf(currDCF, "DESCRIPTION")
system("git add DESCRIPTION")
cat("Incremented package version, now", newVersion, "\n")

#### number of lines
nl <- numeric()
for (i in list.files("R", full=TRUE)){
  nl <- append(nl, length(readLines(i)))
}
cat("now", sum(nl), "lines")
