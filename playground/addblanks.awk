/^[[:blank:]]*#/ {next} # ignore comments (lines starting with #)
NF < 3 {next} # ignore lines which don't have at least 3 columns
int(log($3)*10) != prev {printf "\n"; prev=int(log($3)*10)} # print blank line
{print} # print the line
