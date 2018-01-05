
# CMD somehow lacks a basename() / dirname() feature, forcing you to resort to
# a for loop to parse out the current working directory. Rather than deal with that mess
# we'll just call R, which we know is there...
https://superuser.com/questions/160702/get-current-folder-name-by-a-dos-command

http://www.robvanderwoude.com/ntfor.php
https://ss64.com/nt/syntax-args.html
SET current_dir=%cd%
set Rproj="$(basename "$(pwd)").Rproj"
## NOT TESTED
# Writes a standard Rproj file to workingDirName.Rproj
(
echo Version: 1.0
echo:
echo RestoreWorkspace: Default
echo SaveWorkspace: Default
echo AlwaysSaveHistory: Default
echo:
echo EnableCodeIndexing: Yes
echo UseSpacesForTab: Yes
echo NumSpacesForTab: 2
echo Encoding: UTF-8
echo:
echo RnwWeave: Sweave
echo LaTeX: pdfLaTeX
echo:
echo AutoAppendNewline: Yes
echo StripTrailingWhitespace: Yes
echo:
) >  %Rproj%


