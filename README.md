# IeDEA-DES QA Checks

Please see the [downloads](https://github.com/IeDEA/qa-checks-r/downloads) section for documentation of the exceptions that are currently implemented. You can clone this repository if you are familiar with Git and GitHub. Alternatively, click on the cloud button with the word `zip` to download a zip archive of the most current version of the scripts.

The major directions for running these scripts:

1. Install RStudio (you will also need to install the `latticeExtra` and `brew` packages)
2. Place the programs and the tables that correspond to the IeDEA Data Exchange Standard (`tblBAS.csv` `tblLAB_CD4.csv`, `tblLAB_RNA.csv`, `tblART.csv`. etc...) as csv files in the `input` folder.  
3. Open `tbl_checks.R` with RStudio
4. Change working directory to source of files/data ( _Session -> Set Working Directory -> To Source File Location_ ).
5. Click on `Source` or `CTRL+SHIFT+S`. This will traverse all the csv files in the `input` directory and capture every exception (error) that these scripts look for.
6. If successful, a very large file with a table that includes all the exceptions will also be created in the `output` folder.
7. To generate a summary report of all the exceptions that were captured in that file, you will need to open the file `code/summarize_exceptions.R` and also source it.
8. If successful, a few other files will be created in the `output` directory. The html file `output/summary_report.html` will give you a nice view that you can read in your browser.
