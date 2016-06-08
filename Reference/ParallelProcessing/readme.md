# Parallel Processing Tips 

## mclapply

Libraries required:  
parallel

## foreach & do par

Libraries required:  
doParallel  
foreach

***

#Command line/terminal tips

## Connecting to the server via command line

You can connect to the server via SSH.

1. Open a command line or terminal window
2. Type `ssh afflerbach@mazu.nceas.ucsb.edu` (replace `afflerbach` with your username)
3. Type your password
4. You're in!

To navigate to git-annex run the following:

`cd /home/shares/ohi`

cd stands for Change Directory

## Multiple tabs in your command line

You can utilize [byobu](http://byobu.co/) to create multiple tabs in your command line. To enable this all you do is type:

`byobu-enable`

To create a new tab use F2
To switch between tabs use F3 and F4

To leave a tab type `exit`

To kill a process hit control+c

## Running R scripts on the server

You can run an R script via the command line by the command `Rscript`. Below is an example of how to run the example_mclapply.R script:

`ssh afflerbach@mazu.nceas.ucsb.edu`

`cd github/ohiprep/Reference/ParallelProcessing`

`Rscript example_mclapply.R`

## Monitoring cores on the server

`htop` will open up a viewer to monitor the processes occuring on all cores on Mazu.