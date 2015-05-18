Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data (NSIDC-0051)


DATA ORGANIZATION
Data are in the following FTP directory: 
ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/

Within the final-gsfc directory are north and south directories 
that contain data files, and a browse directory that contains browse image files. 
Daily and monthly data are further separated into directories named daily and 
monthly. For final daily data, there is also one directory for each year of 
available data. For example, all of the north daily data for 1990 are in a 
directory named /nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/1990/. 

The directory structure is illustrated below; not all directory levels are shown. 
The structure for each south directory matches that of the corresponding north 
directory. Each browse directory is divided into a structure that reflects that 
of the data. In this illustration, the year directories underneath final-gsfc 
are representative placeholders; there are actually many such directories, each 
named for the year of data it contains, such as 1987, 2000, etc.


/nsidc0051_gsfc_nasateam_seaice
. . /final-gsfc
. . . . /browse
. . . . . . /north
. . . . . . . . /daily
. . . . . . . . . . /year
. . . . . . . . /monthly
. . . . . . /south
. . . . /north
. . . . . . /daily
. . . . . . . . /year
. . . . . . /monthly
. . . . /south


DOWNLOADING DATA VIA FTP
You may download data via your FTP program of choice. Our FTP server 
allows you to download a recursive set of directories with a single 
FTP command ("tar-on-the-fly"), simply by appending the ".tar" extension 
to a directory name in the FTP "get" command. For example, using a 
command-line FTP program, you can download all of the north data using the 
following commands:

%ftp sidads.colorado.edu 
     (login as anonymous, and use your e-mail address as password)
ftp>cd /pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc
ftp>get north.tar

This command will generate a "tar" file on your local disk containing
everything under the subdirectory "north".

In most Web browsers, you can also use the tar-on-the-fly capability
by simply appending the .tar extension to the directory name in the
FTP address. For example, to download all of the north data, enter the 
following FTP address in your browser's location/address field:

ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north.tar

The browser should prompt for where to save the "tar" file.


DOCUMENTATION
For complete documentation and more information about data access, please see:

http://nsidc.org/data/nsidc-0051.html

We recommend that you read the complete documentation in detail before working 
with the data.


REGISTRATION
If you wish to be notified of updates or corrections to these data,
please register with NSIDC User Services by sending e-mail to:

    nsidc@nsidc.org

Identify yourself as a user of "Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS 
Passive Microwave Data (NSIDC-0051)." Include your name, e-mail address, postal address, and 
telephone number.

If you have questions, please contact NSIDC User Services.

CONTACT INFORMATION:
User Services
National Snow and Ice Data Center
CIRES, 449 UCB
University of Colorado
Boulder, CO USA 80309-0449
Phone: +1 303-492-6199
Fax: +1 303-492-2468
E-mail: nsidc@nsidc.org