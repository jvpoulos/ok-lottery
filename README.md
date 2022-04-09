# ok-lottery
Code and data for the paper "Gender gaps in wealth and entrepreneurship? Evidence from 1901 Oklahoma land lottery winners"


Prerequsites
------

* **R** >= 3.5.0 (tested on 4.0.2)

* package-list.R # required **R** packages

* requires X11 and png graphics capabilities. Open **R** and type `capabilities()` to check. 

Data files
------

The main data files used for the study are listed below. 

* Lawton land lottery records: `data/lawton-A.csv` and  `data/lawton-B.csv`
* El Reno land lottery records: `data/el-reno-homesteader.csv`
* Land patents data: `data/patents.csv.zip`
* [1900 Census sample](https://www.dropbox.com/s/aq7igj6sudaf73o/census-1900-100-sample.csv.zip?dl=1)
* [1910 Census sample](https://www.dropbox.com/s/tj3pkzjzjx3hqp8/census-1910-100-sample.csv.zip?dl=1)

Instructions
------

1. `cd ok-lottery`, and download Census samples into data folder:

`wget -0  data/census-1900-100-sample.csv.zip "https://www.dropbox.com/s/aq7igj6sudaf73o/census-1900-100-sample.csv.zip?dl=1"`
`wget -0  data/census-1910-100-sample.csv.zip "https://www.dropbox.com/s/tj3pkzjzjx3hqp8/census-1910-100-sample.csv.zip?dl=1"`

2. Open `code/main.R` and ensure libraries are installed. By default, links participants to census and land patent records. 

3. `chmod +x code/main.sh` and run from the command line. `code/main.R` prepares data and outputs RD plots, RD estimates, and descriptive plots. By default, it uses trained models for record linkage. 