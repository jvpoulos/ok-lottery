# ok-lottery
Code and data for the paper ["Gender gaps in wealth and entrepreneurship? Evidence from 1901 Oklahoma land lottery winners"](https://www.nowpublishers.com/article/Details/HPE-0042)

Please cite the paper if you use this code for academic research:

```
@article{poulos2023gender,
  title={Gender Gaps in Frontier Entrepreneurship? Evidence from 1901 Oklahoma Land Lottery Winners},
  author={Poulos, Jason},
  journal={Journal of Historical Political Economy},
  volume={2},
  number={4},
  pages={611--634},
  year={2023},
  publisher={Now Publishers, Inc.}
}
```

Prerequsites
------

* **R** >= 3.5.0 (tested on 4.0.2)


Data files
------

The main data files used for the study are listed below. 

* Lawton land lottery records: `data/lawton-A.csv` and  `data/lawton-B.csv`
* El Reno land lottery records: `data/el-reno-homesteader.csv`
* Land patents data: `data/patents.csv.zip`
* [1900 Census sample](https://drive.google.com/file/d/11z90wGNBJool3mY443b8ynlT_KE5wuMe/view?usp=sharing)
* [1910 Census sample](https://drive.google.com/file/d/1nqINp1_OmR-WTAz5DZ8uWbcpsOi_Nm_v/view?usp=sharing)

Instructions
------

1. `cd ok-lottery`, and download Census samples into data folder: `wget -0  data/census-1900-100-sample.csv.zip "https://www.dropbox.com/s/aq7igj6sudaf73o/census-1900-100-sample.csv.zip?dl=1"` and `wget -0  data/census-1910-100-sample.csv.zip "https://www.dropbox.com/s/tj3pkzjzjx3hqp8/census-1910-100-sample.csv.zip?dl=1"`

2. Open `code/main.R` and ensure libraries are installed. By default, links participants to census and land patent records. 

3. `chmod +x code/main.sh` and run from the command line. `code/main.R` prepares data and outputs estimates and descriptive plots. By default, it uses trained models for record linkage. 