# Authoritarian Diffusion: Master's Thesis
This is the repository for all files and data related to my master's thesis work on authoritarian diffusion. To explain it simply: *autocratic diffusion is the phenomenon whereby an increase in autocratic states, may increase the chance that other states become autocratic*. My thesis looks at the influence China, as a big authoritarian country, has on its partner countries through what has been called leverage and linkages (Levitsky and Way 2006).

## R-scripts
My data consist of several R-scripts with different applications. The main script is called *data.R*, and gathers and contains all the data necessary to build the *base* dataset which will be used in constructiong the regression models. The regression models are included in the script called *fixest.R* (name is subject to change). I am running my models as fixed-effects and using the R-package *fixest* (Bergé 2018) to be able to do this. Included is also several different scripts for making maps and figures.

## Data
In the above-mentioned *data.R* file I include data from several different sources, and here I explain my variables and where I have gathered them.

Variables included:
| Variable name | Description | Type | Origin |
| ------------- | ----------- | ---- | ------ |
| Country       | Country name | String | V-dem v14 |
| iso3c         | ISO3c code | String | V-Dem v14 |
| year          | Year | Numeric (1994-2023 | V-dem v14 |
| freedom       | Freedom of Expression and Alternative Sources of Information index | Continuous between 0 and 1 | V-dem v14 `v2x_freexp_altinf` |
| internet      | Internet censorship | Continuous between -4.09 and 2.29 | V-dem v14 `v2mecenefi` |
| fbic          | Dyadic FBIC index-score between a country and China 1 | Continuous between 0 and 1 | FBIC `fbic` |
| regime        | Regimes of the world index | Ordinal variable 0-3 | V-dem v 14 `v2x_regime` |
| west_1        | Restricted West | A restricted sample of Western countries: Andorra, Australia, Austria, Belgium, Canada, Denmark, Finland, France, Germany, Greece, Iceland, Ireland, Israel, Italy, Japan, Liechtenstein, Luxembourg, Malta, Monaco, Netherlands, New Zealand, Norway, Portugal, San Marino, Spain, Sweden, Switzerland, United Kingdom, United States | Continuous between 0 and 4.85 | My own definition |
| west_2        | An expanded sample of Western countries: Andorra, Australia, Austria, Belgium, Bulgaria, Canada, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Israel, Italy, Japan, Latvia, Liechtenstein, Lithuania, Luxembourg, Malta, Netherlands, New Zealand, Norway, Poland, Portugal, Romania, San Marino, Slovakia, Slovenia, South Korea, Spain, Sweden, Switzerland, Taiwan, United Kingdom, United States | Continuous between 0 and 5.32 | My own definition |
| gdppc         | GDP per capita in current US$ averaged from data produced by the World Bank, IMF, and UN | Continuous between 78.91 and 134167.99 | Created from data from WDI, WEO, and UNSD `NY.GDP.PCAP.CD, NGDPDPC` |
| gdppc_log     | GDP per capita (log-transformed) | Logged version of `gdppc` | Continuous between 4.37 and 11.81 | See `gdppc` |
| rents         | Natural resource rents (% of GDP) | Continuous between 0 and 88.59 | WDI `NY.GDP.TOTL.RT.ZS` |
| oil           | Oil rents (% of GDP) | Continuous between 0 and 82.78 | WDI `NY.GDP.PETR.RT.ZS` |
| gas           | Gas rents (% of GDP) | Continuous between 0 and 55.01 | WDI `NY.GDP.NGAS.RT.ZS` |
| oda           | Official Development Assistance (% of GNI) | Continuous between 0 and 113.13 | OECD `OECD.DCD.FSD,DSD_DAC2` |
| consolidated_democracy | Electoral Democracy Index (EDI) score equal to or greater than .8 for 15 consecutive years | Binary | V-dem (Own calculation based on `v2x_polyarchy`) |

## References
> Levitsky, S. and L. A. Way (2006, July). Linkage versus Leverage. Rethinking the International Dimension of Regime Change. Comparative Politics 38 (4), 379–400. Publisher: Comparative Politics, Ph.D. Programs in Political Science, City University of New York.

> Bergé, L. (2018). “Efficient estimation of maximum likelihood models with multiple fixed-effects: the R package FENmlm.” CREA Discussion Papers. 
