# forestbioeconomy
# written by Nicolas Robert, Bioeconomy Unit, European Commission Joint Research Centre
#2019-2020

Method to calculate employment in the wood-based value chain (Robert et al., 2020b). 
The script (in R 3.6) relies on Eurostat data: 
-	the labour force survey (lfsa_egan22d) as the reference dataset 	 https://ec.europa.eu/eurostat/databrowser/view/lfsa_egan22d/default/table?lang=en 
-	the national accounts employment data by industry (nama_10_a64_e)	 https://ec.europa.eu/eurostat/databrowser/view/nama_10_a64_e/default/table?lang=en  and the annual detailed enterprise statistics for industry (sbs_na_ind_r2)	 https://ec.europa.eu/eurostat/databrowser/view/sbs_na_ind_r2/default/table?lang=en to fill in the employment gaps
-	An extract of the Prodcom (table DS-066341) containing furniture products to calculate the shares of wood used in the manufacturing of furniture	 https://ec.europa.eu/eurostat/web/prodcom/data/database/  
-	the use table at purchasers’ prices (naio_10_cp16) to calculate the dependence on wood and wood-based products for each sector	 
