
wd = 'N:/model/GL-WDPA-MPA_v2013'
setwd(wd)

d = read.csv('tmp/WDPApoly_August2013_tbl.csv')
cat(paste(names(d), collapse=', '))
# OBJECTID_1, WDPAID, WDPA_PID, NAME, ORIG_NAME, COUNTRY, SUB_LOC, DESIG, DESIG_ENG, DESIG_TYPE, 
# IUCN_CAT, INT_CRIT, MARINE, REP_M_AREA, GIS_M_AREA, REP_AREA, GIS_AREA, STATUS, STATUS_YR, 
# GOV_TYPE, MANG_AUTH, MANG_PLAN, NO_TAKE, NO_TK_AREA, METADATAID, OBJECTID, SHAPE_Leng, Shape_Length, Shape_Area

table(d$STATUS)
#      Adopted   Designated    Inscribed Not Reported     Proposed 
#           20       170769          223          297         2073

addmargins(table(d[d$STATUS=='Designated', 'DESIG_TYPE']))
# International      National           Sum 
#         27760        143009        170769


table(as.character(d[d$STATUS=='Designated' & d$DESIG_TYPE=='International', 'DESIG_ENG']))
#                               ASEAN Heritage Park 
#                                                26 
#                Baltic Sea Protected Area (HELCOM) 
#                                               159 
#                    Marine Protected Area (CCAMLR) 
#                                                 1 
#                     Marine Protected Area (OSPAR) 
#                                                 6 
#  Ramsar Site, Wetland of International Importance 
#                                              1214 
# Site of Community Importance (Habitats Directive) 
#                                             20892 
#         Special Protection Area (Birds Directive) 
#                                              5244 
#                      UNESCO-MAB Biosphere Reserve 
#                                               218
write.csv(d[d$STATUS=='Designated',], 'tmp/wdpa_poly_designated.csv', row.names=F, na='')


table(d[d$STATUS=='Designated', 'STATUS_YR'])
#     0  1819  1838  1858  1875  1876  1881  1882  1885  1886  1887  1888  1889  1890  1892  1893 
# 35019     1     2     1     4     1     1     2     1     2     1     3     1     3     4     8 
#  1894  1895  1896  1897  1898  1899  1900  1901  1902  1903  1904  1905  1906  1907  1908  1909 
#     4     9     6     4     2     3    25     4     2     5     3     8     3    10    10    14 
#  1910  1911  1912  1913  1914  1915  1916  1917  1918  1919  1920  1921  1922  1923  1924  1925 
#     2     4    12    13     5     6    12    11    20    26    13    41    24    21    27    38 
#  1926  1927  1928  1929  1930  1931  1932  1933  1934  1935  1936  1937  1938  1939  1940  1941 
#    46    56    38    41    66    40   176   111    63    83   107   283   333   258   205   170 
#  1942  1943  1944  1945  1946  1947  1948  1949  1950  1951  1952  1953  1954  1955  1956  1957 
#    87    64    70    84    85   114   332   153   241   205   245   290   375   446   424   414 
#  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968  1969  1970  1971  1972  1973 
#   438   506   383   595   493   480   554   665   479   669   814   595   613   906   883   855 
#  1974  1975  1976  1977  1978  1979  1980  1981  1982  1983  1984  1985  1986  1987  1988  1989 
#  1178   802   999  1067  1083  1733  1220  1290  1783  2077  2490  1955  2505  2258  2341  1923 
#  1990  1991  1992  1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004  2005 
#  2578  1857  2824  2585  2908  4796  3694  3388  5495  4135  8760  7892  5926  3849  8098  5317 
#  2006  2007  2008  2009  2010  2011  2012  2013 
#  3669  3505  2984  3210  4751   613   132    25


# devise priority field for converting polygons to raster
types = c('I','N')                 ; nt = length(types)
years = c(0,1819, 1900, 2000, 2013); ny = length(years)
d = data.frame(type=rep(types, length.out=nt*ny),
               year=rep(years, each=(nt*ny)/ny)); d
d = within(d,{
  priority = c('I'=0,'N'=2)[type] + 1/(year+1)
})
d[order(d$priority, decreasing=T),]
#    type year     priority
# 2     N    0 3.0000000000
# 4     N 1819 2.0005494505
# 6     N 1900 2.0005260389
# 8     N 2000 2.0004997501
# 10    N 2013 2.0004965243
# 1     I    0 1.0000000000
# 3     I 1819 0.0005494505
# 5     I 1900 0.0005260389
# 7     I 2000 0.0004997501
# 9     I 2013 0.0004965243