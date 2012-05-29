kOO
===

R package: kOO project

- start of the project 24/05/2012 -

The R package to be built aims at implementing what I did in a few scripts 
for the preparation of the papers I published in CaGEO and TGIS (see below).
Basically this is related to the computation of the distribution of k co-occurrences of spatial events 
(generalising the contiguity distributions - 2 co-occurrences at distance 0) to derive 
spatial clustering statistics (mainly using the Shannon entropy, then called the k-spatial entropy) 
and methods linked to this: SOOk, SelSOOk (see caGEO paper) and scankOO (see TGIS).
Another method (CAkOO) performing a k-Correspondence Analysis (i.e. on a multiway table with k entries) on 
the contingencies of co-occurrenceshas been already "published in my JSS paper about another R package: PTAk.

- At the moment the basic scripts I have are based on points and use the R package spatstat, but I would like also 
to extend to any geometry type.

Interested to collaborate on this?
===

Some details in: 
===

Leibovici, D.G. Bastin, L. Anand, S. Hobona, G. and Jackson, M (2011) "Spatially Clustered Associations in Health 
            related geospatial data". Transactions in GIS, 15(3): 347-364 (June-July 2011)
            
Leibovici D.G. Bastin L,. and Jackson M. (2011) " Higher-Order Co-occurrences for Exploratory Point Pattern Analysis 
          and Decision Tree Clustering on Spatial Data." Computers & Geosciences: 37(3): 382-389 
          
Leibovici, D.G. (2010) "Spatio-temporal Multiway Decomposition using Principal Tensor Analysis on k-modes: the 
          R package PTAk." Journal of Statistical Software, 34(10), 1-34
