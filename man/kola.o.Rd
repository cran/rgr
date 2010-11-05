\name{kola.o}
\docType{data}
\alias{kola.o}
\title{ Kola Project O-horizon Soil Data }
\description{
These data arise from an ecogeochemical survey undertaken by the Central Kola Expedition of Russia (CKE), the Geological Survey of Finland (GTK) and the Norwegian Geological Survey (NGU).  In 1995 a variety of soil and biological materials were collected from almost 700 sites lying between the Arctic Circle and the Barents Sea, and Longitudes 35.5 and 40 East.  This specific data set is for O-horizon soils found at 617 of the sites visited.  The data consist of an integer identifier, Universal Transverse Mercator (m) eastings and northings coordinates, 38 chemical measurements (total or near-total geochemical analyses), Loss on Ignition, soil pH and specific conductivity for the <2 mm fraction of the O-horizon (humus) soils.  The data reflect both natural biogeochemical variations and the presence of heavy industry.  Further details concerning the project, and methods of sampling and analysis can be found in Reimann et al. (1998) and the numerous papers published by the co-authors in international scientific journals.
}
\usage{kola.o}
\source{
These data are the same as in the R package mvoutlier.  However, note that the names of the spatial coordinates have been changed from XCOO and YCOO to UTME and UTMN, respectively, and COND (specific conductivity) to SC.

The full data set is available from:
\url{http://doi.pangaea.de/10.1594/PANGAEA.56279}

However, it should be noted that this is a superset containing all geochemical analyses and the spatial coordinates are recorded as Latitudes and Longitudes in the full data set.
}
\format{ A data frame containing 44 observations for 617 sites. }
\references{
Reimann, C., Ayras, M., Chekushin, V., Bogatyrev, I., Boyd, R., de Caritat, P., Dutter, R., Finne, T.E., Halleraker, J.H., Jaeger, O., Kashulina, G., Niskavaara, H., Pavlov, V., Raisanen, M.L., Strand, T. and Volden, T., 1998. A geochemical atlas of the central parts of the Barents Region. Norges Geologiske Undersokelse (NGU) Geological Survey of Norway, Trondheim, Norway. ISBN 82-7385-176-1. 745 p.
}
\keyword{ datasets }








