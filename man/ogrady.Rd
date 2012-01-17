\name{ogrady}
\alias{ogrady}
\docType{data}
\title{ Lithogeochemical Data Set from the O'Grady Pluton, NWT, 1970 }
\description{
A subset of data, for the O'Grady pluton, NWT, (NTS map sheet 105I) from a regional lithogeochemical survey undertaken by the Geological Survey of Canada between 1969 and 1972 of Cretaceous-age granitoid plutons northeast of the Tintina Trench in the Yukon and adjoining NWT.  Samples were collected in pairs from each site and a sub-sample ground to <100 mesh.  Major and, some trace, element analyses were undertaken by direct reading optical spectroscopy (OES) either after a Li-Tetraborate (Li-T) fusion, or directly, other major elements were determined by AAS following HNO3 dissolution of the fusion product.  Other trace-elements were determined by atomic absorption spectrophotometry after a HF-HClO4 digestion, or colorimetry (Col) after an alkaline fusion (AF).  For Na, K, Fe, Mo and W detection limits (DLs) were 0.05, 0.1 and 0.1 \%, and 0.5 and 2 mg/kg, respectively; <DL observations are represented by values of 0.02, 0.05, 0.05 \% and 0.2 and 1 mg/kg, respectively.
}
\usage{data(ogrady)}
\format{
  A data frame with 110 observations for the following 24 variables:
  \describe{
    \item{ID}{ a numeric vector, part of the unique GSC sample number. }
    \item{E}{ UTM Eastings (m) for the sample site (UTM Zone 9). }
    \item{N}{ UTM Northings (m) for the sample site (UTM Zone 9). }
    \item{Lith}{ field name for the sampled lithology. } 
    \item{Si}{ silicon (\%) in granitoid (Li-T OES). }
    \item{Al}{ aluminium (\%) in granitoid (Li-T OES). }
    \item{Fe}{ iron (\%) in granitoid (Li-T HNO3 AAS). }
    \item{Mg}{ magnesium (\%) in granitoid (Li-T OES). }
    \item{Ca}{ calcium (\%) in granitoid (Li-T OES). }
    \item{Na}{ sodium (\%) in granitoid (Li-T HNO3 AAS). }
    \item{K}{ potassium (\%) in granitoid (Li-T HNO3 AAS). }
    \item{Ti}{ titanium (mg/kg) in granitoid (Li-T OES). }
    \item{Mn}{ manganese (mg/kg) in granitoid (Li-T OES). }
    \item{Ba}{ barium (mg/kg) in granitoid (Li-T OES). }
    \item{Zn}{ zinc (mg/kg) in granitoid (HF-HClO4 AAS). }
    \item{Cu}{ copper (mg/kg) in granitoid (HF-HClO4 AAS). }
    \item{Pb}{ lead (mg/kg) in granitoid (HF-HClO4 AAS). }
    \item{Mo}{ molybdenum (mg/kg) in granitoid (AF Col). }
    \item{W}{ tungsten (mg/kg) in granitoid. (AF Col). }
    \item{U}{ uranium (mg/kg) in granitoid (HF-HClO4 Fluorimetry). }
    \item{Be}{ beryllium (mg/kg) in granitoid (OES). }
    \item{V}{ vanadium (mg/kg) in granitoid (OES). }
    \item{Sn}{ tin (mg/kg) in granitoid (OES). }
    \item{Zr}{ zirconium (mg/kg) in granitoid (OES). }
  }
}
\source{
Garrett, R.G., 1992. Lithogeochemical data release, major and trace elements in Cretaceous granitoid rocks in the Yukon Territory and adjoining parts of the N.W.T. (95E, L, 105H, I, J, K, L, M, N, O, P, 106D, 115P, 116D). Geological Survey of Canada Open File 2479, digital data. 
}
\references{
Garrett, R.G., 1972. Regional geochemical study of Cretaceous acidic igneous rocks in the northern Canadian Cordillera as a tool for broad mineral exploration.  in Proc. 4th International Geochemical Exploration Symp., Geochemical Exploration 1972 (Ed. M.J. Jones). Institute of Mining and Metallurgy, London, pp. 203-219.

Garrett, R.G., 1988. IDEAS - An interactive computer graphics tool to assist the exploration geochemist. In Current Research Part F, Geological Survey of Canada Paper 88-1F, pp. 1-13.
}
\keyword{datasets}
