package simulaatio

object Tyyppi extends Enumeration{
  
  type Kappale = Value
  val Tahti, SuurPlaneetta, Planeetta, PienPlaneetta, Satelliitti = Value
  
  type Kuvakulma = Value
  val Front, Top, Left = Value
  
}
