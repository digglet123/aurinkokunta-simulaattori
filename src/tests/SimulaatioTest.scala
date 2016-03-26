package tests

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import simulaatio._

@RunWith(classOf[JUnitRunner])
class SimulaatioTest extends FlatSpec {

  "Simulaatio" should "tell if a object can be placed in the simulation" in {

    val testiSimulaatio = new Simulaatio(5000, 1)
    val kappale1 = new LiikkuvaMassa(Tyyppi.Tahti, 100, new AvaruudenPiste(0, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale2 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(0, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    testiSimulaatio.lisaaKappale(kappale1)

    assert(!testiSimulaatio.voiAsettaaKappaleen(kappale2))
  }

  "Simulaatio" should "load objects from files" in {

    val testiSimulaatio = new Simulaatio(5000, 1)
    val tiedosto = Array("T star; M 100; P 0,0,0; V 0,0,0", "T planet; M4; P 0,140,0; V 2.4,0,0", "T large planet; M4; P 0,-140,0; V -2.4,0,0")
    testiSimulaatio.lueTiedostoRivit(tiedosto)
    
    assert(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.Tahti && x.getMassa == 100 && x.paikka == new AvaruudenPiste(0,0,0) && x.nopeus == new Nopeus(0,0,0) })
    assert(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.Planeetta && x.getMassa == 4 && x.paikka == new AvaruudenPiste(0,-140,0) && x.nopeus == new Nopeus(2.4,0,0) })
    assert(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.SuurPlaneetta && x.getMassa == 4 && x.paikka == new AvaruudenPiste(0,140,0) && x.nopeus == new Nopeus(-2.4,0,0) })
  }

  "Simulaatio" should "not load objects that overlap" in {

    val testiSimulaatio = new Simulaatio(5000, 1)
    val tiedosto = Array("T star; M 100; P 0,0,0; V 0,0,0", "T planet; M4; P 0,0,0; V 0,0,0")
    testiSimulaatio.lueTiedostoRivit(tiedosto)
    
    assume(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.Tahti && x.getMassa == 100 && x.paikka == new AvaruudenPiste(0,0,0) && x.nopeus == new Nopeus(0,0,0) })
    assert(testiSimulaatio.kappaleet.size == 1)
  }
  
  "Simulaatio" should "delete destroyed objects" in {

    val testiSimulaatio = new Simulaatio(5000, 1)
    val tiedosto = Array("T star; M 100; P 0,0,0; V 0,0,0", "T planet; M4; P 35,0,0; V -5,0,0")
    testiSimulaatio.lueTiedostoRivit(tiedosto)
    assume(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.Tahti && x.getMassa == 100 && x.paikka == new AvaruudenPiste(0,0,0) && x.nopeus == new Nopeus(0,0,0) })
    assume(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.Planeetta && x.getMassa == 4 && x.paikka == new AvaruudenPiste(35,0,0) && x.nopeus == new Nopeus(-5,0,0) })
    testiSimulaatio.paivitaSimulaatio
    testiSimulaatio.paivitaSimulaatio
    assume(testiSimulaatio.kappaleet.exists { x => x.getKappaleenTyyppi == Tyyppi.Tahti && x.getMassa == 100 && x.paikka == new AvaruudenPiste(0,0,0) && x.nopeus == new Nopeus(0,0,0) })
    assert(testiSimulaatio.kappaleet.size == 1)
  }
  
}