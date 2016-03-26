package tests

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import simulaatio._

@RunWith(classOf[JUnitRunner])
class LiikkuvaMassaTest extends FlatSpec {

  "LiikkuvaMassa" should "calculate distance between objects correctly" in {
    val testiSimulaatio = new Simulaatio(5000, 1)
    val kappale1 = new LiikkuvaMassa(Tyyppi.Satelliitti, 100, new AvaruudenPiste(0, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale2 = new LiikkuvaMassa(Tyyppi.Satelliitti, 100, new AvaruudenPiste(1, 1, 1), new Nopeus(0, 0, 0), testiSimulaatio)

    assert(kappale1.etaisyys(kappale2) == Math.sqrt(3))
  }

  "LiikkuvaMassa" should "calculate force betweene objects correctly" in {
    val testiSimulaatio = new Simulaatio(5000, 1)
    val kappale1 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(0, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale2 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(10, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)

    assert(kappale1.vaikuttavaVoima(kappale2) == new Voima(0.1, 0, 0))
  }
  "LiikkuvaMassa" should "combine forces correctly" in {
    val testiSimulaatio = new Simulaatio(5000, 1)
    val kappale1 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(0, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale2 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(10, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale3 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(0, 10, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    testiSimulaatio.lisaaKappale(kappale1)
    testiSimulaatio.lisaaKappale(kappale2)
    testiSimulaatio.lisaaKappale(kappale3)

    assert(kappale1.summaaVoimat == new Voima(0.1, 0.1, 0))
  }

  "LiikkuvaMassa" should "tell if it is destroyed" in {
    val testiSimulaatio = new Simulaatio(5000, 1)
    val kappale1 = new LiikkuvaMassa(Tyyppi.Tahti, 100, new AvaruudenPiste(0, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale2 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(20, 0, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    val kappale3 = new LiikkuvaMassa(Tyyppi.Planeetta, 1, new AvaruudenPiste(0, 5001, 0), new Nopeus(0, 0, 0), testiSimulaatio)
    testiSimulaatio.lisaaKappale(kappale1)
    testiSimulaatio.lisaaKappale(kappale2)
    testiSimulaatio.lisaaKappale(kappale3)

    assert(kappale2.onTuhoutunut && kappale3.onTuhoutunut)
  }

}