package simulaatio
import scala.collection.mutable.Buffer
import scala.collection.mutable.Queue
import java.io.IOException
import scala.io.Source
import java.io.File

class Simulaatio(koko: Int, simulaatioAskel: Int) {

  val getKoko = this.koko
  var getSimulaatioAskel = this.simulaatioAskel
  val gravitaatioVakio: Double = 10
  val kappaleet: Buffer[LiikkuvaMassa] = Buffer()
  val rajahdyspisteet: Queue[(AvaruudenPiste, Int)] = Queue()
  var nykyinenTiedosto: Array[String] = try { Source.fromFile("src/Presets/2-planet-system.txt").getLines.toArray } catch { case _: Throwable => { Array("") } }
  var nykyinenTiedostoRikki: Boolean = false
  val tiedostoLista = new File("src/Presets").listFiles.map(_.getName).toList.filter(_.takeRight(4) == ".txt")

  //Alustaa simulaation tyhjentamalla sen kappaleet ensin ja lukemalla uudet kappaleiden sijainnit nykyisesta tiedostosta.
  def alusta {
    kappaleet.clear
    lueTiedostoRivit(nykyinenTiedosto)
  }

  //Tarkistaa voiko kappaleen lisata simulaatioon perustuen sen paikkaan avaruudessa.
  def voiAsettaaKappaleen(asetettavaKappale: LiikkuvaMassa): Boolean = {
    var voiAsettaa = true
    for (kappale <- kappaleet) {
      try {
        if (asetettavaKappale.sade + kappale.sade >= asetettavaKappale.etaisyys(kappale)) {
          voiAsettaa = false
        }
      } catch {
        case _: Throwable => false
      }
    }
    voiAsettaa
  }

  //Lukee tiedoston rivit, ja lisaa niiden perusteella kappaleita simulaatioon. Parametriksi annetaan tiedosto riveittain array-kokoelmassa.
  def lueTiedostoRivit(tiedosto: Array[String]) {
    var rikki: Boolean = false
    for (rivi <- tiedosto) {
      if (!rivi.trim.isEmpty) {
        val riviSolut = rivi.split(";")
        var kappaleenTyyppi: Tyyppi.Kappale = null
        var massa: Double = 0
        var aloitusPaikka: AvaruudenPiste = null
        var aloitusNopeus: Nopeus = null
        for (solu <- riviSolut) {
          val teksti = solu.toLowerCase.replaceAll("\\s", "")
          teksti.head match {
            case 't' => {
              teksti.drop(1) match {
                case "star" => kappaleenTyyppi = Tyyppi.Tahti
                case "planet" => kappaleenTyyppi = Tyyppi.Planeetta
                case "smallplanet" => kappaleenTyyppi = Tyyppi.PienPlaneetta
                case "largeplanet" => kappaleenTyyppi = Tyyppi.SuurPlaneetta
                case "satellite" => kappaleenTyyppi = Tyyppi.Satelliitti
                case _ => {
                  rikki = true
                }
              }
            }
            case 'm' => {
              try {
                massa = teksti.drop(1).toDouble
              } catch {
                case _: Throwable =>
                  rikki = true
              }
            }
            case 'p' => {
              try {
                val values: Array[Double] = teksti.drop(1).split(",").map(x => x.toDouble)
                aloitusPaikka = new AvaruudenPiste(values(0), -values(1), values(2))
              } catch {
                case _: Throwable =>
                  rikki = true
              }
            }
            case 'v' => {
              try {
                val values: Array[Double] = teksti.drop(1).split(",").map(x => x.toDouble)
                aloitusNopeus = new Nopeus(values(0), -values(1), values(2))
              } catch {
                case _: Throwable =>
                  rikki = true
              }
            }
            case _ =>
          }
        }
        if (kappaleenTyyppi != null && aloitusPaikka != null && aloitusNopeus != null) {
          val lisattavaKappale = new LiikkuvaMassa(kappaleenTyyppi, Math.min(Math.max(massa, 1), 500), aloitusPaikka, aloitusNopeus, this)
          if (voiAsettaaKappaleen(lisattavaKappale)) {
            lisaaKappale(lisattavaKappale)
          }
        } else {
          rikki = true
        }
      }
    }
    nykyinenTiedostoRikki = rikki
  }

  //Lisaa simulaatioon pisteen, jossa tapahtuu rajahdys ja parittaa taman rajahdys animaation ruudun numeron kanssa. 
  def lisaaRajahdys(paikka: AvaruudenPiste) {
    rajahdyspisteet += ((paikka, 0))
  }
  //Poistaa kappaleen simulaatiosta
  def poistaKappale(kappale: LiikkuvaMassa) {
    kappaleet -= kappale
  }
  //Lisaa kappaleen simulaatioon
  def lisaaKappale(kappale: LiikkuvaMassa) {
    this.kappaleet += kappale
  }
  //Paivittaa simulaation yhden iteraation verran.
  def paivitaSimulaatio {
    kappaleet.foreach(x => x.paivita)
    kappaleet.filter(x => x.onTuhoutunut).foreach(x => poistaKappale(x))
  }
}