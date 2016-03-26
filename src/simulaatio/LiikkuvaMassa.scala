package simulaatio
import scala.collection.mutable.Buffer
import scala.collection.mutable.Queue
import Tyyppi._

class LiikkuvaMassa(kappaleenTyyppi: Kappale, massa: Double, lahtoPaikka: AvaruudenPiste, alkuNopeus: Nopeus, simulaatio: Simulaatio) {

  val getKappaleenTyyppi = this.kappaleenTyyppi
  val getMassa: Double = this.massa
  val polku: Queue[AvaruudenPiste] = Queue(lahtoPaikka)
  var paikka: AvaruudenPiste = lahtoPaikka
  var nopeus: Nopeus = alkuNopeus
  var onJaadytetty: Boolean = false

  //Asettaa kappaleen sateen tyypin mukaan.
  val sade = kappaleenTyyppi match {
    case Tyyppi.Tahti         => 20
    case Tyyppi.SuurPlaneetta => 10
    case Tyyppi.Planeetta     => 5
    case Tyyppi.PienPlaneetta => 3
    case Tyyppi.Satelliitti   => 1
  }
  
  //Asettaa kappaleen tilaan jossa muut simulaation kappaleet eivat vaikuta siihen.
  def jaadyta {
    onJaadytetty = true
  }
  
  //Asettaa kappaleen tilaan jossa muut simulaation kappaleet voivat vaikuttaa siihen.
  def aktivoi {
    onJaadytetty = false
  }

  //Tarkastaa onko kappale tuhoutunut
  def onTuhoutunut: Boolean = {
    var tuhoutunut = false
    for (kappale <- simulaatio.kappaleet) {
      if (kappale != this && etaisyys(kappale) <= this.sade + kappale.sade) {
        kappaleenTyyppi match {
          case Tyyppi.Tahti => {
            if (kappale.getKappaleenTyyppi == Tyyppi.Tahti) {
              tuhoutunut = true
              simulaatio.lisaaRajahdys(this.paikka)
            }
          }
          case _ => {
            if (kappale.getKappaleenTyyppi != Tyyppi.Satelliitti) {
              tuhoutunut = true
              if (this.kappaleenTyyppi != Tyyppi.Satelliitti) {
                simulaatio.lisaaRajahdys(this.paikka)
              }
            }
          }
        }
      }
    }
    if(Math.max(Math.abs(this.paikka.x),Math.max(Math.abs(this.paikka.y), Math.abs(this.paikka.z))) > simulaatio.getKoko/2 ){
        tuhoutunut = true
    }
    return tuhoutunut
  }

  //Laskee etaisyyden toiseen kappaleeseen. Ottaa parametriksi toisen simulaation kappaleen.
  def etaisyys(toinen: LiikkuvaMassa): Double = {
    Math.sqrt(Math.pow(this.paikka.x - toinen.paikka.x, 2)
      + Math.pow(this.paikka.y - toinen.paikka.y, 2)
      + Math.pow(this.paikka.z - toinen.paikka.z, 2))
  }

  //Laskee vaikuttavan voiman itsensa ja toisen kappaleen valilla. Ottaa parametriksi toisen simulaation kappaleen. Kutsuu etaisyys funktiota.
  def vaikuttavaVoima(toinen: LiikkuvaMassa): Voima = {
    val skalaari = simulaatio.gravitaatioVakio * this.getMassa * toinen.getMassa / Math.pow(etaisyys(toinen), 2)
    val kokonaisuus = Math.abs((toinen.paikka.x - this.paikka.x)) +
      Math.abs((toinen.paikka.y - this.paikka.y)) +
      Math.abs((toinen.paikka.z - this.paikka.z))
    val suhdeX = (toinen.paikka.x - this.paikka.x) / kokonaisuus
    val suhdeY = (toinen.paikka.y - this.paikka.y) / kokonaisuus
    val suhdeZ = (toinen.paikka.z - this.paikka.z) / kokonaisuus
    new Voima(suhdeX * skalaari, suhdeY * skalaari, suhdeZ * skalaari)
  }

  //Summaa keskenaan kaikki kappaleeseen vaikuttavat voimat ja palauttaa niiden summavektorin. Kutsuu vaikuttavaVoima funktiota.
  def summaaVoimat(): Voima = {
    val voimat: Buffer[Voima] = Buffer()
    var voimaX: Double = 0
    var voimaY: Double = 0
    var voimaZ: Double = 0
    for (kappale <- this.simulaatio.kappaleet) {
      this.kappaleenTyyppi match {
        case Tyyppi.Tahti => {
          if (kappale != this && kappale.getKappaleenTyyppi == Tyyppi.Tahti && !kappale.onJaadytetty) {
            voimat += vaikuttavaVoima(kappale)
          }
        }
        case _ => {
          if (kappale != this && kappale.getKappaleenTyyppi != Tyyppi.Satelliitti && !kappale.onJaadytetty) {
            voimat += vaikuttavaVoima(kappale)
          }
        }
      }
    }
    for (voima <- voimat) {
      voimaX += voima.x
      voimaY += voima.y
      voimaZ += voima.z
    }
    new Voima(voimaX, voimaY, voimaZ)
  }

  //Paivittaa kappaleen nopeuden ja paikan yhden iteraation jalkeen. Kutsuu summaaVoimat funktiota.
  def paivita {
    if (!onJaadytetty) {
      val kokonaisVoima = summaaVoimat
      nopeus = new Nopeus(nopeus.x + (kokonaisVoima.x / massa), nopeus.y + (kokonaisVoima.y / massa), nopeus.z + (kokonaisVoima.z / massa))
      paikka = new AvaruudenPiste(paikka.x + nopeus.x + 0.5 * Math.pow((kokonaisVoima.x / massa), 2),
        paikka.y + nopeus.y + 0.5 * Math.pow((kokonaisVoima.y / massa), 2),
        paikka.z + nopeus.z + 0.5 * Math.pow((kokonaisVoima.z / massa), 2))
      polku += paikka
      if (polku.size > 300) {
        polku.dequeue
      }
    }
  }

}
