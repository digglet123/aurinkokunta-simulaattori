package simulaatio
import scala.swing._
import BorderPanel.Position._
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import event._
import Tyyppi._
import scala.io.Source
import Dialog._

object SimulaatioNakyma extends SimpleSwingApplication {

  val simulaatio = new Simulaatio(5000, 0) { alusta }
  var nakyma: Kuvakulma = Tyyppi.Front
  var ajossa = false
  var naytaPolut = true
  var naytaRajahdykset = true
  val lahetysMassaSaadin = new Slider {
    min = 1
    max = 100
    minorTickSpacing = 1
    majorTickSpacing = 10
    paintTicks = true
    snapToTicks = true
    value = 1
  }
  val nopeusSaadin = new Slider {
    min = 0
    max = 10
    majorTickSpacing = 1
    paintTicks = true
    snapToTicks = true
    value = 1
  }
  var lahetysTyyppi: Tyyppi.Kappale = Tyyppi.Satelliitti
  var simulaationLeveys: Int = _
  var simulaationKorkeus: Int = _

  //Planeettojen grafiikka
  val aurinkoKuva: BufferedImage = ImageIO.read(new File("src/Kuvat/sun.png"))
  val planeettaKuva: BufferedImage = ImageIO.read(new File("src/Kuvat/planet.png"))
  val suurPlaneettaKuva: BufferedImage = ImageIO.read(new File("src/Kuvat/planet_large.png"))
  val pienPlaneettaKuva: BufferedImage = ImageIO.read(new File("src/Kuvat/planet_small.png"))
  //Planeettojen grafiikka

  //Simulaatioruutu jossa piirretaan kaikki simulaatiossa tapahtuva.
  val simulaatioRuutu = new Panel with Runnable {

    background = Color.black
    var simulaatioAjo: Thread = new Thread(this)
    var asettamassa = false
    var hiirenPaikka0: Int = _
    var hiirenPaikka1: Int = _

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)

      //Piirtaa kappaleiden liikkumat polut ruudulle jos kayttaja niin haluaa. Piirtotapa riippuu katselukulasta.
      def piirraPolku(kappale: LiikkuvaMassa, pituus: Int) {
        if (naytaPolut && kappale.getKappaleenTyyppi != Tyyppi.Satelliitti) {
          val polku = kappale.polku.reverse
          for (i <- 0 until Math.min(polku.size - 1, pituus)) {
            val (start0, start1, end0, end1) = nakyma match {
              case Front => (polku(i).x.toInt, polku(i).y.toInt, polku(i + 1).x.toInt, polku(i + 1).y.toInt)
              case Top   => (polku(i).x.toInt, polku(i).z.toInt, polku(i + 1).x.toInt, polku(i + 1).z.toInt)
              case Left  => (polku(i).z.toInt, polku(i).y.toInt, polku(i + 1).z.toInt, polku(i + 1).y.toInt)
            }
            g.setPaint(Color.gray)
            g.drawLine(start0 + simulaationLeveys / 2, start1 + simulaationKorkeus / 2, end0 + simulaationLeveys / 2, end1 + simulaationKorkeus / 2)
          }
        }
      }

      //Piirtaa tahtaysvektorin simulaatioon asetettavalle satelliitille.
      if (!simulaatio.kappaleet.isEmpty && asettamassa) {

        val koord0 = nakyma match {
          case Tyyppi.Left => simulaatio.kappaleet.last.paikka.z.toInt + simulaationLeveys / 2
          case _           => simulaatio.kappaleet.last.paikka.x.toInt + simulaationLeveys / 2
        }
        val koord1 = nakyma match {
          case Tyyppi.Top => simulaatio.kappaleet.last.paikka.z.toInt + simulaationKorkeus / 2
          case _          => simulaatio.kappaleet.last.paikka.y.toInt + simulaationKorkeus / 2
        }
        val erotus0: Int = koord0 - (hiirenPaikka0 + simulaationLeveys / 2)
        val erotus1: Int = koord1 - (hiirenPaikka1 + simulaationKorkeus / 2)
        g.drawLine(koord0, koord1, koord0 + erotus0, koord1 + erotus1)
      }

      //Piirtaa kappaleet ruudulle jarjestyksessa kauimmasta lahimpaan katsojan nakokulmasta.
      for (
        kappale <- simulaatio.kappaleet.sortBy { kappale =>
          nakyma match {
            case Tyyppi.Front => kappale.paikka.z
            case Tyyppi.Top   => -kappale.paikka.y
            case Tyyppi.Left  => -kappale.paikka.x
          }
        }
      ) {
        val koord0 = nakyma match {
          case Tyyppi.Left => kappale.paikka.z.toInt + simulaationLeveys / 2
          case _           => kappale.paikka.x.toInt + simulaationLeveys / 2
        }
        val koord1 = nakyma match {
          case Tyyppi.Top => kappale.paikka.z.toInt + simulaationKorkeus / 2
          case _          => kappale.paikka.y.toInt + simulaationKorkeus / 2
        }
        val suurennusKerroin = nakyma match {
          case Tyyppi.Front => Math.pow(1.002, kappale.paikka.z)
          case Tyyppi.Top   => Math.pow(1.002, -kappale.paikka.y)
          case Tyyppi.Left  => Math.pow(1.002, -kappale.paikka.x)
        }
        val halkaisija: Int = (2 * kappale.sade * suurennusKerroin).toInt
        piirraPolku(kappale, halkaisija / 2 + 30)

        kappale.getKappaleenTyyppi match {
          case Tyyppi.Tahti => {
            g.drawImage(aurinkoKuva, koord0 - halkaisija / 2, koord1 - halkaisija / 2, halkaisija, halkaisija, null)
          }
          case Tyyppi.SuurPlaneetta => {
            g.drawImage(suurPlaneettaKuva, koord0 - halkaisija / 2, koord1 - halkaisija / 2, halkaisija, halkaisija, null)
          }
          case Tyyppi.Planeetta => {
            g.drawImage(planeettaKuva, koord0 - halkaisija / 2, koord1 - halkaisija / 2, halkaisija, halkaisija, null)
          }
          case Tyyppi.PienPlaneetta => {
            g.drawImage(pienPlaneettaKuva, koord0 - halkaisija / 2, koord1 - halkaisija / 2, halkaisija, halkaisija, null)
          }
          case Tyyppi.Satelliitti => {
            g.setPaint(Color.white)
            g.fillOval(koord0 - 1, koord1 - 1, 2, 2)
          }
        }
      }

      //Piirtaa rajahdykset ruudulle jos kayttaja niin haluaa.
      if (naytaRajahdykset) {
        for (i <- 0 until simulaatio.rajahdyspisteet.size) {
          val koord0 = nakyma match {
            case Tyyppi.Left => simulaatio.rajahdyspisteet(i)._1.z.toInt + simulaationLeveys / 2
            case _           => simulaatio.rajahdyspisteet(i)._1.x.toInt + simulaationLeveys / 2
          }
          val koord1 = nakyma match {
            case Tyyppi.Top => simulaatio.rajahdyspisteet(i)._1.z.toInt + simulaationKorkeus / 2
            case _          => simulaatio.rajahdyspisteet(i)._1.y.toInt + simulaationKorkeus / 2
          }
          val suurennusKerroin = nakyma match {
            case Tyyppi.Front => Math.pow(1.002, simulaatio.rajahdyspisteet(i)._1.z)
            case Tyyppi.Top   => Math.pow(1.002, -simulaatio.rajahdyspisteet(i)._1.y)
            case Tyyppi.Left  => Math.pow(1.002, -simulaatio.rajahdyspisteet(i)._1.x)
          }
          val path = "src/Kuvat/Rajahdys/rajahdys_" + simulaatio.rajahdyspisteet(i)._2 + ".png"
          val rajahdysKuva: BufferedImage = ImageIO.read(new File(path))
          val koko = (100 * suurennusKerroin).toInt
          g.drawImage(rajahdysKuva, koord0 - koko / 2, koord1 - koko / 2, koko, koko, null)
          simulaatio.rajahdyspisteet(i) = ((simulaatio.rajahdyspisteet(i)._1, simulaatio.rajahdyspisteet(i)._2 + 1))
        }

        while (!simulaatio.rajahdyspisteet.isEmpty && simulaatio.rajahdyspisteet.head._2 > 119) {
          simulaatio.rajahdyspisteet.dequeue
        }
      }
      else{
        simulaatio.rajahdyspisteet.clear()
      }
    }

    //Simulaation ajo metodi, joka paivittaa simulaatiota automaattisesti.
    def run() {
      //Paivittaa simulaatiota jos se on asetettu ajettavaksi
      while (true) {
        if (ajossa) {
          for (i <- 0 until simulaatio.getSimulaatioAskel) {
            simulaatio.paivitaSimulaatio
          }
        }
        //Kuuntelee hiiren liikkeita ja napaytyksia
        listenTo(mouse.moves, mouse.clicks)
        reactions += {

          case e: MousePressed => {
            val koord0 = e.point.x - simulaationLeveys / 2
            val koord1 = e.point.y - simulaationKorkeus / 2
            val piste: AvaruudenPiste = nakyma match {
              case Tyyppi.Front => new AvaruudenPiste(koord0, koord1, 0)
              case Tyyppi.Top   => new AvaruudenPiste(koord0, 0, koord1)
              case Tyyppi.Left  => new AvaruudenPiste(0, koord1, koord0)
            }
            val lisattavaKappale = new LiikkuvaMassa(lahetysTyyppi, lahetysMassaSaadin.value, piste, new Nopeus(0, 0, 0), simulaatio)
            if ((simulaatio.kappaleet.isEmpty || !simulaatio.kappaleet.last.onJaadytetty) && simulaatio.voiAsettaaKappaleen(lisattavaKappale)) {
              simulaatio.lisaaKappale(lisattavaKappale)
              simulaatio.kappaleet.last.jaadyta
            }
          }

          case e: MouseDragged => {
            if (simulaatio.kappaleet.isEmpty || simulaatio.kappaleet.last.onJaadytetty) {
              asettamassa = true
              hiirenPaikka0 = e.point.x - simulaationLeveys / 2
              hiirenPaikka1 = e.point.y - simulaationKorkeus / 2
            }
          }

          case e: MouseReleased => {
            if (!simulaatio.kappaleet.isEmpty) {
              simulaatio.kappaleet.last.aktivoi
              if (asettamassa) {
                val koord0 = nakyma match {
                  case Tyyppi.Left => simulaatio.kappaleet.last.paikka.z
                  case _           => simulaatio.kappaleet.last.paikka.x
                }
                val koord1 = nakyma match {
                  case Tyyppi.Top => simulaatio.kappaleet.last.paikka.z
                  case _          => simulaatio.kappaleet.last.paikka.y
                }
                simulaatio.kappaleet.last.nopeus = nakyma match {
                  case Tyyppi.Front => new Nopeus((koord0 - hiirenPaikka0) / 30, (koord1 - hiirenPaikka1) / 30, 0)
                  case Tyyppi.Top   => new Nopeus((koord0 - hiirenPaikka0) / 30, 0, (koord1 - hiirenPaikka1) / 30)
                  case Tyyppi.Left  => new Nopeus(0, (koord1 - hiirenPaikka1) / 30, (koord0 - hiirenPaikka0) / 30)
                }
              }
            }
          }

        }

        if (!simulaatio.kappaleet.isEmpty) {
          if (!simulaatio.kappaleet.last.onJaadytetty) {
            asettamassa = false
          }
        }

        simulaatio.getSimulaatioAskel = nopeusSaadin.value
        simulaationLeveys = paaIkkuna.contents(0).size.width
        simulaationKorkeus = paaIkkuna.contents(0).size.height - 60
        repaint()
        Thread.sleep(10)
      }
    }
  }

  //Koko ohjelman paaikkuna.
  val paaIkkuna: MainFrame = new MainFrame {
    //Paaikkuna alustetaan 
    var kayttajanKoko = showOptions(null, "Choose the simulation screen resolution:", "Welcome!", Options.Default, Message.Question, Swing.EmptyIcon, Seq("1280x270", "1920x1080", "2560x1440"), 2)
    title = "Aurinkokuntasimulaattori"
    val (leveys, korkeus) = kayttajanKoko match {
      case Result.Ok     => (1280, 720)
      case Result.No     => (1920, 1080)
      case Result.Cancel => (2560, 1440)
      case _             => (1280, 720)
    }
    simulaationLeveys = leveys
    simulaationKorkeus = korkeus - 60
    preferredSize = new Dimension(leveys, korkeus)
    minimumSize = new Dimension(1280, 720)

    //Nappi jolla simulaatio aloitetaan ja pysaytetaan
    val startButton = new Button {
      preferredSize = new Dimension(70, 28)
      background = Color.green
      foreground = Color.white
      focusPainted = false
      action = Action("Start") {
        if (text == "Start") {
          ajossa = true
          background = Color.red
          text = "Pause"
        } else {
          ajossa = false
          background = Color.green
          text = "Start"
        }
      }
    }
    //Nappi jolla siirrytaan katsomaan simulaatiota edesta.
    val frontButton: ToggleButton = new ToggleButton("Front") {
      background = Color.gray
      foreground = Color.white
      focusPainted = false
      selected = true
      action = Action("Front") {
        nakyma = Tyyppi.Front
        topButton.selected = false
        leftButton.selected = false
      }
    }
    //Nappi jolla siirrytaan katsomaan simulaatiota ylhaalta.
    val topButton: ToggleButton = new ToggleButton("Top") {
      background = Color.gray
      foreground = Color.white
      focusPainted = false
      action = Action("Top") {
        nakyma = Tyyppi.Top
        leftButton.selected = false
        frontButton.selected = false
      }
    }
    //Nappi jolla siirrytaan katsomaan simulaatiota vasemmalta.
    val leftButton: ToggleButton = new ToggleButton("Left") {
      background = Color.gray
      foreground = Color.white
      focusPainted = false
      action = Action("Left") {
        nakyma = Tyyppi.Left
        topButton.selected = false
        frontButton.selected = false
      }
    }
    //Nappi jolla voidaan vaihtaa lahetettava kappale satelliitiksi.
    val radioSatellite: RadioMenuItem = new RadioMenuItem("Satellite") {
      selected = true
      action = Action("Satellite (mass predefined)") {
        if (selected) {
          lahetysTyyppi = Tyyppi.Satelliitti
          radioPlanet.selected = false
          radioLargePlanet.selected = false
          radioSmallPlanet.selected = false
          radioStar.selected = false
        }
      }
    }
    //Nappi jolla voidaan vaihtaa lahetettava kappale pieeksi planeetaksi.
    val radioSmallPlanet: RadioMenuItem = new RadioMenuItem("Small Planet") {
      action = Action("Small Planet") {
        if (selected) {
          lahetysTyyppi = Tyyppi.PienPlaneetta
          radioPlanet.selected = false
          radioLargePlanet.selected = false
          radioSatellite.selected = false
          radioStar.selected = false
        }
      }
    }
    //Nappi jolla voidaan vaihtaa lahetettava kappale planeetaksi.
    val radioPlanet: RadioMenuItem = new RadioMenuItem("Planet") {
      action = Action("Planet") {
        if (selected) {
          lahetysTyyppi = Tyyppi.Planeetta
          radioLargePlanet.selected = false
          radioSmallPlanet.selected = false
          radioSatellite.selected = false
          radioStar.selected = false
        }
      }
    }
    //Nappi jolla voidaan vaihtaa lahetettava kappale suureksi planeetaksi.
    val radioLargePlanet: RadioMenuItem = new RadioMenuItem("Large Planet") {
      action = Action("Large Planet") {
        if (selected) {
          lahetysTyyppi = Tyyppi.SuurPlaneetta
          radioPlanet.selected = false
          radioSmallPlanet.selected = false
          radioSatellite.selected = false
          radioStar.selected = false
        }
      }
    }
    //Nappi jolla voidaan vaihtaa lahetettava kappale Tahdeksi.
    val radioStar: RadioMenuItem = new RadioMenuItem("Star") {
      action = Action("Star") {
        if (selected) {
          lahetysTyyppi = Tyyppi.Tahti
          radioPlanet.selected = false
          radioSmallPlanet.selected = false
          radioSatellite.selected = false
          radioLargePlanet.selected = false
        }
      }
    }

    //Ohjelman valikkopalkki.
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new Menu("Load") {
          //Tekee listan valikkoon kaikista alkuasetustiedostoista, jotka loytyy Presets kansiosta.
          for (tiedosto <- simulaatio.tiedostoLista) {
            contents += new MenuItem(tiedosto) {
              action = Action(tiedosto) {
                simulaatio.nykyinenTiedosto = Source.fromFile("src/Presets/" + tiedosto).getLines.toArray
                simulaatio.alusta
                ajossa = false
                startButton.background = Color.green
                startButton.text = "Start"
                if (simulaatio.nykyinenTiedostoRikki) {
                  showMessage(this, "File corrupt. Objects might not have loaded correctly!")
                }
              }
            }
          }

        }
        contents += new MenuItem("Exit") {
          action = Action("Exit") {
            sys.exit
          }
        }

      }
      //Tekee valikkoon asetussivun josta voi valita piirretaanko kappaleiden polut
      contents += new Menu("Settings") {
        contents += new RadioMenuItem("Draw object trails") {
          selected = true
          action = Action("Draw object paths") {
            if (selected) {
              naytaPolut = true
            } else {
              naytaPolut = false
            }
          }
        }
        contents += new RadioMenuItem("Enable explosions") {
          selected = true
          action = Action("Enable explosions") {
            if (selected) {
              naytaRajahdykset = true
            } else {
              naytaRajahdykset = false
            }
          }
        }
        contents += new Menu("Change object type") {
          contents += radioSatellite
          contents += radioSmallPlanet
          contents += radioPlanet
          contents += radioLargePlanet
          contents += radioStar
        }
      }

    }
    //Lisataan paaikkunaan simulaatio ruutu ja tyokalupalkki josta simulaatiota voi ohjata.
    contents = new BorderPanel {
      add(new FlowPanel {
        preferredSize = new Dimension(simulaationLeveys, 40)
        border = Swing.LineBorder(Color.gray)
        contents += startButton
        contents += new Label(" " * 20 + "View:  ")
        contents += frontButton
        contents += leftButton
        contents += topButton
        contents += new Label(" " * 20 + "Time Factor(0-10):  ")
        contents += nopeusSaadin
        contents += new Label(" " * 20 + "Mass(1-100):  ")
        contents += lahetysMassaSaadin
      }, North)
      layout += simulaatioRuutu -> Center
    }
    centerOnScreen
    simulaatioRuutu.simulaatioAjo.start
  }
  //Asettaa paaikkunan ruudulle
  def top = {
    paaIkkuna
  }

}