package org.workcraft.gui

import java.awt.Font
import javax.swing.JFrame
import org.workcraft.services.GlobalServiceManager
import org.workcraft.logging.Logger
import org.workcraft.logging.Logger._
import org.workcraft.logging.StandardStreamLogger
import javax.swing.JDialog
import javax.swing.UIManager
import org.pushingpixels.substance.api.SubstanceLookAndFeel
import org.pushingpixels.substance.api.SubstanceConstants.TabContentPaneBorderKind
import org.streum.configrity.Configuration
import javax.swing.SwingUtilities
import javax.swing.JPanel
import java.awt.BorderLayout
import scalaz.effects.IO
import scalaz.effects.IO._
import scalaz.Scalaz._
import java.awt.Frame
import javax.swing.WindowConstants
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JButton
import javax.swing.JComponent
import org.workcraft.gui.docking.DockingRoot
import org.workcraft.gui.docking.DockableWindowConfiguration
import org.workcraft.gui.docking.DockableWindow
import org.workcraft.gui.logger.LoggerWindow
import org.workcraft.gui.modeleditor.ModelEditorPanel
import org.workcraft.services.NewModelImpl

class MainWindow private (val globalServices: () => GlobalServiceManager, reconfigure: () => Unit /*, configuration: Option[GuiConfiguration]*/ ) extends JFrame {
  val dockingRoot = new DockingRoot("workcraft")
  setContentPane(dockingRoot)

  val menu = new MainMenu(this, utilityWindows, globalServices, m => newModel(m), reconfigure)
  
  this.setJMenuBar(menu)

  lazy val logger = new LoggerWindow
  
  dockingRoot.createRootWindow("1", "Kojo", new ModelEditorPanel, DockableWindowConfiguration())

  //applyGuiConfiguration(configuration)

  def utilityWindows: List[DockableWindow] =
    List(
      createUtilityWindow("Log", "Log", logger),
      createUtilityWindow("Bojo", "Bojo", new JButton("Bojo")),
      createUtilityWindow("Kaja", "Kaja", new JButton("Kaja")))

  def closeUtilityWindow(window: DockableWindow) = {
    window.close
    menu.windowsMenu.update(window)
  }

  def createUtilityWindow(title: String, persistentId: String, content: JComponent) =
    dockingRoot.createRootWindow(title, persistentId, content, DockableWindowConfiguration(onCloseClicked = closeUtilityWindow))

  def createUtilityWindow(title: String, persistentId: String, content: JComponent, relativeTo: DockableWindow, relativeRegion: String, split: Double) =
    dockingRoot.createWindow(title, persistentId, content, DockableWindowConfiguration(onCloseClicked = closeUtilityWindow), relativeTo, relativeRegion, split)

  private def applyGuiConfiguration(configOption: Option[GuiConfiguration])(implicit logger:() => Logger[IO]) = configOption match {
    case Some(config) => {
      LafManager.setLaf(config.lookandfeel)
      setSize(config.xSize, config.ySize)
      setLocation(config.xPos, config.yPos)

      if (config.maximised)
        setExtendedState(Frame.MAXIMIZED_BOTH)
    }
    case None =>
      {
        setSize(800, 600)
      }

      SwingUtilities.updateComponentTreeUI(this)
  }

  private def guiConfiguration = {
    val size = getSize()
    val maximised = (getExtendedState() & Frame.MAXIMIZED_BOTH) != 0

    GuiConfiguration(xPos = getX(), yPos = getY(),
      xSize = size.getWidth().toInt, ySize = size.getHeight().toInt, maximised = maximised,
      lookandfeel = LafManager.getCurrentLaf)
  }

  private def applyIconManager(implicit logger:() => Logger[IO]) = MainWindowIconManager.apply(this, logger)
  
  
  def newModel(newModelImpl: NewModelImpl) = {
    val model = newModelImpl.create
  }
  
  def exit = System.exit(0)
}

object MainWindow {
  private def shutdown(implicit mainWindow: MainWindow, logger: () => Logger[IO]) = {
    unsafeInfo("Shutting down")

    GuiConfiguration.save(mainWindow.guiConfiguration)

    mainWindow.setVisible(false)
    
    unsafeInfo("Have a nice day!")
    System.exit(0)
  }

  def startGui(globalServices: () => GlobalServiceManager, reconfigure: () => Unit, switchLogger: Logger[IO] => Unit)(implicit logger: () => Logger[IO]): IO[MainWindow] = {
    // LaF tweaks
    JDialog.setDefaultLookAndFeelDecorated(true)
    UIManager.put(SubstanceLookAndFeel.TABBED_PANE_CONTENT_BORDER_KIND, TabContentPaneBorderKind.SINGLE_FULL)

    implicit val mainWindow = new MainWindow(globalServices, reconfigure)

    mainWindow.applyGuiConfiguration(GuiConfiguration.load)
    mainWindow.applyIconManager

    mainWindow.setTitle("Workcraft")
    mainWindow.setVisible(true)
    
    switchLogger (mainWindow.logger)

    mainWindow.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    mainWindow.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: WindowEvent) = shutdown
    })

    mainWindow
  }.pure
}