package org.workcraft.plugins.fsm

import java.awt.event.KeyEvent
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D
import java.awt.Color
import javax.swing.JOptionPane
import org.workcraft.dependencymanager.advanced.user.Variable
import org.workcraft.dom.visual.connections.Bezier
import org.workcraft.dom.visual.connections.ConnectionGui
import org.workcraft.dom.visual.connections.Polyline
import org.workcraft.dom.visual.connections.RelativePoint
import org.workcraft.dom.visual.connections.VisualConnectionContext
import org.workcraft.dom.visual.connections.VisualConnectionGui
import org.workcraft.exceptions.InvalidConnectionException
import org.workcraft.graphics.Java2DDecoration._
import org.workcraft.graphics.BoundedColorisableGraphicalContent
import org.workcraft.graphics.Colorisation
import org.workcraft.graphics.GraphicalContent
import org.workcraft.graphics.Graphics.label
import org.workcraft.gui.modeleditor.tools.ConnectionManager
import org.workcraft.gui.modeleditor.tools.Button
import org.workcraft.gui.modeleditor.tools.NodeGeneratorTool
import org.workcraft.gui.modeleditor.KeyBinding
import org.workcraft.gui.modeleditor.KeyEventType
import org.workcraft.gui.modeleditor.ModelEditor
import org.workcraft.gui.propertyeditor.integer.IntegerProperty
import org.workcraft.gui.propertyeditor.string.StringProperty
import org.workcraft.gui.propertyeditor.EditableProperty
import org.workcraft.gui.CommonVisualSettings
import org.workcraft.scala.Expressions._
import scalaz.effect.IO._
import scalaz.effect.IO
import org.workcraft.scala.grapheditor.tools.GenericConnectionTool
import org.workcraft.scala.grapheditor.tools.GenericSelectionTool
import scalaz.Scalaz._
import org.workcraft.scala.Scalaz._
import scalaz._
import org.workcraft.gui.modeleditor.sim.GenericSimulationTool
import org.workcraft.services.Service
import org.workcraft.services.EditorScope
import org.workcraft.gui.modeleditor.UndoService
import org.workcraft.gui.modeleditor.Undo
import org.workcraft.gui.modeleditor.UndoAction
import org.workcraft.gui.modeleditor.PropertyService
import org.workcraft.gui.modeleditor.ShowTraceService
import org.workcraft.gui.modeleditor.ShowTrace
import org.workcraft.gui.modeleditor.tools.ToolEnvironment
import org.workcraft.gui.modeleditor.sim.Trace
import org.workcraft.gui.modeleditor.sim.MarkedTrace
import org.workcraft.plugins.petri2.PetriNet
import org.workcraft.plugins.petri2.VisualArc
import javax.swing.JOptionPane

case class EditorState(description: String, net: VisualFSM, selection: Set[Node])

class FSMEditor(fsm: EditableFSM) extends ModelEditor {
  val selection = Variable.create(Set[Node]())

  val props = new FSMProperties(fsm, selection, pushUndo)

  def image(colorisation: Node => Colorisation, offsetNodes: Set[Node], offsetValue: Point2D.Double) =
    (fsm.states.expr <|**|> (fsm.arcs, CommonVisualSettings.settings)) >>= {
      case (comp, a, settings) => {
        treeSequence((a.map(a => arcImageWithOffset(a, offsetNodes, offsetValue, settings).map(_.graphicalContent.applyColorisation(colorisation(a)))) ++
          comp.map(c => (stateImage(c, settings) <**> stateTransformWithOffset(c, offsetNodes, offsetValue))(_.transform(_).cgc.applyColorisation(colorisation(c)))))).map(_.foldLeft(GraphicalContent.Empty)(_.compose(_)))
      }
    }


  def imageForConnection(colorisation: State => Colorisation) =
    image({
      case s : State => colorisation(s)
      case a : Arc => Colorisation.Empty
    }, Set(), new Point2D.Double(0,0))

  def imageForSimulation(colorisation: Arc => Colorisation, curState: (State, List[String])) =
    image({
      case s : State => if (s == curState._1) Colorisation(Some(Color.ORANGE), None) else Colorisation.Empty
      case a : Arc => colorisation(a)
    }, Set(), new Point2D.Double(0,0))

  def stateImage(state: State, settings: CommonVisualSettings): Expression[BoundedColorisableGraphicalContent] =
    (fsm.labels.map(_(state)) <***> (fsm.initialState.map(_ == state), fsm.finalStates.map(_.contains(state))))((l, i, t) => FSMGraphics.stateImage(l, i, t, settings))

  def arcImage(a: Arc, settings: CommonVisualSettings): Expression[ConnectionGui] = arcImageWithOffset(a, Set(), new Point2D.Double(0, 0), settings)

  def arcImageWithOffset(a: Arc, offsetNodes: Set[Node], offsetValue: Point2D.Double, settings:CommonVisualSettings) = for {
    //TODO: support this  visualArcs <- fsm.visualArcs; 
    preset <- fsm.preset(a.from);
    is <- fsm.initialState;
    t1 <- touchableWithOffset(a.from, offsetNodes, offsetValue, settings);
    t2 <- touchableWithOffset(a.to, offsetNodes, offsetValue, settings);
    ap1 <- statePositionWithOffset(a.from, offsetNodes, offsetValue);
    ap2 <- statePositionWithOffset(a.to, offsetNodes, offsetValue);
    labels <- fsm.arcLabels
  } yield {
    val props = VisualArc.properties.copy(
      label = Some(label(ArcLabels.printLabels(",")(labels(a)),
			 settings.effectiveLabelFont,
			 settings.foregroundColor).boundedColorisableGraphicalContent)) 


    if (a.from == a.to) {
      val podgon = 6
      val podgonAdjustment = if (a.from == is) -1 else 1
      VisualConnectionGui.getConnectionGui(
	props,
	VisualConnectionContext.makeContext(t1, ap1 + point(-0.125, -0.75 * podgonAdjustment), t2, ap2 + point(0.125, -0.75 * podgonAdjustment)),
	Bezier(RelativePoint(point(-podgon, -podgon * podgonAdjustment)), RelativePoint(point(1+podgon, -podgon * podgonAdjustment))))
    }
    else 
      if (preset.exists (_._1 == a.to))
	VisualConnectionGui.getConnectionGui(
	  props,
	  VisualConnectionContext.makeContext(t1, ap1, t2, ap2),
	  Bezier(RelativePoint(point(0.5, -0.1)), RelativePoint(point(0.5, -0.1))))
      else VisualConnectionGui.getConnectionGui(
	props,
	VisualConnectionContext.makeContext(t1, ap1, t2, ap2),
	Polyline(List()))
  }

  def makeTransform(p: Point2D.Double) = AffineTransform.getTranslateInstance(p.getX, p.getY)

  def stateTransform(s: State) = statePosition(s).map(makeTransform(_))

  def stateTransformWithOffset(s: State, offsetNodes: Set[Node], offsetValue: Point2D.Double) = statePositionWithOffset(s, offsetNodes, offsetValue).map(makeTransform(_))

  def statePosition(s: State) = fsm.layout.map(_(s))

  def statePositionWithOffset(c: State, offsetNodes: Set[Node], offsetValue: Point2D.Double) = statePosition(c).lwmap(pos => if (offsetNodes.contains(c)) pos + offsetValue else pos)

  def touchable(n: Node, settings: CommonVisualSettings) = touchableWithOffset(n, Set(), new Point2D.Double(), settings)

  def touchableWithOffset(n: Node, offsetNodes: Set[Node], offsetValue: Point2D.Double, settings: CommonVisualSettings) = n match {
    case s: State => (FSMGraphics.stateTouchable <**> stateTransformWithOffset(s, offsetNodes, offsetValue))((touchable, xform) => touchable.transform(xform))
    case a: Arc => arcImage(a, settings).map(_.shape.touchable)
  }

  def move(nodes: Set[Node], offset: Point2D.Double): IO[Unit] = pushUndo("move nodes") >> nodes.map({
    case c: State => fsm.layout.update(l => l + (c -> (l(c) + offset)))
    case _ => ().pure[IO]
  }).traverse_(x => x)

  private def selectionTool = GenericSelectionTool[Node](
    fsm.nodes,
    selection,
    move,
    (_, x) => x,
    n => CommonVisualSettings.settings >>= (s => touchable(n, s)),
    image(_, _, _),
    List(KeyBinding("Delete selection", KeyEvent.VK_DELETE, KeyEventType.KeyPressed, Set(), selection.eval >>= { sel =>
      fsm.deleteNodes(sel) >>= {
        case Right(io) => (pushUndo("delete nodes") >> selection.update(_ -- sel) >> io) >| None
        case Left(message) => IO { (Some(message)) }
      }
    })),
    None)

  private val connectionManager = new ConnectionManager[State] {
    def connect(node1: State, node2: State): Expression[Either[InvalidConnectionException, IO[Unit]]] =
      fsm.saveState.map ( s =>
	if (s.fsm.preset(node2).exists(_._1 == node1))
	  Left(new InvalidConnectionException("There is already an arc between " + s.fsm.labels(node1) + " and " +
					    s.fsm.labels(node2)))
	else
	  Right(pushUndo("create arc") >> fsm.createArc(node1, node2) >| Unit)
		   )
  }


  private val simToolMessage = (fsm.incidentArcs <***> (fsm.finalStates, fsm.arcLabels)) (
    (ia, fs, al) =>
      {
	val success = Some(("The input has been recognised :-)", Color.GREEN.darker))
	val failure = Some(("The input has not been recognised :-(", Color.RED.darker))

	s: (State, List[String]) => s match {
	  case (q, Nil) =>
	    if (fs.contains(q)) success
	    else failure
	  case (q, input@(x :: _)) => {
	    if (ia(q).filter (arc => (arc.from == q && ((al(arc).list.contains(Some(x))) || (al(arc).list.contains(None))))).isEmpty) failure
	    else Some(("Remaining input: " + input.mkString(", "), Color.BLACK))
	  }
	}
      }
  )

  val ss = fsm.finalStates.map ( fs => 
    (s: (State, List[String])) => 
      if (fs.contains(s._1))
	Some(("Input trace (in final state): " + s._2.mkString(", "), Color.GREEN.darker))
      else
	Some(("Input trace: " + s._2.mkString(", "), Color.BLACK)))

  private val simToolMessageGen = ss

  private val traceSimToolButton = new Button {
    import org.workcraft.gui.GUI
    override def hotkey = Some(KeyEvent.VK_M)
    override def icon = Some(GUI.createIconFromSvgUsingSettingsSize("images/icons/svg/start-yellow.svg").unsafePerformIO)
    override def label = "Simulate trace"
  }
      
  private val simulationTool =
    new GenericSimulationTool[Arc, (State, List[String])](
      fsm.arcs, 
      n => CommonVisualSettings.settings >>= (s => touchable(n, s)),
      (fsm.saveState.eval <**> IO{  
        JOptionPane.showInputDialog(null, "Input to use for simulation:").replace(" ","").split(",").toList })(
        (fsm, input) =>
        FSMSimulation(fsm.fsm, input)),
      imageForSimulation(_, _),
      simToolMessage
      ) {
      override def button = traceSimToolButton
    }

  private val simulationToolGen =
    GenericSimulationTool[Arc, (State, List[String])](
      fsm.arcs, 
      n => CommonVisualSettings.settings >>= (s => touchable(n, s)),
      fsm.saveState.eval.map (s => FSMSimulationGen(s.fsm)),
      imageForSimulation(_, _),
      simToolMessageGen
      )


  private def connectionTool =
    GenericConnectionTool[State](fsm.states, n => CommonVisualSettings.settings >>= (s => touchable(n, s)), statePosition(_), connectionManager, imageForConnection(_))

  private def stateGeneratorTool =
    NodeGeneratorTool(Button("State", "images/icons/svg/place_empty.svg", Some(KeyEvent.VK_T)).unsafePerformIO, imageForConnection(_ => Colorisation(None, None)), pushUndo("create state") >> fsm.createState(_) >| Unit)

  def tools = NonEmptyList(selectionTool, connectionTool, stateGeneratorTool, simulationToolGen, simulationTool)
  def keyBindings = List()

  val undoStack = Variable.create(List[EditorState]())
  val redoStack = Variable.create(List[EditorState]())

  def saveState(description: String): IO[EditorState] = (fsm.saveState.eval <**> selection.eval)(EditorState(description, _, _))

  def loadState(state: EditorState): IO[Unit] = fsm.loadState(state.net) >> selection.set(state.selection)

  def pushState(description: String, stack: ModifiableExpression[List[EditorState]]): IO[Unit] = (saveState(description) >>= (state => stack.update(s => (state :: s).take(100))))

  def pushUndo(description: String) = pushState(description, undoStack) >> redoStack.update(_ => Nil)

  def popUndo: IO[Unit] = undoStack.eval >>= ({
    case top :: rest => pushState(top.description, redoStack) >> loadState(top) >> undoStack.update(_ => rest)
    case _ => IO {}
  })

  def popRedo: IO[Unit] = redoStack.eval >>= ({
    case top :: rest => pushState(top.description, undoStack) >> loadState(top) >> redoStack.update(_ => rest)
    case _ => IO {}
  })

  def implementation[T](service: Service[EditorScope, T]) = service match {
    case UndoService => Some(Undo(
      undoStack.map({ case top :: _ => Some(UndoAction(top.description, popUndo)); case x => None }),
      redoStack.map({ case top :: _ => Some(UndoAction(top.description, popRedo)); case x => None })))

    case PropertyService => Some(props.props)
  }
}
