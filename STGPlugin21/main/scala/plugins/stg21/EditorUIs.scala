package org.workcraft.plugins.stg21
import org.workcraft.gui.propertyeditor.EditableProperty
import org.workcraft.scala.Expressions.ModifiableExpression
import org.workcraft.gui.propertyeditor.string.StringProperty
import org.workcraft.gui.propertyeditor.integer.IntegerProperty
import org.workcraft.gui.propertyeditor.bool.BooleanProperty
import org.workcraft.gui.propertyeditor.dubble.DoubleProperty
import org.workcraft.plugins.stg21.modifiable._
import org.workcraft.gui.propertyeditor.choice.ChoiceProperty
import scala.collection.JavaConversions._
import org.pcollections.TreePVector
import org.workcraft.scala.Expressions.Expression

object EditorUIs {
  case class EditorUI[T](create : String => ModifiableExpression[T] => Expression[EditableProperty])
  implicit val stringEditorUI : EditorUI[String] = EditorUI (name => x => StringProperty(name, x))
  implicit val intEditorUI : EditorUI[Int] = EditorUI (name => x => IntegerProperty(name, x))
  // implicit val doubleEditorUI : EditorUI[java.lang.Double] = EditorUI (name => x => DoubleProperty(name, x))
  implicit val valueDoubleEditorUI : EditorUI[Double] = 
    EditorUI (name => x => DoubleProperty(name, x))
  case class Choice[T] (values : List[(String, T)])
  implicit def choiceEditorUI[T] (implicit choice : Choice[T]) : EditorUI[T] = EditorUI { name => x =>
    ChoiceProperty(name, (choice.values), x)
  }
}
