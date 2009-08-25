package org.workcraft.plugins.circuit;

import java.util.ArrayList;

import org.workcraft.dom.visual.VisualModel;
import org.workcraft.framework.exceptions.VisualComponentCreationException;
import org.workcraft.framework.exceptions.VisualConnectionCreationException;
import org.workcraft.framework.exceptions.VisualModelInstantiationException;
import org.workcraft.gui.edit.tools.GraphEditorTool;


public class VisualCircuit extends VisualModel {

	@Override
	public ArrayList<GraphEditorTool> getAdditionalTools() {
		ArrayList<GraphEditorTool> list = new ArrayList<GraphEditorTool>(super.getAdditionalTools());
//		list.add(new SimulationTool());
		return list;
	}

	public VisualCircuit(Circuit model)
	throws VisualModelInstantiationException {
		super(model);
		try {
			createDefaultFlatStructure();
		} catch (VisualComponentCreationException e) {
			throw new VisualModelInstantiationException(e);
		} catch (VisualConnectionCreationException e) {
			throw new VisualModelInstantiationException(e);
		}
	}

}
