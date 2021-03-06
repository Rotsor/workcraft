package org.workcraft.plugins.stg;

import org.workcraft.dependencymanager.advanced.user.ModifiableExpression;
import org.workcraft.dependencymanager.advanced.user.Variable;
import org.workcraft.dom.Node;
import org.workcraft.dom.visual.VisualGroup;

import org.pcollections.HashTreePSet;
import org.pcollections.PSet;

public class StgEditorState {
	public StgEditorState(VisualGroup root) {
		this.currentLevel = Variable.create(root);
	}
	public final ModifiableExpression<PSet<Node>> selection = Variable.<PSet<Node>>create(HashTreePSet.<Node>empty());
	public final ModifiableExpression<VisualGroup> currentLevel;
}
