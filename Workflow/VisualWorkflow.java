package org.workcraft.plugins.workflow;

import org.workcraft.dependencymanager.advanced.user.StorageManager;
import org.workcraft.dom.Node;
import org.workcraft.dom.visual.AbstractVisualModel;
import org.workcraft.exceptions.InvalidConnectionException;

public class VisualWorkflow extends AbstractVisualModel {
	
	public VisualWorkflow(Workflow mathModel, StorageManager storage)
	{
		super(mathModel, storage);
	}

	@Override
	public void connect(Node first, Node second) throws InvalidConnectionException {
		throw new org.workcraft.exceptions.NotImplementedException();
	}

	@Override
	public void validateConnection(Node first, Node second) throws InvalidConnectionException {
		throw new InvalidConnectionException("Незя так!");
	}

}
