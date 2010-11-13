/*
*
* Copyright 2008,2009 Newcastle University
*
* This file is part of Workcraft.
* 
* Workcraft is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
* 
* Workcraft is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Workcraft.  If not, see <http://www.gnu.org/licenses/>.
*
*/

package org.workcraft.dom;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.workcraft.dependencymanager.advanced.core.Expression;
import org.workcraft.dependencymanager.advanced.user.Variable;
import org.workcraft.dom.visual.connections.VisualConnection;

public class ArbitraryInsertionGroupImpl extends AbstractGroup implements Container {
	Variable<LinkedList<Node>> children = new Variable<LinkedList<Node>> (new LinkedList<Node>());
	
	public ArbitraryInsertionGroupImpl (Container groupRef, VisualConnection parent) {
		super(groupRef);
		this.parent.setValue(parent);
	}

	public List<Node> getChildren() {
		return Collections.unmodifiableList(children.getValue());
	}
	
	public void add(int index, Node node) {
		children.getValue().add(index, node);
		children.setValue(children.getValue());
	}

	@Override
	protected void addInternal(Node node) {
		children.getValue().add(node);
		children.setValue(children.getValue());
	}

	@Override
	protected void removeInternal(Node node) {
		children.getValue().remove(node);
		children.setValue(children.getValue());
	}

	@Override
	public Expression<? extends LinkedList<Node>> children() {
		return children;
	}
}
