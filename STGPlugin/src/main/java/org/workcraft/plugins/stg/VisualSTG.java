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

package org.workcraft.plugins.stg;

import static org.workcraft.dependencymanager.advanced.core.GlobalCache.eval;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;

import org.workcraft.annotations.CustomTools;
import org.workcraft.annotations.DefaultCreateButtons;
import org.workcraft.annotations.DisplayName;
import org.workcraft.dependencymanager.advanced.user.StorageManager;
import org.workcraft.dom.Connection;
import org.workcraft.dom.Container;
import org.workcraft.dom.Node;
import org.workcraft.dom.math.MathConnection;
import org.workcraft.dom.visual.AbstractVisualModel;
import org.workcraft.dom.visual.CustomToolButtons;
import org.workcraft.dom.visual.VisualComponent;
import org.workcraft.dom.visual.VisualGroup;
import org.workcraft.dom.visual.connections.VisualConnection;
import org.workcraft.exceptions.InvalidConnectionException;
import org.workcraft.exceptions.NodeCreationException;
import org.workcraft.plugins.petri.VisualPlace;
import org.workcraft.plugins.petri.VisualTransition;
import org.workcraft.plugins.stg.SignalTransition.Direction;
import org.workcraft.plugins.stg.tools.STGSimulationTool;
import org.workcraft.util.Hierarchy;

@DisplayName("Signal Transition Graph")
@CustomTools ( STGToolsProvider.class )
@DefaultCreateButtons ( { STGPlace.class,  SignalTransition.class, DummyTransition.class } )
@CustomToolButtons ( { STGSimulationTool.class } )  
public class VisualSTG extends AbstractVisualModel {
	public final STG stg;
	public StorageManager storage;

	@Override
	public void validateConnection(Node first, Node second)	throws InvalidConnectionException {
		if (first==second) {
			throw new InvalidConnectionException ("Connections are only valid between different objects");
		}
		
		if (first instanceof VisualPlace) {
			if (second instanceof VisualPlace)
				throw new InvalidConnectionException ("Arcs between places are not allowed");
			if (second instanceof VisualConnection)
				throw new InvalidConnectionException ("Arcs between places and implicit places are not allowed");
		}

		if (first instanceof VisualTransition) {
			if (second instanceof VisualConnection)
				if (! (second  instanceof VisualImplicitPlaceArc))
					throw new InvalidConnectionException ("Only connections with arcs having implicit places are allowed");
		}

		if (first instanceof VisualConnection) {
			if (!(first instanceof VisualImplicitPlaceArc))
				throw new InvalidConnectionException ("Only connections with arcs having implicit places are allowed");
			if (second instanceof VisualConnection)
				throw new InvalidConnectionException ("Arcs between places are not allowed");
			if (second instanceof VisualPlace)
				throw new InvalidConnectionException ("Arcs between places are not allowed");

			VisualImplicitPlaceArc con = (VisualImplicitPlaceArc) first;
			if (con.getFirst() == second || con.getSecond() == second)
				throw new InvalidConnectionException ("Arc already exists");
		}
	}

	@Override
	public void connect(Node first,	Node second)  throws InvalidConnectionException {
		createConnection(first, second);
	}
	
	public VisualConnection createConnection(Node first,	Node second) throws InvalidConnectionException {
		validateConnection(first, second);

		if (first instanceof VisualTransition) {
			if (second instanceof VisualTransition) {
				return createImplicitPlaceConnection((VisualTransition) first, (VisualTransition) second);
			} else if (second instanceof VisualImplicitPlaceArc) {
				VisualImplicitPlaceArc con = (VisualImplicitPlaceArc)second;
				VisualPlace place = makeExplicit(con);
				return createConnection (first, place);
			} else if (second instanceof VisualPlace) {
				return createSimpleConnection((VisualComponent) first, (VisualComponent) second);
			}
			else throw new InvalidConnectionException("invalid connection");
		} else if (first instanceof VisualImplicitPlaceArc) {
			if (second instanceof VisualTransition) {
				VisualImplicitPlaceArc con = (VisualImplicitPlaceArc)first;
				VisualPlace place = makeExplicit(con);
				return createConnection(place, second);
			}
			else throw new InvalidConnectionException("invalid connection");
		} else {
			return createSimpleConnection((VisualComponent) first, (VisualComponent) second);
		}
	}

	private VisualImplicitPlaceArc createImplicitPlaceConnection(VisualTransition t1,
			VisualTransition t2) throws InvalidConnectionException {
		final ConnectionResult connectResult = stg.connect(t1.getReferencedTransition(), t2.getReferencedTransition());

		STGPlace implicitPlace = connectResult.getImplicitPlace();
		MathConnection con1 = connectResult.getCon1();
		MathConnection con2 = connectResult.getCon2();

		if (implicitPlace == null || con1 == null || con2 == null)
			throw new NullPointerException();

		VisualImplicitPlaceArc result = new VisualImplicitPlaceArc(t1, t2, con1, con2, implicitPlace, storage);
		add(Hierarchy.getNearestContainer(t1, t2), result);
		return result;
	}

	private VisualConnection createSimpleConnection(final VisualComponent firstComponent,
			final VisualComponent secondComponent)
	throws InvalidConnectionException {
		ConnectionResult mathConnection = stg.connect(
				firstComponent.getReferencedComponent(), 
				secondComponent.getReferencedComponent());

		MathConnection con = mathConnection.getSimpleResult();

		if (con == null)
			throw new NullPointerException();

		VisualConnection result = new VisualConnection(con, firstComponent, secondComponent, storage);
		add(Hierarchy.getNearestContainer(firstComponent, secondComponent), result);
		return result;
	}

	private VisualPlace makeExplicit(VisualImplicitPlaceArc con) {
		Container group = Hierarchy.getNearestAncestor(con, Container.class);

		STGPlace implicitPlace = con.getImplicitPlace();
		
		stg.makeExplicit(implicitPlace);
		
		VisualPlace place = new VisualPlace(implicitPlace, storage);
		Point2D p = con.getPointOnConnection(0.5);
		place.position().setValue(p);

		VisualConnection con1 = new VisualConnection(con.getRefCon1(), con.getFirst(), place, storage);
		VisualConnection con2 = new VisualConnection(con.getRefCon2(), place, con.getSecond(), storage);

		group.add(place);
		group.add(con1);
		group.add(con2);

		remove(con);
		return place;
	}

	private void maybeMakeImplicit (VisualPlace place) {
		final STGPlace stgPlace = (STGPlace)place.getReferencedPlace();
		if ( eval(stgPlace.implicit()) ) {

			MathConnection refCon1 = null, refCon2 = null;

			VisualComponent first = (VisualComponent) eval(nodeContext()).getPreset(place).iterator().next();
			VisualComponent second = (VisualComponent) eval(nodeContext()).getPostset(place).iterator().next();

			Collection<Connection> connections = new ArrayList<Connection> (eval(nodeContext()).getConnections(place));
			for (Connection con: connections)
				if (con.getFirst() == place)
					refCon2 = ((VisualConnection)con).getReferencedConnection();
				else if (con.getSecond() == place)
					refCon1 = ((VisualConnection)con).getReferencedConnection();


			VisualImplicitPlaceArc con = new VisualImplicitPlaceArc(first, second, refCon1, refCon2, (STGPlace)place.getReferencedPlace(), storage);

			Hierarchy.getNearestAncestor(
					Hierarchy.getCommonParent(first, second), Container.class)
					.add(con);

			remove(place);
			// connections will get removed automatically by the hanging connection remover
		}
	} 

	public VisualSTG(STG model, StorageManager storage) {
		this (model, null, storage);
	}

	public VisualSTG(STG model, VisualGroup root, StorageManager storage) {
		super(model, root, storage);
		
		this.storage = storage;

		if (root == null)
			try {
				createDefaultFlatStructure();
			} catch (NodeCreationException e) {
				throw new RuntimeException(e);
			}

			this.stg = model;

			Collection<VisualPlace> places = new ArrayList<VisualPlace>(Hierarchy.getDescendantsOfType(getRoot(), VisualPlace.class));
			for(VisualPlace place : places)
				maybeMakeImplicit(place);
	}

	public VisualPlace createPlace() {
		return createPlace(null);
	}
	
	public VisualPlace createPlace(String name) {
		VisualPlace place = new VisualPlace(stg.createPlace(name), storage);
		add(place);
		return place;
	}

	public VisualSignalTransition createSignalTransition(String signalName, SignalTransition.Type type, Direction direction) {
		SignalTransition transition = stg.createSignalTransition(signalName, direction);
		stg.signalType(transition).setValue(type);
		VisualSignalTransition visualTransition = new VisualSignalTransition(transition, storage);
		add(visualTransition);
		return visualTransition;
	}

	public VisualSignalTransition createSignalTransition() {
		SignalTransition transition = stg.createSignalTransition();
		VisualSignalTransition visualTransition = new VisualSignalTransition(transition, storage);
		add(visualTransition);
		return visualTransition;
	}

	public VisualDummyTransition createDummyTransition() {
		DummyTransition transition = stg.createDummyTransition();
		VisualDummyTransition visualTransition = new VisualDummyTransition(transition, storage);
		add(visualTransition);
		return visualTransition;
	}

}