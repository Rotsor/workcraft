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

package org.workcraft.plugins.balsa.handshakes;

import java.util.Map;

import org.workcraft.plugins.balsa.components.SynchPush;
import org.workcraft.plugins.balsa.handshakebuilder.Handshake;

public class SynchPushHandshakes extends HandshakeMaker<SynchPush> {
	@Override
	protected void fillHandshakes(SynchPush component, Map<String, Handshake> handshakes) {
		handshakes.put("inp", builder.CreatePassivePush(component.getWidth()));
		for(int i=0;i<component.getOutputCount();i++)
			handshakes.put("pout"+i, builder.CreatePassivePull(component.getWidth()));
		handshakes.put("aout", builder.CreateActivePush(component.getWidth()));
	}
}
