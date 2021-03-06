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
package org.workcraft.plugins.cpog.optimisation.expressions;

import java.io.Serializable;

import org.workcraft.plugins.cpog.optimisation.BooleanFormula;

public class Not<Var> implements BooleanFormula<Var>, Serializable {
	private static final long serialVersionUID = 1L;
	
	private final BooleanFormula<Var> x;

	private Not(BooleanFormula<Var> x) {
		this.x = x;
	}

	public static <Var> Not<Var> create(BooleanFormula<Var> x) {
		return new Not<Var>(x);
	}
	
	@Override
	public <T> T accept(BooleanVisitor<Var, T> visitor) {
		return visitor.visit(this);
	}

	public BooleanFormula<Var> getX() {
		return x;
	}
}
