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

import org.workcraft.plugins.cpog.optimisation.BooleanFormula;

public class BooleanWorkerPrettifier implements BooleanWorker
{
	private final ReducedBooleanWorker worker;

	public BooleanWorkerPrettifier(ReducedBooleanWorker worker)
	{
		this.worker = worker;
	}
	
	@Override
	public <Var> BooleanFormula<Var> not(BooleanFormula<Var> x) {
		if(x == zero())
			return one();
		if(x == one())
			return zero();
		return worker.not(x);
	}

	@Override
	public <Var> BooleanFormula<Var> iff(BooleanFormula<Var> x, BooleanFormula<Var> y) {
		if(x==y)
			return one();
		if(x == zero())
			return not(y);
		if(x == one())
			return y;
		if(y == zero())
			return not(x);
		if(y == one())
			return x;
		return worker.iff(x, y);
	}

	@Override
	public <Var> BooleanFormula<Var> imply(BooleanFormula<Var> x, BooleanFormula<Var> y) {
		return or(not(x), y);
	}

	@Override
	public <Var> BooleanFormula<Var> and(BooleanFormula<Var> x, BooleanFormula<Var> y) {
		if(x==y)
			return x;
		if(x == zero() || y == zero())
			return zero();
		if(x == one())
			return y;
		if(y == one())
			return x;
		return worker.and(x,y);
	}

	@Override
	public <Var> BooleanFormula<Var> or(BooleanFormula<Var> x, BooleanFormula<Var> y) {
		return not(and(not(x), not(y)));
	}

	@Override
	public <Var> BooleanFormula<Var> one() {
		return One.instance();
	}

	@Override
	public <Var> BooleanFormula<Var> xor(BooleanFormula<Var> x, BooleanFormula<Var> y) {
		return not(iff(x,y));
	}

	@Override
	public <Var> BooleanFormula<Var> zero() {
		return Zero.instance();
	}

	@Override
	public <Var> BooleanFormula<Var> var(Var var) {
		return Variable.create(var);
	}
}
