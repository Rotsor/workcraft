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

package org.workcraft.dom.visual.connections;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.workcraft.dependencymanager.advanced.core.EvaluationContext;
import org.workcraft.dependencymanager.advanced.core.Expression;
import org.workcraft.dependencymanager.advanced.core.ExpressionBase;
import org.workcraft.dependencymanager.advanced.core.Expressions;
import org.workcraft.dependencymanager.advanced.core.GlobalCache;
import org.workcraft.dependencymanager.advanced.user.ModifiableExpression;
import org.workcraft.dependencymanager.advanced.user.ModifiableExpressionImpl;
import org.workcraft.dependencymanager.advanced.user.StorageManager;
import org.workcraft.dom.visual.DrawRequest;
import org.workcraft.dom.visual.DrawableNew;
import org.workcraft.dom.visual.DeprecatedGraphicalContent;
import org.workcraft.dom.visual.Touchable;
import org.workcraft.dom.visual.VisualTransformableNode;
import org.workcraft.gui.Coloriser;


public class ControlPoint extends VisualTransformableNode implements DrawableNew {
	private double size = 0.15;
	private Color fillColor = Color.BLUE;

	public ControlPoint(StorageManager storage) {
		super(storage);
	}
	
	Shape shape = new Ellipse2D.Double(
			-size / 2,
			-size / 2,
			size,
			size);
	
	public Rectangle2D getBoundingBoxInLocalSpace() {
		return new Rectangle2D.Double(-size, -size, size*2, size*2);
	}

	@Override
	public Expression<? extends DeprecatedGraphicalContent> graphicalContent() {
		return Expressions.constant(new DeprecatedGraphicalContent() {
			@Override
			public void draw(DrawRequest r) {
				r.getGraphics().setColor(Coloriser.colorise(fillColor, r.getDecoration().getColorisation()));
				r.getGraphics().fill(shape);
			}
		});
	}

	public Expression<Touchable> localSpaceTouchable() {
		return new ExpressionBase<Touchable>() {

			@Override
			protected Touchable evaluate(EvaluationContext context) {
				return new Touchable() {

					@Override
					public boolean hitTest(Point2D point) {
						return getBoundingBox().contains(point);
					}

					@Override
					public Rectangle2D getBoundingBox() {
						return new Rectangle2D.Double(-size, -size, size*2, size*2);
					}

					@Override
					public Point2D getCenter() {
						return new Point2D.Double(0, 0);
					}
				};
			}
		};
	}
	
	public ModifiableExpression<AffineTransform> simpleTransform() {
		return super.transform();
	}
	
	@Override
	public ModifiableExpression<AffineTransform> transform() {
		Expression<ModifiableExpression<AffineTransform>> expr = new ExpressionBase<ModifiableExpression<AffineTransform>>() {

			@Override
			protected ModifiableExpression<AffineTransform> evaluate(EvaluationContext context) {
				ConnectionGraphic parent = (ConnectionGraphic)context.resolve(parent());
				if(parent == null)
					return simpleTransform();
				return context.resolve(parent.scaler()).get(ControlPoint.this);
			}
		};
		
		return unfold(expr);
	}

	private <T> ModifiableExpression<T> unfold(final Expression<? extends ModifiableExpression<T>> expr) {
		return new ModifiableExpressionImpl<T>() {

			@Override
			protected void simpleSetValue(T newValue) {
				GlobalCache.eval(expr).setValue(newValue);
			}

			@Override
			protected T evaluate(EvaluationContext context) {
				return context.resolve(context.resolve(expr));
			}
		};
	}
}
