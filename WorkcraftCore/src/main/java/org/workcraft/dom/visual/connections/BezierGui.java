package org.workcraft.dom.visual.connections;

import static org.workcraft.dependencymanager.advanced.core.GlobalCache.eval;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Collections;

import org.workcraft.dependencymanager.advanced.core.EvaluationContext;
import org.workcraft.dependencymanager.advanced.core.Expression;
import org.workcraft.dependencymanager.advanced.core.ExpressionBase;
import org.workcraft.dependencymanager.advanced.core.Expressions;
import org.workcraft.dependencymanager.advanced.user.Variable;
import org.workcraft.dom.Node;
import org.workcraft.dom.visual.ColorisableGraphicalContent;
import org.workcraft.dom.visual.DrawHelper;
import org.workcraft.dom.visual.DrawRequest;
import org.workcraft.dom.visual.Touchable;
import org.workcraft.dom.visual.connections.Bezier.Curve;
import org.workcraft.exceptions.NotImplementedException;
import org.workcraft.gui.Coloriser;
import org.workcraft.util.Geometry;
import org.workcraft.util.Geometry.CurveSplitResult;

public class BezierGui {
	
	@Override
	public Expression<? extends ColorisableGraphicalContent> graphicalContent() {
		return new ExpressionBase<ColorisableGraphicalContent>() {
			@Override
			protected ColorisableGraphicalContent evaluate(final EvaluationContext context) {
				return new ColorisableGraphicalContent() {
					@Override
					public void draw(DrawRequest r) {
						Graphics2D g = r.getGraphics();
						
						VisualConnectionProperties cinfo = context.resolve(connectionInfo);
						Color color = Coloriser.colorise(cinfo.getDrawColor(), r.getColorisation().getColorisation());
						g.setColor(color);
//						g.setStroke(new BasicStroke((float)connectionInfo.getLineWidth()));
						g.setStroke(cinfo.getStroke());
						
						g.draw(context.resolve(visibleCurve2D));
						PartialCurveInfo cvInfo = context.resolve(curveInfo);
						if (cinfo.hasArrow())
							DrawHelper.drawArrowHead(g, color,
									cvInfo.arrowHeadPosition,
									cvInfo.arrowOrientation,
									cinfo.getArrowLength(),
									cinfo.getArrowWidth());
					}
				};
			}
		};
	}

	@Override
	public Expression<? extends Touchable> shape() {
		return new ExpressionBase<Touchable>() {

			@Override
			protected Touchable evaluate(final EvaluationContext context) {
				return new Touchable() {

					@Override
					public boolean hitTest(Point2D point) {
						return context.resolve(parametricCurve).getDistanceToCurve(point) < VisualConnection.HIT_THRESHOLD;
					}

					@Override
					public Rectangle2D getBoundingBox() {
						return context.resolve(parametricCurve).getBoundingBox();
					}

					@Override
					public Point2D getCenter() {
						return context.resolve(parametricCurve).getPointOnCurve(0.5);
					}
					
				};
			}
		};
	}
	

	private final Expression<VisualConnectionProperties> connectionInfo;
	
	public BezierGui(){
		this.curveInfo = new ExpressionBase<PartialCurveInfo>() {
			@Override
			protected PartialCurveInfo evaluate(EvaluationContext context) {
				return Geometry.buildConnectionCurveInfo(context.resolve(connectionInfo), context.resolve(parametricCurve), 0); }
		};

		this.fullCurve2D = new ExpressionBase<CubicCurve2D>(){
			@Override
			public CubicCurve2D evaluate(org.workcraft.dependencymanager.advanced.core.EvaluationContext resolver) {
				CubicCurve2D result = new CubicCurve2D.Double();
				result.setCurve(resolver.resolve(connectionInfo).getFirstShape().getCenter(), resolver.resolve(resolver.resolve(cp1).position()), resolver.resolve(resolver.resolve(cp2).position()), resolver.resolve(connectionInfo).getSecondShape().getCenter());
				return result;
			};
		};
		this.visibleCurve2D = getPartialCurve(fullCurve2D, curveInfo);
		
		
		parametricCurve = new ExpressionBase<ParametricCurve>() {
			@Override
			protected ParametricCurve evaluate(EvaluationContext context) {
				return new Curve(context);
			}
		};
		
	}
	private static Expression<CubicCurve2D> getPartialCurve(
			final Expression<? extends CubicCurve2D> fullCurve2D,
			final Expression<? extends PartialCurveInfo> curveInfo) {
		
		return new ExpressionBase<CubicCurve2D>() {
			@Override
				protected CubicCurve2D evaluate(EvaluationContext context) {
					PartialCurveInfo curve = context.resolve(curveInfo);
					double tEnd = curve.tEnd;
					double tStart = curve.tStart;
				
					CubicCurve2D fullCurve = context.resolve(fullCurve2D);
					
					CurveSplitResult firstSplit = Geometry.splitCubicCurve(fullCurve, tStart);
					CurveSplitResult secondSplit = Geometry.splitCubicCurve(firstSplit.curve2, (tEnd-tStart)/(1-tStart));
					return secondSplit.curve1;
				}
		};
	}
	

	
	public ExpressionBase<Point2D> origin1() {
		return new ExpressionBase<Point2D>() {
			@Override
			protected Point2D evaluate(EvaluationContext context) {
				return context.resolve(connectionInfo).getFirstShape().getCenter();
			}
		};
	}
	
	public ExpressionBase<Point2D> origin2() {
		return new ExpressionBase<Point2D>() {
			@Override
			protected Point2D evaluate(EvaluationContext context) {
				return context.resolve(connectionInfo).getSecondShape().getCenter();
			}
		};
	}
	
	
	private final Expression<PartialCurveInfo> curveInfo;
	private final Expression<CubicCurve2D> fullCurve2D;
	private final Expression<CubicCurve2D> visibleCurve2D;
	private final Expression<? extends ParametricCurve> parametricCurve;
	

	
	private final class Curve implements ParametricCurve {

		public Curve(EvaluationContext resolver) {
			this.resolver = resolver;
		}
		
		private final EvaluationContext resolver;
		
		@Override
		public double getDistanceToCurve(Point2D pt) {
			return pt.distance(getNearestPointOnCurve(pt));
		}

		@Override
		public Point2D getNearestPointOnCurve(Point2D pt) {
			// FIXME: should be done using some proper algorithm
			Point2D nearest = new Point2D.Double(resolver.resolve(fullCurve2D).getX1(), resolver.resolve(fullCurve2D).getY1());
			double nearestDist = Double.MAX_VALUE;
			
			for (double t=0.01; t<=1.0; t+=0.01) {
				Point2D samplePoint = Geometry.getPointOnCubicCurve(resolver.resolve(fullCurve2D), t);
				double distance = pt.distance(samplePoint);
				if (distance < nearestDist)	{
					nearestDist = distance;
					nearest = samplePoint;
				}
			}
			
			return nearest;
		}

		@Override
		public Point2D getPointOnCurve(double t) {
			return Geometry.getPointOnCubicCurve(resolver.resolve(fullCurve2D), t);
		}
		@Override
		public Point2D getDerivativeAt(double t) {
			return Geometry.getDerivativeOfCubicCurve(resolver.resolve(fullCurve2D), t);
		}

		@Override
		public Point2D getSecondDerivativeAt(double t) {
			return Geometry.getSecondDerivativeOfCubicCurve(resolver.resolve(fullCurve2D), t);
		}
		
		@Override
		public Rectangle2D getBoundingBox() {
			Rectangle2D boundingBox = resolver.resolve(fullCurve2D).getBounds2D();
			boundingBox.add(boundingBox.getMinX()-VisualConnection.HIT_THRESHOLD, boundingBox.getMinY()-VisualConnection.HIT_THRESHOLD);
			boundingBox.add(boundingBox.getMinX()-VisualConnection.HIT_THRESHOLD, boundingBox.getMaxY()+VisualConnection.HIT_THRESHOLD);
			boundingBox.add(boundingBox.getMaxX()+VisualConnection.HIT_THRESHOLD, boundingBox.getMinY()-VisualConnection.HIT_THRESHOLD);
			boundingBox.add(boundingBox.getMaxX()+VisualConnection.HIT_THRESHOLD, boundingBox.getMaxY()+VisualConnection.HIT_THRESHOLD);
			return boundingBox;
		}
	}

	Variable<Expression<? extends Collection<? extends Node>>> selectionTracker = new Variable<Expression<? extends Collection<? extends Node>>>(Expressions.constant(Collections.<Node>emptyList()));
	
	Expression<Boolean> controlsHidden = new ExpressionBase<Boolean>(){
		@Override
		protected Boolean evaluate(EvaluationContext context) {
			boolean controlsVisible = true;
			for (Node n : context.resolve(context.resolve(selectionTracker)))
				if (n==context.resolve(cp1) || n == context.resolve(cp2) || n == parent) {
					controlsVisible = false;
					break;
				}
			return controlsVisible;
		}
	};
	
	public void setDefaultControlPoints() {
		Expression<Point2D> p1 = origin1();
		Expression<Point2D> p2 = new ExpressionBase<Point2D>() {
			@Override
			protected Point2D evaluate(EvaluationContext context) {
				return context.resolve(connectionInfo).getSecondShape().getCenter();
			}
		};
		
		BezierControlPoint cp1 = new BezierControlPoint(p1, storage);
		BezierControlPoint cp2 = new BezierControlPoint(p2, storage);
		initControlPoints (cp1, cp2);

		Point2D c1 = eval(p1);
		Point2D c2 = eval(p2);
		cp1.position().setValue(Geometry.lerp(c1, c2, 0.3));
		cp2.position().setValue(Geometry.lerp(c1, c2, 0.6));
		
		finaliseControlPoints();
	}

	
}