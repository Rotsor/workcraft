package org.workcraft.dependencymanager.advanced.core;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashSet;

import org.workcraft.dependencymanager.util.listeners.Listener;
import org.workcraft.dependencymanager.util.listeners.WeakFireOnceListenersCollection;

public abstract class ExpressionBase<T> implements Expression<T> {
	
	public static class ValueHandleTuple<T> {
		public final T value;
		public final Listener handle;
		public ValueHandleTuple(T value, Listener handle) {
			this.handle = handle;
			this.value = value;
		}
	}
	
	static class Cache<T> implements Listener {
		public T value;
		public boolean filled = false;
		public boolean valid = true;
		HashSet<Listener> dependencies = new HashSet<Listener>(); // used to make sure the dependencies don't get garbage collected too early
		//Exception stackTrace = new Exception("Stack trace");

		WeakFireOnceListenersCollection listeners = new WeakFireOnceListenersCollection();
		
		public <T2> T2 getValue(Expression<T2> expr) {
			ValueHandleTuple<T2> res = expr.getValue(this);
			Listener l = res.handle;
			dependencies.add(l);
			return res.value;
		}
		
/*		@Override
		public String toString() {
			StringWriter writer = new StringWriter();
			stackTrace.printStackTrace(new PrintWriter(writer));
			return super.toString() + " created with the following stack trace: \n" + writer.toString(); 
		}*/
		
		@Override
		public void changed() {
			if(!filled) {
				throw new RuntimeException("error: tried to invalidate an expression during its evaluation");
			}
			
			if(listeners != null) {
				valid = false;
				listeners.changed();
				listeners = null;
			}
		}
	}
	
	Cache<T> cache;
	
	public final ValueHandleTuple<T> getValue(Listener subscriber) {
		if(cache != null && cache.valid) {
			cache.listeners.addListener(subscriber);
			return new ValueHandleTuple<T>(cache.value, cache);
		}
		else {
			final Cache<T> c = new Cache<T>();
			T result = evaluate(new EvaluationContext() {
				@Override
				public <T2> T2 resolve(Expression<T2> dependency) {
					return c.getValue(dependency);
				}
			});
			c.value = result;
			c.filled = true;
			WeakFireOnceListenersCollection listeners = c.listeners;
			listeners.addListener(subscriber);
			
			cache = c;
			
			return new ValueHandleTuple<T>(result, cache);
		}
	}
	
	public final void refresh() {
		if(cache!=null) {
			Cache<?> c = cache;
			cache = null;
			c.changed();
		}
	}
	
	abstract protected T evaluate(EvaluationContext context);
}
