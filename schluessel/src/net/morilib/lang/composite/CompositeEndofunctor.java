/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.lang.composite;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
public abstract class CompositeEndofunctor<T> extends CompositeFunctor<T, T> {
	
	
	public boolean cut(T o) {
		return false;
	}
	
	//
	/*package*/ boolean canCut(T t) {
		Method mt1;
		
		try {
			mt1 = getClass().getMethod("canCut", t.getClass());
			return ((Boolean)mt1.invoke(this, t)).booleanValue();
		} catch (NoSuchMethodException e) {
			throw new FunctorException(e);
		} catch (IllegalArgumentException e) {
			throw new FunctorException(e);
		} catch (IllegalAccessException e) {
			throw new FunctorException(e);
		} catch (InvocationTargetException e) {
			throw new FunctorException(e);
		}
	}
	
	//
	@SuppressWarnings("unchecked")
	/*package*/ T doCut(T t) {
		Method mt1;
		
		try {
			mt1 = getClass().getMethod("doCut", t.getClass());
			return (T)mt1.invoke(this, t);
		} catch (NoSuchMethodException e) {
			throw new FunctorException(e);
		} catch (IllegalArgumentException e) {
			throw new FunctorException(e);
		} catch (IllegalAccessException e) {
			throw new FunctorException(e);
		} catch (InvocationTargetException e) {
			throw new FunctorException(e);
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.functor.Functor#newTarget()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T before(T t) {
		Constructor<?> c;
		
		try {
			c = t.getClass().getDeclaredConstructor();
			c.setAccessible(true);
			return (T)c.newInstance();
		} catch (NoSuchMethodException e) {
			throw new FunctorException(e);
		} catch (IllegalArgumentException e) {
			throw new FunctorException(e);
		} catch (InstantiationException e) {
			throw new FunctorException(e);
		} catch (IllegalAccessException e) {
			throw new FunctorException(e);
		} catch (InvocationTargetException e) {
			throw new FunctorException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.composite.CompositeFunctor#after(java.lang.Object)
	 */
	@Override
	public T after(T o) {
		return o;
	}

}
