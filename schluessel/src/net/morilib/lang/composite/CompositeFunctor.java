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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.morilib.lang.bean.BeanInfo2;
import net.morilib.lang.bean.Property;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
public abstract class CompositeFunctor<T, S> {
	
	
	public abstract S before(T o);
	
	
	public abstract S after(S o);
	
	//
	/*package*/ boolean canCut(T o) throws NoSuchMethodException {
		return false;
	}
	
	//
	/*package*/ S doCut(T o) {
		throw new UnsupportedOperationException();
	}
	
	
	@SuppressWarnings("unchecked")
	private List<?> getlst(
			Property f, T t
			) throws IllegalArgumentException, IllegalAccessException {
		List<Object> r2 = new ArrayList<Object>();
		Collection<T> cl1;
		
		cl1 = (Collection<T>)f.get(t);
		for(T o : cl1) {
			r2.add(transform(o));
		}
		return r2;
	}
	
	@SuppressWarnings("unchecked")
	public final S transform(T t) {
		try {
			if(canCut(t)) {
				return doCut(t);
			}
			
			//
			Method mt1 = getClass().getMethod("before", t.getClass());
			S res      = (S)mt1.invoke(this, t);
			BeanInfo2<T> bn1 = BeanInfo2.getInfo(t);
			BeanInfo2<S> bn2 = BeanInfo2.getInfo(res);
			
			for(Property f : bn1.getPropertiesAndFields()) {
				Property f2 = bn2.getPropertyOrField(f.getName());
				
				if(f.isAnnotationPresent(TraverseCollection.class)) {
					TraverseCollection a;
					Collection<S> cl2;
					Class<?> c3;
					Constructor<?> cr2;
					
					f.setAccessible(true);
					a = f.getAnnotation(TraverseCollection.class);
					c3 = a.value();
					
					cr2 = c3.getDeclaredConstructor(f.getType());
					cl2 = (Collection<S>)cr2.newInstance(getlst(f, t));
					f2.set(res, cl2);
				} else if(f.isAnnotationPresent(TraverseArray.class)) {
					f.setAccessible(true);
					f2.set(res, getlst(f, t).toArray());
				} else if(f.isAnnotationPresent(Traverse.class)) {
					f.setAccessible(true);
					f2.set(res, transform((T)f.get(t)));
				}
			}
			
			//
			Method mt2 = getClass().getMethod("after", res.getClass());
			return (S)mt2.invoke(this, res);
		} catch (NoSuchMethodException e) {
			throw new FunctorClassContractException(e);
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
	
}
