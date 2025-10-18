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
package net.morilib.lang.bean;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
/*package*/ class BeanProperty extends Property {
	
	//
	private static final Object[] ZEROOBJ = new Object[0];
	
	//
	private PropertyDescriptor prop;

	/**
	 * @param propertyDescriptor
	 */
	/*package*/ BeanProperty(PropertyDescriptor propertyDescriptor) {
		prop = propertyDescriptor;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#get(java.lang.Object)
	 */
	@Override
	public Object get(Object bean) {
		try {
			return prop.getReadMethod().invoke(bean, ZEROOBJ);
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		} catch (InvocationTargetException e) {
			throw new BeanException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#set(java.lang.Object, java.lang.Object)
	 */
	@Override
	public void set(Object bean, Object value) {
		try {
			prop.getWriteMethod().invoke(bean, value);
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		} catch (InvocationTargetException e) {
			throw new BeanException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#setAccesible(boolean)
	 */
	@Override
	public void setAccessible(boolean b) {
		prop.getReadMethod().setAccessible(b);
		prop.getWriteMethod().setAccessible(b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#isAnnotationPresent(java.lang.Class)
	 */
	@Override
	public boolean isAnnotationPresent(
			Class<? extends Annotation> annotationClass) {
		Method r = prop.getReadMethod();
		Method w = prop.getWriteMethod();
		
		return (r.isAnnotationPresent(annotationClass) ||
				w.isAnnotationPresent(annotationClass));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getAnnotation(java.lang.Class)
	 */
	@Override
	public <T extends Annotation> T getAnnotation(
			Class<T> annotationClass) {
		Method r = prop.getReadMethod();
		Method w = prop.getWriteMethod();
		T a;
		
		a = r.getAnnotation(annotationClass);
		return (a != null) ? a : w.getAnnotation(annotationClass);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getAnnotations()
	 */
	@Override
	public Annotation[] getAnnotations() {
		Method r = prop.getReadMethod();
		Method w = prop.getWriteMethod();
		List<Annotation> res;
		
		res = new ArrayList<Annotation>(
				Arrays.asList(r.getAnnotations()));
		
		outer:
		for(Annotation a : w.getAnnotations()) {
			for(Annotation b : res) {
				if(a.annotationType().equals(b.annotationType())) {
					if(a.equals(b)) {
						continue outer;
					} else {
						throw new BeanException(
								"contradicted annotation");
					}
				}
			}
			res.add(a);
		}
		return res.toArray(new Annotation[0]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getName()
	 */
	@Override
	public String getName() {
		return prop.getName();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getPropertyType()
	 */
	@Override
	public Class<?> getType() {
		return prop.getPropertyType();
	}

}
