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

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
/*package*/ class FieldProperty extends Property {
	
	//
	private Field field;

	/**
	 * @param declaredField
	 */
	/*package*/ FieldProperty(Field declaredField) {
		field = declaredField;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#get(java.lang.Object)
	 */
	@Override
	public Object get(Object bean) {
		try {
			return field.get(bean);
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#set(java.lang.Object, java.lang.Object)
	 */
	@Override
	public void set(Object bean, Object value) {
		// TODO Auto-generated method stub
		try {
			field.set(bean, value);
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#setAccesible(boolean)
	 */
	@Override
	public void setAccessible(boolean b) {
		field.setAccessible(b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#isAnnotationPresent(java.lang.Class)
	 */
	@Override
	public boolean isAnnotationPresent(
			Class<? extends Annotation> annotationClass) {
		return field.isAnnotationPresent(annotationClass);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getAnnotation(java.lang.Class)
	 */
	@Override
	public <T extends Annotation> T getAnnotation(
			Class<T> annotationClass) {
		return field.getAnnotation(annotationClass);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getAnnotations()
	 */
	@Override
	public Annotation[] getAnnotations() {
		return field.getAnnotations();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getName()
	 */
	@Override
	public String getName() {
		return field.getName();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.bean.Property#getPropertyType()
	 */
	@Override
	public Class<?> getType() {
		return field.getType();
	}
	
}
