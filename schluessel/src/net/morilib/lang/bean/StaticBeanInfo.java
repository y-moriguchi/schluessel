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

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
/*package*/ final class StaticBeanInfo<T> extends BeanInfo2<T> {
	
	//
	private Class<?> klass;
	
	
	/*package*/ StaticBeanInfo(Class<T> t) {
		klass = t;
	}
	
	
	@SuppressWarnings("unchecked")
	public static<T> StaticBeanInfo<T> newBean(T t) {
		return new StaticBeanInfo<T>((Class<T>)t.getClass());
	}
	
	
	public Property getPropertyOrField(String name) {
		try {
			return new FieldProperty(klass.getField(name));
		} catch (NoSuchFieldException e) {
			// do nothing
		}
		
		try {
			return new FieldProperty(klass.getDeclaredField(name));
		} catch (NoSuchFieldException e1) {
			// do nothing
		}
		
		try {
			return new BeanProperty(
					new PropertyDescriptor(name, klass));
		} catch (IntrospectionException e2) {
			throw new BeanException(e2);
		}
	}
	
	
	public Property[] getPropertiesAndFields() {
		Field[] fs;
		List<Property> res = new ArrayList<Property>();
		
		fs = klass.getFields();
		for(Field f : fs) {
			if(!res.contains(f)) {
				res.add(new FieldProperty(f));
			}
		}
	
		fs = klass.getDeclaredFields();
		for(Field f : fs) {
			if(!res.contains(f)) {
				res.add(new FieldProperty(f));
			}
		}
		
		try {
			BeanInfo bi = Introspector.getBeanInfo(klass);
			
			for(PropertyDescriptor f : bi.getPropertyDescriptors()) {
				if(!res.contains(f)) {
					res.add(new BeanProperty(f));
				}
			}
		} catch (IntrospectionException e2) {
			throw new BeanException(e2);
		}
		
		return res.toArray(new Property[0]);
	}
	
	
	public Property getProperty(String name) {
		try {
			return new BeanProperty(
					new PropertyDescriptor(name, klass));
		} catch (IntrospectionException e2) {
			throw new BeanException(e2);
		}
	}
	
	
	public Property[] getProperties() {
		List<Property> res = new ArrayList<Property>();
		
		try {
			BeanInfo bi = Introspector.getBeanInfo(klass);
			
			for(PropertyDescriptor f : bi.getPropertyDescriptors()) {
				if(!res.contains(f)) {
					res.add(new BeanProperty(f));
				}
			}
		} catch (IntrospectionException e2) {
			throw new BeanException(e2);
		}
		
		return res.toArray(new Property[0]);
	}
	
	
	@SuppressWarnings("unchecked")
	public T newInstance(Object... args) {
		Class<?>[] clss;
		Constructor<T> c;
		
		if(args == null) {
			throw new NullPointerException();
		}
		
		try {
			clss = new Class<?>[args.length];
			for(int i = 0; i < args.length; i++) {
				clss[i] = args[i].getClass();
			}
			c = (Constructor<T>)klass.getConstructor(clss);
			return (T)c.newInstance(args);
		} catch (NoSuchMethodException e) {
			throw new BeanException(e);
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (InstantiationException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		} catch (InvocationTargetException e) {
			throw new BeanException(e);
		}
		
	}
	
	
	@SuppressWarnings("unchecked")
	public T forceNewInstance(Object... args) {
		Class<?>[] clss;
		Constructor<T> c;
		
		if(args == null) {
			throw new NullPointerException();
		}
		
		try {
			clss = new Class<?>[args.length];
			for(int i = 0; i < args.length; i++) {
				clss[i] = args[i].getClass();
			}
			c = (Constructor<T>)klass.getDeclaredConstructor(clss);
			c.setAccessible(true);
			return (T)c.newInstance(args);
		} catch (NoSuchMethodException e) {
			throw new BeanException(e);
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (InstantiationException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		} catch (InvocationTargetException e) {
			throw new BeanException(e);
		}
		
	}
	
}
