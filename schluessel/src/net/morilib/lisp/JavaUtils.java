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
package net.morilib.lisp;

import java.beans.IndexedPropertyDescriptor;
import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/17
 */
public final class JavaUtils {

	//
	private static final Object NOTFOUND = new Object();

	//
	private JavaUtils() {}

	//
	private static Object invokeMethod0(
			Method mth, Object obj,
			List<Datum> lst) throws ParameterNotFoundException {
		Class<?>[] cls = mth.getParameterTypes();
		boolean    va  = mth.isVarArgs();

		if(LispJavaUtils.isSameClasses(cls, lst, va)) {
			Object[] args = LispJavaUtils.toJavaInstances(cls, lst, va);

			return invoke(mth, obj, args);
		} else {
			return NOTFOUND;
		}
	}

	//
	private static Object newInstance0(
			Constructor<?> mth,
			List<Datum> lst) throws ParameterNotFoundException {
		Class<?>[] cls = mth.getParameterTypes();
		boolean    va  = mth.isVarArgs();

		if(LispJavaUtils.isSameClasses(cls, lst, va)) {
			Object[] args = LispJavaUtils.toJavaInstances(cls, lst, va);

			return invoke(mth, args);
		} else {
			return NOTFOUND;
		}
	}

	//
	/*package*/ static Object invoke(
			Method mtd, Object obj, Object... args
			) throws ParameterNotFoundException {
		try {
			Object res = mtd.invoke(obj, args);

			if(mtd.getReturnType().isAssignableFrom(Void.TYPE)) {
				return Undef.UNDEF;
			} else {
				return (res == null) ? JavaNull.JAVA_NULL : res;
			}
		} catch (IllegalArgumentException e) {
			throw new JavaException(e);
		} catch (IllegalAccessException e) {
			throw new JavaException(e);
		} catch (InvocationTargetException e) {
			throw new JavaTargetException(e.getCause());
		} catch (NullPointerException e) {
			throw new ParameterNotFoundException();
		}
	}

	//
	/*package*/ static Object invoke(
			Constructor<?> mtd, Object... args
			) throws ParameterNotFoundException {
		try {
			return mtd.newInstance(args);
		} catch (IllegalArgumentException e) {
			throw new JavaException(e);
		} catch (IllegalAccessException e) {
			throw new JavaException(e);
		} catch (InvocationTargetException e) {
			throw new JavaTargetException(e.getCause());
		} catch (NullPointerException e) {
			throw new ParameterNotFoundException();
		} catch (InstantiationException e) {
			throw new ParameterNotFoundException();
		}
	}

	//
	/*package*/ static Object invokeMethod(
			Method mth, Object obj,
			List<Datum> lst) throws ParameterNotFoundException {
		Object res = invokeMethod0(mth, obj, lst);

		if(res == NOTFOUND) {
			throw new ParameterNotFoundException();
		} else {
			return res;
		}
	}

	//
	/*package*/ static Object newInstance(
			Constructor<?> mth,
			List<Datum> lst) throws ParameterNotFoundException {
		Object res = newInstance0(mth, lst);

		if(res == NOTFOUND) {
			throw new ParameterNotFoundException();
		} else {
			return res;
		}
	}

	/**
	 * 
	 * @param klass
	 * @param obj
	 * @param name
	 * @param lst
	 * @return
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeMethod(
			Class<?> klass, Object obj, String name, List<Datum> lst
			) throws ParameterNotFoundException {
		Method[] cns = klass.getMethods();

		for(int i = 0; i < cns.length; i++) {
			String     cnm = cns[i].getName();

			if(cnm.equals(name)) {
				Object res = invokeMethod0(cns[i], obj, lst);

				if(res != NOTFOUND) {
					return res;
				}
			}
		}
		throw new ParameterNotFoundException();
	}

	/**
	 * 
	 * @param obj
	 * @param name
	 * @param lst
	 * @return
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeMethod(
			Object obj, String name, List<Datum> lst
			) throws ParameterNotFoundException {
		return invokeMethod(obj.getClass(), obj, name, lst);
	}

	/**
	 * 
	 * @param klass
	 * @param lst
	 * @return
	 * @throws ParameterNotFoundException
	 */
	public static Object newInstance(
			Class<?> klass, List<Datum> lst
			) throws ParameterNotFoundException {
		Constructor<?>[] cns = klass.getConstructors();

		for(int i = 0; i < cns.length; i++) {
			Object res = newInstance0(cns[i], lst);

			if(res != NOTFOUND) {
				return res;
			}
		}
		throw new ParameterNotFoundException();
	}

	/**
	 * 
	 * @param obj
	 * @param pd
	 * @return
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeGetter(
			Object obj,
			PropertyDescriptor pd) throws ParameterNotFoundException {
		Method mtd = pd.getReadMethod();

		if(mtd == null) {
			throw new ParameterNotFoundException();
			//return null;
		} else {
			Class<?>[] prm = mtd.getParameterTypes();

			if(prm.length != 0) {
				throw new ParameterNotFoundException();
			}
			return invoke(mtd, obj);
		}
	}

	/**
	 * 
	 * @param obj
	 * @param prop
	 * @return
	 * @throws IntrospectionException
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeGetter(
			Object obj, String prop
			) throws IntrospectionException,
			ParameterNotFoundException {
		PropertyDescriptor pd =
			new PropertyDescriptor(prop, obj.getClass());

		return invokeGetter(obj, pd);
	}

	//
	/*package*/ static Object invokeSetter(
			Object obj,
			PropertyDescriptor pd,
			Datum d) throws ParameterNotFoundException {
		Method mtd = pd.getWriteMethod();

		if(mtd == null) {
			throw new ParameterNotFoundException();
			//return null;
		} else {
			Class<?>[] prm = mtd.getParameterTypes();

			if(prm.length != 1 ||
					!LispJavaUtils.isSameClass(prm[0], d)) {
				throw new ParameterNotFoundException();
			}
			return invoke(mtd, obj,
					LispJavaUtils.toJavaInstance(prm[0], d));
		}
	}

	/**
	 * 
	 * @param obj
	 * @param prop
	 * @param d
	 * @return
	 * @throws IntrospectionException
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeSetter(
			Object obj, String prop, Datum d
			) throws IntrospectionException,
			ParameterNotFoundException {
		PropertyDescriptor pd =
			new PropertyDescriptor(prop, obj.getClass());

		return invokeSetter(obj, pd, d);
	}

	//
	/*package*/ static Object invokeGetter(
			Object obj,
			IndexedPropertyDescriptor pd,
			int index) throws ParameterNotFoundException {
		Method mtd = pd.getIndexedReadMethod();

		if(mtd == null) {
			throw new ParameterNotFoundException();
		} else {
			Class<?>[] prm = mtd.getParameterTypes();

			if(prm.length != 1 ||
					!LispJavaUtils.isAssignableInt(prm[0])) {
				throw new ParameterNotFoundException();
			}
			return invoke(mtd, obj, index);
		}
	}

	/**
	 * 
	 * @param obj
	 * @param prop
	 * @param index
	 * @return
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeGetter(
			Object obj, String prop, int index
			) throws ParameterNotFoundException {
		IndexedPropertyDescriptor pd;

		try {
			pd = new IndexedPropertyDescriptor(prop, obj.getClass());
			return invokeGetter(obj, pd, index);
		} catch (IntrospectionException e) {
			throw new ParameterNotFoundException(e);
		}
	}

	//
	/*package*/ static Object invokeSetter(
			Object obj,
			IndexedPropertyDescriptor pd,
			int index,
			Datum d) throws ParameterNotFoundException {
		Method mtd = pd.getIndexedWriteMethod();

		if(mtd == null) {
			throw new ParameterNotFoundException();
		} else {
			Class<?>[] prm = mtd.getParameterTypes();

			if(prm.length != 2
					|| !LispJavaUtils.isAssignableInt(prm[0])
					|| !LispJavaUtils.isSameClass(prm[1], d)) {
				throw new ParameterNotFoundException();
			}
			return invoke(mtd, obj, index,
					LispJavaUtils.toJavaInstance(prm[1], d));
		}
	}

	/**
	 * 
	 * @param obj
	 * @param prop
	 * @param index
	 * @param d
	 * @return
	 * @throws ParameterNotFoundException
	 */
	public static Object invokeSetter(
			Object obj, String prop, int index, Datum d
			) throws ParameterNotFoundException {
		IndexedPropertyDescriptor pd;

		try {
			pd = new IndexedPropertyDescriptor(prop, obj.getClass());
			return invokeSetter(obj, pd, index, d);
		} catch (IntrospectionException e) {
			throw new ParameterNotFoundException(e);
		}
	}

	/**
	 * 
	 * @param obj
	 * @param field
	 * @return
	 * @throws NoSuchFieldException
	 */
	public static Object getField(
			Object obj, String field) throws NoSuchFieldException {
		try {
			Field f = obj.getClass().getField(field);

			return f.get(obj);
		} catch (SecurityException e) {
			throw new JavaException(e);
		} catch (IllegalArgumentException e) {
			throw new JavaException(e);
		} catch (IllegalAccessException e) {
			throw new JavaException(e);
		}
	}

	/**
	 * 
	 * @param obj
	 * @param field
	 * @param d
	 * @throws NoSuchFieldException
	 * @throws ParameterNotFoundException
	 */
	public static void setField(
			Object obj, String field, Datum d
			) throws NoSuchFieldException, ParameterNotFoundException {
		try {
			Field  f = obj.getClass().getField(field);
			Object dj;

			if(!LispJavaUtils.isSameClass(f.getType(), d)) {
				throw new ParameterNotFoundException();
			}
			dj = LispJavaUtils.toJavaInstance(f.getType(), d);
			f.set(obj, dj);
		} catch (IllegalArgumentException e) {
			throw new JavaException(e);
		} catch (IllegalAccessException e) {
			throw new JavaException(e);
		}
	}

	/**
	 * 
	 * @param cls
	 * @param field
	 * @return
	 * @throws NoSuchFieldException
	 */
	public static Object getStaticField(
			Class<?> cls, String field) throws NoSuchFieldException {
		if(cls == null || field == null) {
			throw new NullPointerException();
		}

		try {
			return cls.getField(field).get(null);
		} catch(SecurityException e) {
			throw new JavaException(e);
		} catch(IllegalArgumentException e) {
			throw new JavaException(e);
		} catch(IllegalAccessException e) {
			throw new JavaException(e);
		} catch(NullPointerException e) {
			throw new NoSuchFieldException();
		}
	}

	/**
	 * 
	 * @param cls
	 * @param field
	 * @param d
	 * @throws NoSuchFieldException
	 * @throws ParameterNotFoundException
	 */
	public static void setStaticField(
			Class<?> cls, String field, Datum d
			) throws NoSuchFieldException, ParameterNotFoundException {
		if(cls == null || field == null) {
			throw new NullPointerException();
		}

		try {
			Field  f = cls.getField(field);
			Object dj;

			if(!LispJavaUtils.isSameClass(f.getType(), d)) {
				throw new ParameterNotFoundException();
			}
			dj = LispJavaUtils.toJavaInstance(f.getType(), d);

			try {
				f.set(null, dj);
			} catch(NullPointerException e) {
				throw new NoSuchFieldException();
			}
		} catch (IllegalArgumentException e) {
			throw new JavaException(e);
		} catch (IllegalAccessException e) {
			throw new JavaException(e);
		}
	}

}
