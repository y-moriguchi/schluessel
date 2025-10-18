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
package net.morilib.parser;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.Format;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import net.morilib.lang.bean.BeanException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/02
 */
public final class BeanIntrospection {
	
	//
	private BeanIntrospection() { }
	
	/**
	 * 
	 * @param mth
	 * @param fm
	 * @param val
	 * @param toset
	 * @throws ParseException
	 */
	public static void setFormattedProperty(
			Method mth,
			Format fm,
			String val,
			Object toset) throws ParseException {
		try {
			Object f;
			
			if(fm != null) {
				f = fm.parseObject(val);
			} else {
				f = val;
			}
			
			if(f instanceof Number) {
				Number z = (Number)f;
				Class<?>[] klsen;
				
				klsen = mth.getParameterTypes();
				if(klsen[0].equals(Integer.TYPE) ||
						klsen[0].isAssignableFrom(Integer.class)) {
					mth.invoke(toset, z.intValue());
				} else if(klsen[0].equals(Long.TYPE) ||
						klsen[0].isAssignableFrom(Long.class)) {
					mth.invoke(toset, z.longValue());
				} else if(klsen[0].equals(Double.TYPE) ||
						klsen[0].isAssignableFrom(Double.class)) {
					mth.invoke(toset, z.doubleValue());
				} else if(klsen[0].equals(Short.TYPE) ||
						klsen[0].isAssignableFrom(Short.class)) {
					mth.invoke(toset, z.shortValue());
				} else if(klsen[0].equals(Byte.TYPE) ||
						klsen[0].isAssignableFrom(Byte.class)) {
					mth.invoke(toset, z.byteValue());
				} else if(klsen[0].equals(Float.TYPE) ||
						klsen[0].isAssignableFrom(Float.class)) {
					mth.invoke(toset, z.floatValue());
				} else {
					mth.invoke(toset, f);
				}
			} else {
				mth.invoke(toset, f);
			}
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		} catch (InvocationTargetException e) {
			throw new BeanException(e);
		}
	}
	
	/**
	 * 
	 * @param fld
	 * @param fm
	 * @param val
	 * @param toset
	 * @throws ParseException
	 */
	public static void setFormattedField(
			Field  fld,
			Format fm,
			String val,
			Object toset) throws ParseException {
		try {
			Object f;
			
			if(fm != null) {
				f = fm.parseObject(val);
			} else {
				f = val;
			}
			
			if(f instanceof Number) {
				Number z = (Number)f;
				Class<?> kls;
				
				kls = fld.getType();
				if(kls.equals(Integer.TYPE) ||
						kls.isAssignableFrom(Integer.class)) {
					fld.set(toset, z.intValue());
				} else if(kls.equals(Long.TYPE) ||
						kls.isAssignableFrom(Long.class)) {
					fld.set(toset, z.longValue());
				} else if(kls.equals(Double.TYPE) ||
						kls.isAssignableFrom(Double.class)) {
					fld.set(toset, z.doubleValue());
				} else if(kls.equals(Short.TYPE) ||
						kls.isAssignableFrom(Short.class)) {
					fld.set(toset, z.shortValue());
				} else if(kls.equals(Byte.TYPE) ||
						kls.isAssignableFrom(Byte.class)) {
					fld.set(toset, z.byteValue());
				} else if(kls.equals(Float.TYPE) ||
						kls.isAssignableFrom(Float.class)) {
					fld.set(toset, z.floatValue());
				} else {
					fld.set(toset, f);
				}
			} else {
				fld.set(toset, f);
			}
		} catch (IllegalArgumentException e) {
			throw new BeanException(e);
		} catch (IllegalAccessException e) {
			throw new BeanException(e);
		}
	}
	
	/**
	 * 
	 * @param kls
	 * @param format
	 * @return
	 */
	public static Format introspectFormat(
			Class<?> kls, String format) {
		if(     kls.equals(Integer.TYPE)  ||
				kls.equals(Long.TYPE)     ||
				kls.equals(Double.TYPE)   ||
				kls.equals(Short.TYPE)    ||
				kls.equals(Byte.TYPE)     ||
				kls.equals(Float.TYPE)    ||
				kls.isAssignableFrom(Integer.class) ||
				kls.isAssignableFrom(Long.class)    ||
				kls.isAssignableFrom(Double.class)  ||
				kls.isAssignableFrom(Short.class)   ||
				kls.isAssignableFrom(Byte.class)    ||
				kls.isAssignableFrom(Float.class)) {
			DecimalFormat df = null;
			
			if(format != null && format.equals("")) {
				df = new DecimalFormat(format);
			}
			return df;
		} else if(kls.isAssignableFrom(java.util.Date.class)) {
			SimpleDateFormat df = null;
			
			if(format != null && format.equals("")) {
				df = new SimpleDateFormat(format);
			} else {
				throw new IllegalArgumentException(
						"date format required");
			}
			return df;
		} else if(kls.equals(BigDecimal.class)) {
			DecimalFormat df = null;
			
			if(format != null && format.equals("")) {
				df = new DecimalFormat(format);
			}
			df.setParseBigDecimal(true);
			return df;
		}
		return null;
	}
	
}
