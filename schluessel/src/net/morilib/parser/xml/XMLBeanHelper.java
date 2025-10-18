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
package net.morilib.parser.xml;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.ReflectionException;
import net.morilib.lang.bean.ClassProposition;
import net.morilib.lang.proposition.Proposition;
import net.morilib.lingua.plural.PluralConverter;
import net.morilib.lingua.plural.SimplePluralConverter;
import net.morilib.util.collection.CollectionTypes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/24
 */
/*package*/ class XMLBeanHelper {
	
	//
	private XMLBeanHelper() { }
	
	//
	private static final PluralConverter _E =
		SimplePluralConverter.ENGLISH;
	
	//
	private Map<Class<?>, Class<?>> defaults;
	
	/*initialize*/ {
		defaults = new LinkedHashMap<Class<?>, Class<?>>();
		defaults.put(
				net.morilib.util.primitive.ByteList.class,
				net.morilib.util.primitive.ByteArrayVector.class);
		defaults.put(
				net.morilib.util.primitive.ShortList.class,
				net.morilib.util.primitive.ShortArrayVector.class);
		defaults.put(
				net.morilib.util.primitive.IntegerList.class,
				net.morilib.util.primitive.IntegerArrayVector.class);
		defaults.put(
				net.morilib.util.primitive.LongList.class,
				net.morilib.util.primitive.LongArrayVector.class);
		defaults.put(
				net.morilib.util.primitive.DoubleList.class,
				net.morilib.util.primitive.DoubleArrayVector.class);
		defaults.put(
				net.morilib.util.primitive.FloatList.class,
				net.morilib.util.primitive.FloatArrayVector.class);
		defaults.put(
				net.morilib.util.primitive.CharacterList.class,
				net.morilib.util.primitive.CharacterArrayVector.class);
		defaults.put(List.class, ArrayList.class);
		defaults.put(
				net.morilib.util.primitive.ByteSet.class,
				net.morilib.util.primitive.ByteHashSet.class);
		defaults.put(
				net.morilib.util.primitive.ShortSet.class,
				net.morilib.util.primitive.ShortHashSet.class);
		defaults.put(
				net.morilib.util.primitive.IntegerSet.class,
				net.morilib.util.primitive.IntegerHashSet.class);
		defaults.put(
				net.morilib.util.primitive.LongSet.class,
				net.morilib.util.primitive.LongHashSet.class);
		defaults.put(
				net.morilib.util.primitive.DoubleSet.class,
				net.morilib.util.primitive.DoubleHashSet.class);
		defaults.put(
				net.morilib.util.primitive.FloatSet.class,
				net.morilib.util.primitive.FloatHashSet.class);
		defaults.put(
				net.morilib.util.primitive.CharacterSet.class,
				net.morilib.util.primitive.CharacterHashSet.class);
		defaults.put(Set.class,  HashSet.class);
		defaults.put(Map.class,  HashMap.class);
	}
	
	/**
	 * @return
	 */
	public static XMLBeanHelper getInstance() {
		return new XMLBeanHelper();
	}
	
	/**
	 * Warning: This method manipulates accessible flag.
	 * If you use security manager,
	 * you should configure security policy.
	 * 
	 * @param cls
	 * @return
	 */
	public Object newInstance(Class<?> cls) {
		Constructor<?> c;
		Field[] fs;
		Object res;
		
		try {
			c = cls.getDeclaredConstructor();
			c.setAccessible(true);
			res = c.newInstance();
			
			// initialize collections
			fs = cls.getDeclaredFields();
			for(Field f : fs) {
				Class<?> c2 = f.getType();
				
				f.setAccessible(true);
				for(Class<?> c0 : defaults.keySet()) {
					if(c0.isAssignableFrom(c2)) {
						Object l;
						
						if(c2.isInterface()) {
							l = defaults.get(c0).newInstance();
						} else {
							l = c2.newInstance();
						}
						f.set(res, l);
						break;
					}
				}
			}
			return res;
		} catch (NoSuchMethodException e) {
			throw new ReflectionException(e);
		} catch (IllegalArgumentException e) {
			throw new ReflectionException(e);
		} catch (InstantiationException e) {
			throw new ReflectionException(e);
		} catch (IllegalAccessException e) {
			throw new ReflectionException(e);
		} catch (InvocationTargetException e) {
			throw new ReflectionException(e);
		}
	}
	
	//
	private static Object checkPrimitive(
			Class<?> c, String msg) {
		if(c.isPrimitive()) {
			throw new XMLBeanParseException(msg);
		} else {
			return null;
		}
	}
	
	//
	private static Number parseNum(
			String fs, String v, boolean isBig) {
		DecimalFormat nf = new DecimalFormat(fs);
		
		nf.setParseBigDecimal(isBig);
		try {
			return nf.parse(v).floatValue();
		} catch (ParseException e) {
			throw new XMLBeanParseException(e.getMessage());
		}
	}
	
	//
	private static String trim2(Object w) {
		String r;
		
		if(w == null) {
			return null;
		} else {
			if((r = w.toString()) != null) {
				r = r.replaceAll("\\A(\\p{javaWhitespace})+", "");
				r = r.replaceAll("(\\p{javaWhitespace})+\\G", "");
				return r.equals("") ? null : r;
			} else {
				return null;
			}
		}
	}
	
	//
	private static String trim3(Object w) {
		String r;
		
		if(w == null) {
			return null;
		} else {
			if((r = w.toString()) != null) {
				r = r.replaceAll("\\A(\\p{javaWhitespace})+", "");
				r = r.replaceAll("(\\p{javaWhitespace})+\\G", "");
				return r;
			} else {
				return null;
			}
		}
	}
	
	/**
	 * 
	 * @param f
	 * @param v
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 */
	public static Object cast(
			Class<?> c, Object w, AnnotatedElement ae) {
		XMLBeanFormat a = ae.getAnnotation(XMLBeanFormat.class);
		String fs = null;
		String v;
		
		if(a != null) {
			fs = a.value();
		}
		
		if(String.class.isAssignableFrom(c)) {
			if(w == null) {
				return null;
			} else if(!(w instanceof String)) {
				throw new ClassCastException();
			}
			v = trim3(w);
			v = v.replaceAll("\n(\\p{javaWhitespace})+", "\n");
			return v;
		} else if(Integer.TYPE.isAssignableFrom(c) ||
				Integer.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Integer required");
			}
			return Integer.parseInt(v.toString());
		} else if(Long.TYPE.isAssignableFrom(c) ||
				Long.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Long required");
			}
			return Long.parseLong(v);
		} else if(Boolean.TYPE.isAssignableFrom(c) ||
				Boolean.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Boolean required");
			}
			return (v.equalsIgnoreCase("true") ||
					v.equalsIgnoreCase("on"));
		} else if(Float.TYPE.isAssignableFrom(c) ||
				Float.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Float required");
			} else if(fs != null && !fs.equals("")) {
				return parseNum(fs, v, false).floatValue();
			} else {
				return Float.parseFloat(v);
			}
		} else if(Double.TYPE.isAssignableFrom(c) ||
				Double.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Double required");
			} else if(fs != null && !fs.equals("")) {
				return parseNum(fs, v, false).doubleValue();
			} else {
				return Double.parseDouble(v);
			}
		} else if(Short.TYPE.isAssignableFrom(c) ||
				Short.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Short required");
			}
			return Short.parseShort(v);
		} else if(Byte.TYPE.isAssignableFrom(c) ||
				Byte.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Byte required");
			}
			return Byte.parseByte(v);
		} else if(Character.TYPE.isAssignableFrom(c) ||
				Character.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return checkPrimitive(c, "Character required");
			} else if(v.length() == 1) {
				return v.charAt(0);
			} else {
				return v.charAt(0);
			}
		} else if(BigDecimal.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return null;
			} else if(fs != null && !fs.equals("")) {
				Number n = parseNum(fs, v, true);
				
				return (n instanceof BigDecimal) ? n : null;
			} else {
				return new BigDecimal(v);
			}
		} else if(java.util.Date.class.isAssignableFrom(c)) {
			v = trim2(w);
			if(v == null) {
				return null;
			} else if(fs != null && !fs.equals("")) {
				try {
					SimpleDateFormat sf = new SimpleDateFormat(fs);
					
					return sf.parse(v);
				} catch (ParseException e) {
					throw new XMLBeanParseException(e.getMessage());
				}
			} else {
				throw new XMLBeanParseException(
						"Date format required");
			}
		} else {
			if(!c.isAssignableFrom(w.getClass())) {
				throw new ClassCastException();
			}
			return w;
		}
	}
	
	/**
	 * @param o
	 * @param qName
	 * @param value
	 */
	public void setField(Object o, String q, Object val) {
		Field[] fs;
		
		try {
			fs = o.getClass().getDeclaredFields();
			for(Field f : fs) {
				Class<?> c2 = f.getType();
				
				if(Collection.class.isAssignableFrom(c2) &&
						_E.isPlural(q, f.getName())) {
					Object l;
					Method m;
					
					f.setAccessible(true);
					l = f.get(o);
					m = l.getClass().getMethod("add", Object.class);
					m.invoke(l, val);
					return;
				} else if(Map.class.isAssignableFrom(c2) &&
						_E.isPlural(q, f.getName())) {
					IDAttribute a;
					Object p;
					Method m;
					Field  g;
					
					f.setAccessible(true);
					p  = f.get(o);
					m  = p.getClass().getMethod(
							"put", Object.class, Object.class);
					a  = f.getAnnotation(IDAttribute.class);
					g  = val.getClass().getDeclaredField(
							(a == null) ? "id" : a.value());
					
					if(!String.class.isAssignableFrom(g.getType())) {
						throw new XMLBeanParseException();
					}
					g.setAccessible(true);
					m.invoke(p, g.get(val), val);
					return;
				} else if(q.equals(f.getName())) {
					f.setAccessible(true);
					f.set(o, val);
					return;
				}
			}
		} catch (NoSuchMethodException e) {
			throw new ReflectionException(e);
		} catch (IllegalArgumentException e) {
			throw new ReflectionException(e);
		} catch (IllegalAccessException e) {
			throw new ReflectionException(e);
		} catch (InvocationTargetException e) {
			throw new ReflectionException(e);
		} catch (NoSuchFieldException e) {
			throw new ReflectionException(e);
		}
	}

	/**
	 * @param qName
	 * @return
	 */
	public boolean isStringField(Object o, String q) {
		Field[] fs;
		
		try {
			fs = o.getClass().getDeclaredFields();
			for(Field f : fs) {
				if(q.equals(f.getName()) &&
						String.class.isAssignableFrom(f.getType())) {
					return true;
				}
			}
			return false;
		} catch (IllegalArgumentException e) {
			throw new ReflectionException(e);
		}
	}
	
	//
	private boolean isBoundString(Class<?> cl) {
		Proposition<ClassProposition> cp;
		
		cp = ClassProposition.superClass(CollectionTypes.getType(cl));
		return (Collection.class.isAssignableFrom(cl) &&
				cp.is1(String.class));
	}
	
	/**
	 * @param qName
	 * @return
	 */
	public boolean isStringListField(Object o, String q) {
		Field[] fs;
		
		try {
			fs = o.getClass().getDeclaredFields();
			for(Field f : fs) {
				Class<?> cl = f.getType();
				
				if(_E.isPlural(q, f.getName()) &&
						isBoundString(cl)) {
					return true;
				}
			}
			return false;
		} catch (IllegalArgumentException e) {
			throw new ReflectionException(e);
		}
	}

	/**
	 * @param p
	 * @param q
	 * @return
	 */
	public boolean isStringListFieldExtra(Object o, String q) {
		Field[] fs;
		
		try {
			fs = o.getClass().getDeclaredFields();
			for(Field f : fs) {
				Class<?> cl = f.getType();
				
				if(q.equals(f.getName()) &&
						isBoundString(cl)) {
					return true;
				}
			}
			return false;
		} catch (IllegalArgumentException e) {
			throw new ReflectionException(e);
		}
	}
	
}
