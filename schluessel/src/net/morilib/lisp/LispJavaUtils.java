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

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import net.morilib.lisp.util.ReflContainer;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/12
 */
public final class LispJavaUtils {

	//
	static final String[] JAVAPATH = {
			"net.morilib.lisp.swing"
	};

	//
	private LispJavaUtils() {}

	private static Class<?> getCompornentType(Class<?> cl) {
		if(cl.isArray()) {
			return cl.getComponentType();
		} else if(cl.isAssignableFrom(List.class)) {
			/*TypeVariable<? extends Class<?>>[] tv;

			tv = cl.getTypeParameters();
			if(tv.length == 0) {    // raw type
				return Object.class;
			} else if(tv.length == 1) {
				return tv[0].getClass();
			} else {
				return null;
			}*/
			return null;
		} else {
			return null;
		}
	}

	/*package*/ static boolean isAssignableInt(Class<?> rtp) {
		return (Integer.TYPE.isAssignableFrom(rtp) ||
				Integer.class.isAssignableFrom(rtp));
	}

	/*package*/ static boolean isAssignableLong(Class<?> rtp) {
		return (Long.TYPE.isAssignableFrom(rtp) ||
				Long.class.isAssignableFrom(rtp));
	}

	/*package*/ static boolean isAssignableFloat(Class<?> rtp) {
		return (Float.TYPE.isAssignableFrom(rtp) ||
				Float.class.isAssignableFrom(rtp));
	}

	/*package*/ static boolean isAssignableDouble(Class<?> rtp) {
		return (Double.TYPE.isAssignableFrom(rtp) ||
				Double.class.isAssignableFrom(rtp));
	}

	/*package*/ static boolean isAssignableBigInteger(Class<?> rtp) {
		return (BigInteger.class.isAssignableFrom(rtp));
	}

	/*package*/ static boolean isAssignableBigDecimal(Class<?> rtp) {
		return (BigDecimal.class.isAssignableFrom(rtp));
	}

	/*package*/ static boolean isSameClass(Class<?> cl, Datum ld) {
		if(cl.equals(Object.class)) {
			return true;
		} else if(ld instanceof LispString) {
			return cl.isAssignableFrom(String.class);
		} else if(ld instanceof LispBoolean) {
			return (cl.isAssignableFrom(Boolean.TYPE) ||
					cl.isAssignableFrom(Boolean.class));
		} else if(ld instanceof LispCharacter) {
			return (cl.isAssignableFrom(Character.TYPE) ||
					cl.isAssignableFrom(Character.class));
		} else if(ld instanceof LispInteger) {
			return (isAssignableInt(cl)        ||
					isAssignableLong(cl)       ||
					isAssignableFloat(cl)      ||
					isAssignableDouble(cl)     ||
					isAssignableBigInteger(cl) ||
					isAssignableBigDecimal(cl));
		} else if(ld instanceof LispReal) {
			return (isAssignableInt(cl)        ||
					isAssignableLong(cl)       ||
					isAssignableFloat(cl)      ||
					isAssignableDouble(cl)     ||
					isAssignableBigInteger(cl) ||
					isAssignableBigDecimal(cl));
		} else if(ld instanceof Cons) {
			Datum p = ld;
			Class<?> cc = getCompornentType(cl);
	
			if(cc == null) {
				return false;
			}
	
			while(p != Nil.NIL) {
				if(p instanceof Cons) {
					Cons c = (Cons)p;
	
					if(!isSameClass(cc, c.getCar())) {
						return false;
					}
					p = c.getCdr();
				} else {
					return false;
				}
			}
			return true;
		} else if(ld instanceof LispVector) {
			Class<?> cc = getCompornentType(cl);
	
			if(cc == null) {
				return false;
			}
	
			for(Datum o : ((LispVector)ld).getList()) {
				if(!isSameClass(cc, o)) {
					return false;
				}
			}
			return true;
		} else if(ld instanceof JavaInstance) {
			return cl.isAssignableFrom(
					((JavaInstance)ld).getJavaInstance().getClass());
		} else if(ld == JavaNull.JAVA_NULL) {
			return true;
		} else if(ld == Nil.NIL) {
			return true;
		} else {
			return false;
		}
	}

	/*package*/ static boolean isSameClasses(
			Class<?>[] cl, List<Datum> ld, boolean varargs) {
		if(!varargs && cl.length != ld.size()) {
			return false;
		} else if(cl.length - 1 > ld.size()) {
			return false;
		}
	
		for(int i = 0; i < ld.size(); i++) {
			if(varargs && i >= cl.length - 1) {
				Class<?> lcl = cl[cl.length - 1];
	
				if(!lcl.isArray()) {
					return false;
				} else if(!isSameClass(
						lcl.getComponentType(), ld.get(i))) {
					return false;
				}
			} else if(!isSameClass(cl[i], ld.get(i))) {
				return false;
			}
		}
		return true;
	}

	/*package*/ static Object toJavaInstance(Class<?> cl, Datum d) {
		if(cl.equals(Object.class)) {
			return d;
		} else if(d instanceof LispString) {
			return d.getString();
		} else if(d instanceof LispBoolean) {
			return d.isTrue();
		} else if(d instanceof LispCharacter) {
			return Character.valueOf(d.getCharacter());
		} else if(d instanceof LispInteger) {
			if(isAssignableInt(cl)) {
				return Integer.valueOf(d.getInt());
			} else if(isAssignableLong(cl)) {
				return Long.valueOf(d.getLong());
			} else if(isAssignableFloat(cl)) {
				return Float.valueOf((float)d.getRealDouble());
			} else if(isAssignableDouble(cl)) {
				return Double.valueOf(d.getRealDouble());
			} else if(isAssignableBigInteger(cl)) {
				return d.getBigInteger();
			} else if(isAssignableBigDecimal(cl)) {
				return d.getBigDecimal();
			} else {
				throw new ClassCastException(cl.getName());
			}
		} else if(d instanceof LispReal) {
			if(isAssignableInt(cl)) {
				return Integer.valueOf(d.getInt());
			} else if(isAssignableLong(cl)) {
				return Long.valueOf(d.getLong());
			} else if(isAssignableFloat(cl)) {
				return Float.valueOf((float)d.getRealDouble());
			} else if(isAssignableDouble(cl)) {
				return Double.valueOf(d.getRealDouble());
			} else if(isAssignableBigInteger(cl)) {
				return d.getBigInteger();
			} else if(isAssignableBigDecimal(cl)) {
				return d.getBigDecimal();
			} else {
				throw new ClassCastException(cl.getName());
			}
		} else if(d instanceof Cons) {
			Datum         p   = d;
			Class<?>      cc  = getCompornentType(cl);
			int           len = LispUtils.consLength(d);
			ReflContainer cnt;
	
			if(cc == null) {
				throw new ClassCastException(cl.getName());
			} else {
				cnt = new ReflContainer(cl, len);
			}
	
			for(int i = 0; p != Nil.NIL; i++) {
				if(p instanceof Cons) {
					Cons c = (Cons)p;
	
					cnt.set(i, toJavaInstance(cc, c.getCar()));
					p = c.getCdr();
				} else {
					throw new ClassCastException(cl.getName());
				}
			}
			return cnt.toObject();
		} else if(d instanceof LispVector) {
			Class<?>      cc = getCompornentType(cl);
			int           len = LispUtils.consLength(d);
			ReflContainer cnt;
	
			if(cc == null) {
				throw new ClassCastException(cl.getName());
			} else {
				cnt = new ReflContainer(cl, len);
			}
	
			int i = 0;
			for(Datum o : ((LispVector)d).getList()) {
				cnt.set(i, toJavaInstance(cc, o));
				i++;
			}
			return cnt.toObject();
		} else if(d instanceof JavaInstance) {
			return ((JavaInstance)d).getJavaInstance();
		} else if(d instanceof JavaObjective) {
			return ((JavaObjective)d).toObject();
		} else if(d == JavaNull.JAVA_NULL) {
			return null;
		} else if(d == Nil.NIL) {
			return new ReflContainer(cl, 0).toObject();
			//return Collections.emptyList();
		} else {
			throw new ClassCastException(cl.getName());
		}
	}

	/*package*/ static Object[] toJavaInstances(
			Class<?>[] cl, List<Datum> ld, boolean varargs) {
		Object[] res = new Object[cl.length];
		Object   rst;
	
		if(!varargs && cl.length != ld.size()) {
			throw new ClassCastException();
		} else if(varargs) {
			Class<?> lcl = cl[cl.length - 1];
	
			if(!lcl.isArray()) {
				throw new RuntimeException();
			}
			rst = Array.newInstance(
					lcl.getComponentType(),
					ld.size() - cl.length + 1);
			res[cl.length - 1] = rst;
		} else {
			rst = null;
		}
	
		for(int i = 0; i < ld.size(); i++) {
			Class<?> lcl = cl[cl.length - 1];
			Class<?> lcc = lcl.getComponentType();
	
			if(varargs && i >= cl.length - 1) {
				Array.set(rst, i - cl.length + 1,
						toJavaInstance(lcc, ld.get(i)));
			} else if(isSameClass(cl[i], ld.get(i))) {
				res[i] = toJavaInstance(cl[i], ld.get(i));
			}
		}
		return res;
	}

	/*package*/ static Object[] toJavaInstances(
			Class<?>[] cl, boolean varargs, Datum... ld) {
		Object[] res = new Object[cl.length];
		Object   rst;
	
		if(!varargs && cl.length != ld.length) {
			throw new ClassCastException();
		} else if(varargs) {
			Class<?> lcl = cl[cl.length - 1];
	
			if(!lcl.isArray()) {
				throw new RuntimeException();
			}
			rst = Array.newInstance(
					lcl.getComponentType(),
					ld.length - cl.length + 1);
			res[cl.length - 1] = rst;
		} else {
			rst = null;
		}
	
		for(int i = 0; i < ld.length; i++) {
			Class<?> lcl = cl[cl.length - 1];
			Class<?> lcc = lcl.getComponentType();
	
			if(varargs && i >= cl.length - 1) {
				Array.set(rst, i - cl.length + 1,
						toJavaInstance(lcc, ld[i]));
			} else if(isSameClass(cl[i], ld[i])) {
				res[i] = toJavaInstance(cl[i], ld[i]);
			}
		}
		return res;
	}

	/*package*/ static Object newInstance(
			Class<?> klass, List<Datum> lst) throws ParameterNotFoundException {
		Constructor<?>[] cns = klass.getConstructors();
	
		for(int i = 0; i < cns.length; i++) {
			Class<?>[] cls = cns[i].getParameterTypes();
			boolean    va = cns[i].isVarArgs();
	
			if(LispJavaUtils.isSameClasses(cls, lst, va)) {
				Object[] args =
					LispJavaUtils.toJavaInstances(cls, lst, va);
	
				try {
					return cns[i].newInstance(args);
				} catch (IllegalArgumentException e) {
					throw new JavaException(e);
				} catch (InstantiationException e) {
					throw new JavaException(e);
				} catch (IllegalAccessException e) {
					throw new JavaException(e);
				} catch (InvocationTargetException e) {
					throw new JavaTargetException(e.getCause());
				}
			}
		}
		throw new ParameterNotFoundException();
	}

	//
	private static Class<?> applyRule1(Class<?> cl) {
		String n = cl.getName();
	
		for(String p : JAVAPATH) {
			try {
				return Class.forName(
						p + n.replaceFirst("^(.*)\\.J", ".Lisp"));
			} catch (ClassNotFoundException e) {
				// go next
			}
		}
		return null;
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public static Datum newInstance(Object o) {
		Class<?> cl = (o == null) ? null : o.getClass();
		Class<?> iv;
	
		if(o == null || o == JavaNull.JAVA_NULL) {
			return Nil.NIL;
		} else if(o instanceof Datum) {
			return (Datum)o;
		} else if(o instanceof Integer) {
			return LispInteger.valueOf(((Integer)o).intValue());
		} else if(o instanceof Long) {
			return LispInteger.valueOf(((Long)o).longValue());
		} else if(o instanceof BigInteger) {
			return LispInteger.valueOf((BigInteger)o);
		} else if(o instanceof BigDecimal) {
			return LispUtils.bigDecimalToRational((BigDecimal)o);
		} else if(o instanceof Float) {
			return new LispDouble(((Float)o).doubleValue());
		} else if(o instanceof Double) {
			return new LispDouble(((Double)o).doubleValue());
		} else if(o instanceof String) {
			return new LispString((String)o);
		} else if(o instanceof Character) {
			return new LispCharacter(((Character)o).charValue());
		} else if(o instanceof Boolean) {
			return LispBoolean.getInstance(((Boolean)o).booleanValue());
		} else if(cl.isArray()) {
			ConsListBuilder bld = new ConsListBuilder();
			int len = Array.getLength(o);
	
			for(int i = 0; i < len; i++) {
				bld.append(newInstance(Array.get(o, i)));
			}
			return bld.get();
		}
	
		if((iv = applyRule1(cl)) != null) {
			try {
				return (Datum)iv.getConstructor(cl).newInstance(o);
			} catch (IllegalArgumentException e) {
				// go next
			} catch (InstantiationException e) {
				// go next
			} catch (IllegalAccessException e) {
				// go next
			} catch (InvocationTargetException e) {
				// go next
			} catch (NoSuchMethodException e) {
				// go next
			}
		}
		return new JavaInstance(o);
	}

	
}
