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
package net.morilib.lang.cont;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public final class CPS {
	
	
	public static final class ByName {
		
		//
		private Object   program;
		private String   name;
		private Object[] args;
		
		
		public ByName(Object program, String name, Object... args) {
			if(program == null) {
				throw new NullPointerException();
			} else if(name == null) {
				throw new NullPointerException();
			} else if(args == null) {
				throw new NullPointerException();
			}
			this.program = program;
			this.name    = name;
			this.args    = args;
		}
		
	}
	
	//
	private Object   program;
	private Object[] args;
	
	
	public CPS(Object program, Object... args) {
		if(program == null) {
			throw new NullPointerException();
		} else if(args == null) {
			throw new NullPointerException();
		}
		this.program = program;
		this.args    = args;
	}
	
	private static boolean assignable(Class<?>[] cs, Object... args) {
		for(int i = 0; i < args.length; i++) {
			if(args[i] == null) {
				// do nothing
			} else if(cs[i].equals(Boolean.TYPE)) {
				if(!(args[i] instanceof Boolean)) {
					return false;
				}
			} else if(cs[i].equals(Byte.TYPE)) {
				if(!(args[i] instanceof Byte)) {
					return false;
				}
			} else if(cs[i].equals(Character.TYPE)) {
				if(!(args[i] instanceof Character)) {
					return false;
				}
			} else if(cs[i].equals(Short.TYPE)) {
				if(!(args[i] instanceof Short)) {
					return false;
				}
			} else if(cs[i].equals(Integer.TYPE)) {
				if(!(args[i] instanceof Integer)) {
					return false;
				}
			} else if(cs[i].equals(Long.TYPE)) {
				if(!(args[i] instanceof Long)) {
					return false;
				}
			} else if(cs[i].equals(Float.TYPE)) {
				if(!(args[i] instanceof Float)) {
					return false;
				}
			} else if(cs[i].equals(Double.TYPE)) {
				if(!(args[i] instanceof Double)) {
					return false;
				}
			} else if(!cs[i].isInstance(args[i])) {
				return false;
			}
		}
		return true;
	}
	
	private static Method getmth1(
			Method[] ms, Object cont, Object... cargs) {
		for(Method m : ms) {
			if(m.getAnnotation(Continuatable.class) != null) {
				Class<?>[] cs = m.getParameterTypes();
				
				if(assignable(cs, cargs)) {
					return m;
				}
			}
		}
		return null;
	}
	
	private static Method getmthByName1(
			Method[] ms, Object cont, String name, Object... cargs) {
		for(Method m : ms) {
			if(m.getName().equals(name)) {
				Class<?>[] cs = m.getParameterTypes();
				
				if(assignable(cs, cargs)) {
					return m;
				}
			}
		}
		return null;
	}
	
	private static Method getmth(Object cont, Object... cargs) {
		Method[] ms;
		Method   res;
		
		ms = cont.getClass().getDeclaredMethods();
		if((res = getmth1(ms, cont, cargs)) != null) {
			return res;
		}
		ms = cont.getClass().getMethods();
		if((res = getmth1(ms, cont, cargs)) != null) {
			return res;
		}
		throw new CPSInvocationException();
	}
	
	private static Method getmthByName(
			Object cont, String name, Object... cargs) {
		Method[] ms;
		Method   res;
		
		ms = cont.getClass().getDeclaredMethods();
		if((res = getmthByName1(ms, cont, name, cargs)) != null) {
			return res;
		}
		ms = cont.getClass().getMethods();
		if((res = getmthByName1(ms, cont, name, cargs)) != null) {
			return res;
		}
		throw new CPSInvocationException();
	}
	
	
	private static Object _invoke(
			Object cont, String name, boolean changeAccess,
			Object... cargs) {
		Object   c = cont;
		String   n = name;
		Object[] a = cargs;
		try {
			while(true) {
				Method m0;
				Object re;
				
				m0 = (n == null) ? getmth(c, a) : getmthByName(c, n, a);
				if(changeAccess) {
					m0.setAccessible(true);
				}
				
				re = m0.invoke(c, a);
				if(re instanceof CPS) {
					c = ((CPS)re).program;
					a = ((CPS)re).args;
					n = null;
				} else if(re instanceof ByName) {
					c = ((ByName)re).program;
					a = ((ByName)re).args;
					n = ((ByName)re).name;
				} else {
					return re;
				}
			}
		} catch (IllegalArgumentException e) {
			throw new CPSInvocationException(e);
		} catch (IllegalAccessException e) {
			throw new CPSInvocationException(e);
		} catch (InvocationTargetException e) {
			throw new CPSInvocationException(e);
		}
	}
	
	
	public static Object invoke(
			Object cont, boolean changeAccess, Object... cargs) {
		return _invoke(cont, null, changeAccess, cargs);
	}
	
	
	public static Object invokeByName(
			Object cont, String name, boolean changeAccess,
			Object... cargs) {
		if(name == null) {
			throw new NullPointerException();
		}
		return _invoke(cont, name, changeAccess, cargs);
	}
	
	
	public static Object invokeChange(Object cont, Object... cargs) {
		return _invoke(cont, null, true, cargs);
	}
	
	
	public static Object invokeByName(
			Object cont, String name, Object... cargs) {
		if(name == null) {
			throw new NullPointerException();
		}
		return _invoke(cont, name, true, cargs);
	}
	
	
	private static Object _call(
			Object cont, String name, boolean changeAccess,
			Object... cargs) {
		Object   c = cont;
		String   n = name;
		Object[] a = cargs;
		try {
			while(true) {
				Method m0;
				
				m0 = (n == null) ? getmth(c, a) : getmthByName(c, n, a);
				if(changeAccess) {
					m0.setAccessible(true);
				}
				return m0.invoke(c, a);
			}
		} catch (IllegalArgumentException e) {
			throw new CPSInvocationException(e);
		} catch (IllegalAccessException e) {
			throw new CPSInvocationException(e);
		} catch (InvocationTargetException e) {
			throw new CPSInvocationException(e);
		}
	}
	
	
	public static Object call(
			Object cont, boolean changeAccess, Object... cargs) {
		return _call(cont, null, changeAccess, cargs);
	}
	
	
	public static Object callByName(
			Object cont, String name, boolean changeAccess,
			Object... cargs) {
		if(name == null) {
			throw new NullPointerException();
		}
		return _call(cont, name, changeAccess, cargs);
	}
	
	
	public static Object call(Object cont, Object... cargs) {
		return _call(cont, null, true, cargs);
	}
	
	
	public static Object callByName(
			Object cont, String name, Object... cargs) {
		if(name == null) {
			throw new NullPointerException();
		}
		return _call(cont, name, true, cargs);
	}
	
}
