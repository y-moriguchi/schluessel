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

import net.morilib.lang.proposition.Logical;
import net.morilib.lang.proposition.OneVariableProposition;
import net.morilib.lang.proposition.Proposition;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/13
 */
public abstract class ClassProposition
extends OneVariableProposition<ClassProposition> {
	
	//
	/*package*/ Class<?> classe;
	
	//
	/*package*/ ClassProposition() { }
	
	//
	/*package*/ static class _Only extends ClassProposition {
		
		/*package*/ _Only(Class<?> cls) {
			classe = cls;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
		 */
		@Override
		public boolean is1(Object var1) {
			Class<?> c;
			
			if(var1 instanceof Class) {
				c = (Class<?>)var1;
			} else if(var1 == null) {
				return false;
			} else {
				c = var1.getClass();
			}
			return c.equals(classe);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#implies(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean implies(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			}
			return p.isFalse() || isEqualTo(p);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isImplied(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			}
			return p.is1(classe);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isIndependent(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			}
			return !p.is1(classe);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isEqualTo(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p instanceof _Only) {
				return classe.equals(((_Only)p).classe);
			}
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isFalse()
		 */
		@Override
		public boolean isFalse() {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isTrue()
		 */
		@Override
		public boolean isTrue() {
			return false;
		}
		
	}
	
	//
	/*package*/ static class _Sub extends ClassProposition {
		
		/*package*/ _Sub(Class<?> cls) {
			classe = cls;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
		 */
		@Override
		public boolean is1(Object var1) {
			Class<?> c;
			
			if(var1 instanceof Class) {
				c = (Class<?>)var1;
			} else if(var1 == null) {
				return false;
			} else {
				c = var1.getClass();
			}
			return classe.isAssignableFrom(c);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#implies(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean implies(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return false;
			} else if(p.isFalse()) {
				return true;
			} else if(p instanceof _Only || p instanceof _Sub) {
				return classe.isAssignableFrom(
						((ClassProposition)p).classe);
			} else if(p instanceof _Sup) {
				return false;
			}
			return p.isImplied(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isImplied(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return true;
			} else if(p.isFalse()) {
				return false;
			} else if(p instanceof _Sub) {
				return ((ClassProposition)p).classe.isAssignableFrom(
						classe);
			} else if(p instanceof _Only || p instanceof _Sup) {
				return false;
			}
			return p.implies(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isIndependent(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return false;
			} else if(p.isFalse()) {
				return true;
			} else if(p instanceof _Only || p instanceof _Sup) {
				return !classe.isAssignableFrom(
						((ClassProposition)p).classe);
			} else if(p instanceof _Sub) {
				Class<?> c = ((ClassProposition)p).classe;
				
				return (!classe.isAssignableFrom(c) &&
						!c.isAssignableFrom(classe));
			}
			return p.isIndependent(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isEqualTo(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p instanceof _Sub) {
				return classe.equals(((_Sub)p).classe);
			}
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isFalse()
		 */
		@Override
		public boolean isFalse() {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isTrue()
		 */
		@Override
		public boolean isTrue() {
			return false;
		}
		
	}
	
	//
	/*package*/ static class _Sup extends ClassProposition {
		
		/*package*/ _Sup(Class<?> cls) {
			classe = cls;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
		 */
		@Override
		public boolean is1(Object var1) {
			Class<?> c;
			
			if(var1 instanceof Class) {
				c = (Class<?>)var1;
			} else if(var1 == null) {
				return false;
			} else {
				c = var1.getClass();
			}
			return c.isAssignableFrom(classe);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#implies(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean implies(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return false;
			} else if(p.isFalse()) {
				return true;
			} else if(p instanceof _Only || p instanceof _Sup) {
				return ((ClassProposition)p).classe.isAssignableFrom(
						classe);
			} else if(p instanceof _Sub) {
				return false;
			}
			return p.isImplied(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isImplied(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return true;
			} else if(p.isFalse()) {
				return false;
			} else if(p instanceof _Sup) {
				return classe.isAssignableFrom(
						((ClassProposition)p).classe);
			} else if(p instanceof _Only || p instanceof _Sub) {
				return false;
			}
			return p.implies(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isIndependent(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return false;
			} else if(p.isFalse()) {
				return true;
			} else if(p instanceof _Only || p instanceof _Sub) {
				return !((ClassProposition)p).classe.isAssignableFrom(
						classe);
			} else if(p instanceof _Sup) {
				Class<?> c = ((ClassProposition)p).classe;
				
				return (!classe.isAssignableFrom(c) &&
						!c.isAssignableFrom(classe));
			}
			return p.isIndependent(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
		 */
		@Override
		public boolean isEqualTo(Proposition<ClassProposition> p) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p instanceof _Sup) {
				return classe.equals(((_Sup)p).classe);
			}
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isFalse()
		 */
		@Override
		public boolean isFalse() {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lang.proposition.Proposition#isTrue()
		 */
		@Override
		public boolean isTrue() {
			return false;
		}
		
	}
	
	/**
	 * 
	 */
	public static final Proposition<ClassProposition>
	ALL_CLASSES = Logical.getTrue();
	
	/**
	 * 
	 */
	public static final Proposition<ClassProposition>
	NO_CLASSES = Logical.getFalse();
	
	/**
	 * 
	 * @param c
	 * @return
	 */
	public static Proposition<ClassProposition> equalClass(
			Class<?> c) {
		return (c == null) ? NO_CLASSES : new _Only(c);
	}
	
	/**
	 * 
	 * @param c
	 * @return
	 */
	public static Proposition<ClassProposition> subClass(
			Class<?> c) {
		return (c == null) ? NO_CLASSES : new _Sub(c);
	}
	
	/**
	 * 
	 * @param c
	 * @return
	 */
	public static Proposition<ClassProposition> superClass(
			Class<?> c) {
		return (c == null) ? NO_CLASSES : new _Sup(c);
	}
	
	/**
	 * 
	 * @param c
	 * @return
	 */
	public static Proposition<ClassProposition> equalClass(
			Object c) {
		return (c == null) ? NO_CLASSES : new _Only(c.getClass());
	}
	
	/**
	 * 
	 * @param c
	 * @return
	 */
	public static Proposition<ClassProposition> subClass(
			Object c) {
		return (c == null) ? NO_CLASSES : new _Sub(c.getClass());
	}
	
	/**
	 * 
	 * @param c
	 * @return
	 */
	public static Proposition<ClassProposition> superClass(
			Object c) {
		return (c == null) ? NO_CLASSES : new _Sup(c.getClass());
	}
	
}
