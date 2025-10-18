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
package net.morilib.math.functions;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import net.morilib.lang.Equivalence;
import net.morilib.lang.algebra.FieldElement;
import net.morilib.lang.algebra.UnitaryRingElement;
import net.morilib.lang.monad.BooleanMonad;
import net.morilib.util.Maps;
import net.morilib.util.MethodLocal;
import net.morilib.util.set.quotient.Classifier;

public class Polynomial
<F extends Comparable<F>, C extends FieldElement<C>>
implements UnitaryRingElement<Polynomial<F, C>>, Serializable {

	//
	private static final long serialVersionUID = 7611588386752523588L;
	
	private final Maps.Transform2<C> _add = new Maps.Transform2<C>() {
		
		//
//		private static final long serialVersionUID = -1554655061800510876L;

		public C transform(C x, C y) {
			if(x == null) {
				return (y == null) ? null : y;
			} else {
				return (y == null) ? x : x.add(y);
			}
		}
		
	};
	
	private final Maps.Transform2<C> _sub = new Maps.Transform2<C>() {
		
		//
//		private static final long serialVersionUID = -1554655061800510876L;

		public C transform(C x, C y) {
			if(x == null) {
				return (y == null) ? null : y.negate();
			} else {
				return (y == null) ? x : x.subtract(y);
			}
		}
		
	};
	
	//
	private Map<Factor<F>, C> coefficients =
		new TreeMap<Factor<F>, C>();
	
	public Polynomial(C coefficient, Factor<F> factor) {
		if(!factor.isPolynomial()) {
			throw new IllegalArgumentException();
		}
		coefficients.put(factor, coefficient);
	}
	
	
	public Polynomial(C coefficient, F var) {
		coefficients.put(new Factor<F>(var), coefficient);
	}
	
	
	public Polynomial(C coefficient) {
		if(!coefficient.isZero()) {
			coefficients.put(new Factor<F>(), coefficient);
		}
	}
	
	
	public Polynomial() {
		// do nothing
	}
	
	
	private void del0() {
		Iterator<Map.Entry<Factor<F>, C>> i1;
		
		i1 = coefficients.entrySet().iterator();
		while(i1.hasNext()) {
			Map.Entry<Factor<F>, C> e1 = i1.next();
			
			if(e1.getValue().isZero()) {
				i1.remove();
			}
		}
	}
	
	
	public boolean isUnit() {
		if(coefficients.size() == 1) {
			Map.Entry<Factor<F>, C> e =
				coefficients.entrySet().iterator().next();
			
			return e.getKey().isConstant() && e.getValue().isUnit();
		}
		return false;
	}

	
	public boolean isZero() {
		return coefficients.isEmpty();
	}
	
	
	public Polynomial<F, C> negate() {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
		res.coefficients = Maps.map(
				new Maps.Transform<C>() {

					public C transform(C o) {
						return o.negate();
					}
					
				},
				new TreeMap<Factor<F>, C>(),
				coefficients);
		return res;
	}

	
	public Polynomial<F, C> subtract(Polynomial<F, C> x) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
		res.coefficients = Maps.map(
				_sub,
				new TreeMap<Factor<F>, C>(),
				coefficients, x.coefficients);
		res.del0();
		return res;
	}
	
	
	public Polynomial<F, C> add(Polynomial<F, C> x) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
		res.coefficients = Maps.map(
				_add,
				new TreeMap<Factor<F>, C>(),
				coefficients, x.coefficients);
		res.del0();
		return res;
	}
	
	
	private void _add(C c, Factor<F> x) {
		C d = coefficients.get(x);
		
		if(d != null) {
			d = c.add(d);
			if(!d.isZero()) {
				coefficients.put(x, d);
			} else {
				coefficients.remove(x);
			}
		} else {
			coefficients.put(x, c);
		}
	}
	
	
	public Polynomial<F, C> multiply(int n) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		final int y = n;
		
		res.coefficients = Maps.map(new Maps.Transform<C>() {
			
			public C transform(C x) {
				return x.multiply(y);
			}
			
		}, new TreeMap<Factor<F>, C>(), coefficients);
		res.del0();
		return res;
	}
	
	
	public Polynomial<F, C> multiply(C n) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		final C y = n;
		
//		System.out.println("coe:" + n);
		res.coefficients = Maps.map(new Maps.Transform<C>() {
			
			public C transform(C x) {
				return x.multiply(y);
			}
			
		}, new TreeMap<Factor<F>, C>(), coefficients);
		res.del0();
		return res;
	}
	
	
	public Polynomial<F, C> multiply(C c, Factor<F> x) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
//		System.out.println("coe:" + c);
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			C d = e.getValue().multiply(c);
			
			if(!d.isZero()) {
				res.coefficients.put(e.getKey().multiply(x), d);
			}
		}
		res.del0();
		return res;
	}
	
	
	public Polynomial<F, C> multiply(Polynomial<F, C> x) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
		for(Map.Entry<Factor<F>, C> e : x.coefficients.entrySet()) {
			res = res.add(multiply(e.getValue(), e.getKey()));
		}
		return res;
	}
	
	
	public Polynomial<F, C> divide(C n) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		final C y = n;
		
		res.coefficients = Maps.map(new Maps.Transform<C>() {
			
			public C transform(C x) {
				return x.divide(y);
			}
			
		}, new TreeMap<Factor<F>, C>(), coefficients);
		res.del0();
		return res;
	}
	
	
	public Polynomial<F, C> power(int n) {
		if(n < 0) {
			throw new IllegalArgumentException();
		} else if(n == 0) {
			return new Polynomial<F, C>();
		} else {
			Polynomial<F, C> res = this;
			
			for(int i = 1; i < n; i++) {
				res = res.multiply(res);
			}
			return res;
		}
	}
	
	
	public boolean isConstant() {
		if(coefficients.isEmpty()) {
			return true;
		} else if(coefficients.size() == 1) {
			Map.Entry<Factor<F>, C> e =
				coefficients.entrySet().iterator().next();
			
			return e.getKey().isConstant();
		}
		return false;
	}
	
	
	public boolean isLinear() {
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			if(!e.getKey().isLinear()) {
				return false;
			}
		}
		return true;
	}
	
	
	public boolean isLinearOf(F var) {
		int res = 0;
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			if(e.getKey().containsVariable(var)) {
				if(!e.getKey().isLinear()) {
					return false;
				} else if(e.getKey().getOrder(var) != 1) {
					return false;
				} else if(++res > 1) {
					return false;
				}
				
			}
		}
		return (res > 0);
	}
	
	
	public Polynomial<F, C> substitute(
			F var, Polynomial<F, C> val) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			if(e.getKey().containsVariable(var)) {
				int o = e.getKey().getOrder(var);
				
				res = res.add(val.power(o).multiply(
						e.getValue(), e.getKey().removeVariable(var)));
			} else {
				res._add(e.getValue(), e.getKey());
			}
		}
		return res;
	}
	
	
	public Polynomial<F, C> solveLinear(F var) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		C coeff = null;
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			if(e.getKey().containsVariable(var)) {
				if(!e.getKey().isLinear()) {
					return null;
				}
				coeff = e.getValue();
			} else {
				res.coefficients.put(e.getKey(), e.getValue());
			}
		}
		
		if(coeff == null) {
			return null;
		}
		return res.divide(coeff).negate();
	}
	
	
	public Polynomial<F, C> substituteVariables(Map<F, F> map) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			Factor<F> g = e.getKey().replaceVariables(map);
			
			if(g != null) {
				res._add(e.getValue(), g);
			} else {
				res._add(e.getValue(), e.getKey());
			}
		}
		return res;
	}
	
	
	public void collectVariables(Set<F> res) {
		for(Factor<F> f : coefficients.keySet()) {
			f.collectVariables(res);
		}
	}
	
	
	public C getConstant() {
		C c = coefficients.get(new Factor<F>());
		
		return c;
	}
	
	
	public Polynomial<F, C> differenciate(F val) {
		Polynomial<F, C> res = new Polynomial<F, C>();
		Map<Factor<F>, C>   rmp = new TreeMap<Factor<F>, C>();
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			if(e.getKey().containsVariable(val)) {
				int n = e.getKey().getOrder(val);
				
				rmp.put(e.getKey().differenciate(val),
						e.getValue().multiply(n));
			}
		}
		res.coefficients = rmp;
		res.del0();
		return res;
	}
	
	
	public C substituteAll(Map<F, C> values) {
		Set<F> vals = new HashSet<F>();
		C res = null;
		
		collectVariables(vals);
		if(!values.keySet().containsAll(vals)) {
			return null;
		}
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			C r1 = e.getKey().replaceAll(values).multiply(e.getValue());
			
			res = (res == null) ? r1 : res.add(r1);
		}
		return res;
	}

	/**
	 * @param var
	 * @return
	 */
	public boolean containsVariable(F var) {
		for(Factor<F> f : coefficients.keySet()) {
			if(f.containsVariable(var)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @param polynomial
	 * @param v
	 * @return
	 */
	public boolean equalsWithoutVariable(
			Polynomial<F, C> polynomial,
			Set<F> v) {
		final Set<F> var = v;
		final MethodLocal<BooleanMonad> mnd = 
			new MethodLocal<BooleanMonad>(BooleanMonad.TRUE);
		final Equivalence<Factor<F>, Factor<F>>
		eqv = new Equivalence<Factor<F>, Factor<F>>() {

			public boolean isEquivalent(Factor<F> a, Factor<F> b) {
				return a.removeVariables(var).equals(
						b.removeVariables(var));
			}

			public Factor<F> classify(Factor<F> o) {
				return o.removeVariables(var);
			}
			
		};
		
		if(coefficients.size() != polynomial.coefficients.size()) {
			return false;
		}
		
		Maps.each(
				new Maps.Each2<Set<C>>() {

					public void each(Set<C> a, Set<C> b) {
						if(a == null || b != null || !a.equals(b)) {
							mnd.set(mnd.get().bind(BooleanMonad.RESET));
							// $break
						}
					}
					
				},
				Classifier.classify(coefficients, eqv),
				Classifier.classify(polynomial.coefficients, eqv));
		return mnd.get().getValue();
	}
	
	/**
	 * 
	 * @return
	 */
	public F getSingleVariable() {
		F var = null;
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			Factor<F> f = e.getKey();
			
			if(!f.isSingleVariable()) {
				return null;
			} else if(var == null) {
				for(F g : f.orders.keySet()) {
					var = g;
				}
			} else {
				if(!f.isConstant() && !f.containsVariable(var)) {
					return null;
				}
			}
		}
		return var;
	}
	
	/**
	 * 
	 * @return
	 */
	public Polynomial1Coefficients<C> toPolynomial1Coefficients() {
		F var = null;
		SortedMap<Integer, C> s = new TreeMap<Integer, C>();
		
		for(Map.Entry<Factor<F>, C> e : coefficients.entrySet()) {
			Factor<F> f = e.getKey();
			
			if(!f.isSingleVariable()) {
				return null;
			} else if(var == null) {
				for(F g : f.orders.keySet()) {
					var = g;
				}
			} else {
				if(!f.isConstant() && !f.containsVariable(var)) {
					return null;
				}
			}
			s.put(f.isConstant() ? 0 : f.getOrder(var), e.getValue());
		}
		return new SortedMapPolynomial1Coefficients<C>(s);
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof Polynomial<?, ?>) {
			return coefficients.equals(
					((Polynomial<?, ?>)o).coefficients);
		}
		return false;
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return coefficients.hashCode();
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder buf = new StringBuilder();
		String a = "";
		
		for(Map.Entry<Factor<F>, C> m : coefficients.entrySet()) {
			C      v = m.getValue();
			
			buf.append(a);
			if(!v.isUnit() || m.getKey().isConstant()) {
				buf.append(v);
			}
			
			if(!m.getKey().isConstant()) {
				buf.append(m.getKey());
			}
			a = " + ";
		}
		return buf.toString();
	}
	
}
