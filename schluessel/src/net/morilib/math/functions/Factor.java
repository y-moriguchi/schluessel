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

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import net.morilib.lang.algebra.Dividable;
import net.morilib.lang.algebra.FieldElement;
import net.morilib.util.Maps;

public class Factor<F extends Comparable<F>>
implements Dividable<Factor<F>>, Comparable<Factor<F>>,
java.io.Serializable {

	//
	private static final long serialVersionUID = -4309016154545851641L;
	
	//
	private static final Maps.Transform2<Integer>
	ADD_INT2 = new Maps.Transform2<Integer>() {

		public Integer transform(Integer x, Integer y) {
			if(x == null) {
				return (y == null) ? null : y;
			} else {
				return (y == null) ? x : x + y;
			}
		}
		
	};
	
	//
	private static final Maps.Transform2<Integer>
	SUB_INT2 = new Maps.Transform2<Integer>() {

		public Integer transform(Integer x, Integer y) {
			if(x == null) {
				return (y == null) ? null : -y;
			} else {
				return (y == null) ? x : x - y;
			}
		}
		
	};
	
	/**
	 * 
	 */
	protected Map<F, Integer> orders = new TreeMap<F, Integer>();
	
	/**
	 * 
	 */
	public Factor() {
		// do nothing
	}
	
	/**
	 * 
	 * @param variable
	 */
	public Factor(F variable) {
		orders.put(variable, 1);
	}
	
	/**
	 * 
	 * @param factor
	 */
	public Factor(Factor<F> factor) {
		orders = new TreeMap<F, Integer>(factor.orders);
	}
	
	
	private void del0() {
		Iterator<Map.Entry<F, Integer>> i1;
		
		i1 = orders.entrySet().iterator();
		while(i1.hasNext()) {
			Map.Entry<F, Integer> e1 = i1.next();
			
			if(e1.getValue() == 0) {
				i1.remove();
			}
		}
	}
	
	
	public Factor<F> divide(Factor<F> x) {
		Factor<F> res = new Factor<F>();
		
		res.orders = Maps.map(SUB_INT2,
				new TreeMap<F, Integer>(), orders, x.orders);
		del0();
		return res;
	}

	
	public Factor<F> multiply(Factor<F> x) {
		Factor<F> res = new Factor<F>();
		
		res.orders = Maps.map(ADD_INT2,
				new TreeMap<F, Integer>(), orders, x.orders);
		del0();
		return res;
	}

	
	public Factor<F> power(int n) {
		Factor<F> res = new Factor<F>();
		final int y = n;
		
		res.orders = Maps.map(new Maps.Transform<Integer>() {
			
			public Integer transform(Integer x) {
				return x * y;
			}
			
		}, new TreeMap<F, Integer>(), orders);
		return res;
	}


	public int compareTo(Factor<F> o) {
		Iterator<Map.Entry<F, Integer>> i1, i2;
		
		i1 = orders.entrySet().iterator();
		i2 = o.orders.entrySet().iterator();
		
		while(i1.hasNext() && i2.hasNext()) {
			Map.Entry<F, Integer> e1 = i1.next();
			Map.Entry<F, Integer> e2 = i2.next();
			int cr;
			
			cr = e1.getKey().compareTo(e2.getKey());
			if(cr != 0) {
				return cr;
			}
			cr = e1.getValue().compareTo(e2.getValue());
			if(cr != 0) {
				return cr;
			}
		}
		return i1.hasNext() ? 1 : (i2.hasNext() ? -1 : 0);
	}
	
	
	public boolean containsVariable(F var) {
		return orders.containsKey(var);
	}
	
	
	public int getOrder(F var) {
		Integer res = orders.get(var);
		
		if(res == null) {
			throw new IllegalArgumentException();
		}
		return res;
	}
	
	
	public Factor<F> removeVariable(F var) {
		Factor<F> res = new Factor<F>();
		
		for(Map.Entry<F, Integer> m : orders.entrySet()) {
			if(!m.getKey().equals(var)) {
				res.orders.put(m.getKey(), m.getValue());
			}
		}
		return res;
	}
	
	
	public Factor<F> removeVariables(Set<F> vars) {
		Factor<F> res = new Factor<F>();
		
		for(Map.Entry<F, Integer> m : orders.entrySet()) {
			if(!vars.contains(m.getKey())) {
				res.orders.put(m.getKey(), m.getValue());
			}
		}
		return res;
	}
	
	
	public boolean isConstant() {
		return orders.isEmpty();
	}
	
	
	public boolean isPolynomial() {
		for(Integer i : orders.values()) {
			if(i < 0) {
				return false;
			}
		}
		return true;
	}
	
	
	public boolean isLinear() {
		if(orders.size() == 1) {
			Map.Entry<F, Integer> e =
				orders.entrySet().iterator().next();
			
			return e.getValue() == 1;
		}
		return isConstant();
	}
	
	
	public <G extends Comparable<G>>
	Factor<G> replaceVariables(Map<F, G> map) {
		Factor<G> res = new Factor<G>();
		Map<G, Integer> o2 = new TreeMap<G, Integer>();
		
		for(Map.Entry<F, Integer> e : orders.entrySet()) {
			G g = map.get(e.getKey());
			int val = e.getValue();
			
			if(g == null) {
				return null;
			} else if(o2.containsKey(g)) {
				val = o2.get(g) + val;
			}
			o2.put(g, val);
		}
		res.orders = o2;
		return res;
	}
	
	
	public void collectVariables(Set<F> res) {
		res.addAll(orders.keySet());
	}
	
	
	public Factor<F> differenciate(F var) {
		Factor<F> res = new Factor<F>();
		
		if(containsVariable(var)) {
			for(Map.Entry<F, Integer> m : orders.entrySet()) {
				if(m.getKey().equals(var)) {
					int n = m.getValue();
					
					if(n > 0) {
						res.orders.put(m.getKey(), n - 1);
					}
				} else {
					res.orders.put(m.getKey(), m.getValue());
				}
			}
		}
		return res;
	}
	
	
	public<C extends FieldElement<C>> C replaceAll(Map<F, C> values) {
		C res = null;
		
		if(!values.keySet().containsAll(orders.keySet())) {
			return null;
		}
		
		for(Map.Entry<F, C> m : values.entrySet()) {
			C r1 = m.getValue().power(orders.get(m.getKey()));
			
			res = (res == null) ? r1 : res.multiply(r1);
		}
		return res;
	}
	
	/**
	 * 
	 * @param var
	 * @return
	 */
	public boolean isSingleVariable() {
		return orders.size() <= 1;
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof Factor<?>) {
			return orders.equals(((Factor<?>)o).orders);
		}
		return false;
	}
	
	
	public int hashCode() {
		return orders.hashCode();
	}
	
	
	public String toString() {
		StringBuilder buf = new StringBuilder();
		String a = "";
		
		for(Map.Entry<F, Integer> m : orders.entrySet()) {
			String k = m.getKey().toString();
			int    v = m.getValue();
			
			if(k.length() == 1) {
				buf.append(k);
			} else {
				buf.append(a).append(k);
			}
			
			if(v != 1) {
				buf.append("^").append(v);
			}
			a = "*";
		}
		return buf.toString();
	}
	
}
