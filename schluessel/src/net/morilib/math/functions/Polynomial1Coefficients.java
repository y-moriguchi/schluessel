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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.morilib.lang.Hashes;
import net.morilib.lang.algebra.FieldElement;
import net.morilib.lang.algebra.QuotientAndRemainder;
import net.morilib.lang.algebra.Remainderable;
import net.morilib.lang.algebra.RingElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/01
 */
public abstract class Polynomial1Coefficients
<C extends FieldElement<C>>
implements RingElement<Polynomial1Coefficients<C>>,
Remainderable<Polynomial1Coefficients<C>> {

	/**
	 * 
	 * @param n
	 * @return
	 */
	protected abstract C getCoefficient(int n);
	
	/**
	 * 
	 * @param n
	 * @param v
	 * @return
	 */
	protected abstract void setCoefficient(int n, C v);
	
	/**
	 * 
	 * @return
	 */
	protected abstract Iterator<Integer> indexIterator();
	
	/**
	 * 
	 * @return
	 */
	public abstract int deg();
	
	/**
	 * 
	 * @return
	 */
	protected abstract Polynomial1Coefficients<C> newInstance(
			int size);
	
	/**
	 * 
	 * @return
	 */
	public abstract Polynomial1Coefficients<C> duplicate();
	
	/**
	 * 
	 * @return
	 */
	public boolean isUnit() {
		return (deg() == 0) && getCoefficient(0).isUnit();
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public Polynomial1Coefficients<C> divide(
			Polynomial1Coefficients<C> x) {
		return divideAndRemainder(x).getQuotient();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public Polynomial1Coefficients<C> negate() {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> res = duplicate();
		
		while(itr.hasNext()) {
			int i = itr.next();
			
			res.setCoefficient(i, getCoefficient(i).negate());
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public Polynomial1Coefficients<C> multiply(
			Polynomial1Coefficients<C> x) {
		Iterator<Integer> itr = x.indexIterator();
		Polynomial1Coefficients<C> res =
			newInstance(deg() + x.deg());
		
		while(itr.hasNext()) {
			Iterator<Integer> jtr = indexIterator();
			int i = itr.next();
			C ci = x.getCoefficient(i);
			
			while(jtr.hasNext()) {
				int j = jtr.next();
				C cj = getCoefficient(j);
				C cr = res.getCoefficient(j);
				
				if(cr == null) {
					cr = cj.multiply(ci);
				} else {
					cr = cr.add(cj.multiply(ci));
				}
				res.setCoefficient(i + j, cr);
			}
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public Polynomial1Coefficients<C> power(int n) {
		if(n == 0) {
			return newInstance(-1);
		} else if(n == 1) {
			return this;
		} else if(n < 0) {
			throw new IllegalArgumentException();
		}
		
		Polynomial1Coefficients<C> res = duplicate();
		for(int i = 2; i < n; i++) {
			res = res.multiply(res);
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public Polynomial1Coefficients<C> subtract(
			Polynomial1Coefficients<C> x) {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> res = x.duplicate();
		
		while(itr.hasNext()) {
			int i = itr.next();
			C c = x.getCoefficient(i);
			
			if(c != null) {
				res.setCoefficient(i, getCoefficient(i).subtract(c));
			}
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public Polynomial1Coefficients<C> add(
			Polynomial1Coefficients<C> x) {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> res = x.duplicate();
		
		while(itr.hasNext()) {
			int i = itr.next();
			C c = x.getCoefficient(i);
			
			if(c != null) {
				res.setCoefficient(i, getCoefficient(i).add(c));
			}
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public Polynomial1Coefficients<C> multiply(int n) {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> res = duplicate();
		
		while(itr.hasNext()) {
			int i = itr.next();
			
			res.setCoefficient(i, getCoefficient(i).multiply(n));
		}
		return res;
	}

	/**
	 * 
	 * @param c
	 * @return
	 */
	public Polynomial1Coefficients<C> multiply(C c) {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> res = duplicate();
		
		while(itr.hasNext()) {
			int i = itr.next();
			
			res.setCoefficient(i, getCoefficient(i).multiply(c));
		}
		return res;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Remainderable#remainder(java.lang.Object)
	 */
	public Polynomial1Coefficients<C> remainder(
			Polynomial1Coefficients<C> x) {
		return divideAndRemainder(x).getRemainder();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Remainderable#divideAndRemainder(java.lang.Object)
	 */
	public QuotientAndRemainder<Polynomial1Coefficients<C>>
	divideAndRemainder(Polynomial1Coefficients<C> x) {
		Polynomial1Coefficients<C> r = duplicate();
		Polynomial1Coefficients<C> q;
		
		if(deg() < x.deg()) {
			return new QuotientAndRemainder
			<Polynomial1Coefficients<C>>(
					newInstance(-1), x);
		}
		
		q = newInstance(deg() - x.deg());
		for(int k = deg() - x.deg(); k >= 0; k--) {
			Iterator<Integer> jtr = x.indexIterator();
			C qk;
			
			qk = r.getCoefficient(k + x.deg()).divide(
					x.getCoefficient(x.deg()));
			q.setCoefficient(k, qk);
			while(jtr.hasNext()) {
				int j = jtr.next() + k;
				C uj  = r.getCoefficient(j);
				C vjk = x.getCoefficient(j - k);
				
				uj = uj.subtract(qk.multiply(vjk));
				r.setCoefficient(j, uj);
			}
		}
		
		return new QuotientAndRemainder
		<Polynomial1Coefficients<C>>(q, r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return deg() < 0;
	}
	
	/**
	 * 
	 * @return
	 */
	public Polynomial1Coefficients<C> differenciate() {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> q = newInstance(deg() - 1);
		
		while(itr.hasNext()) {
			int i = itr.next();
			
			if(i > 0) {
				q.setCoefficient(
						i - 1,
						q.getCoefficient(i).multiply(i));
			}
		}
		return q;
	}
	
	/**
	 * 
	 * @param x
	 * @return
	 */
	public Polynomial1Coefficients<C> substitute(
			Polynomial1Coefficients<C> x) {
		Iterator<Integer> itr = indexIterator();
		Polynomial1Coefficients<C> r = newInstance(-1);
		
		while(itr.hasNext()) {
			int i = itr.next();
			
			r = r.add(x.power(i).multiply(getCoefficient(i)));
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		Iterator<Integer> itr = indexIterator();
		int r = Hashes.INIT;
		
		while(itr.hasNext()) {
			r = Hashes.A * (
					r + getCoefficient(itr.next()).hashCode());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof Polynomial1Coefficients) {
			Polynomial1Coefficients<?> p =
				(Polynomial1Coefficients<?>)obj;
			Iterator<Integer> itr = indexIterator();
			
			while(itr.hasNext()) {
				int i = itr.next();
				Object c1 = getCoefficient(i);
				Object c2 = p.getCoefficient(i);
				
				if(c2 == null || !c1.equals(c2)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public String toString(String var) {
		StringBuilder b = new StringBuilder();
		Iterator<Integer> itr = indexIterator();
		String d = "";
		List<Integer> l = new ArrayList<Integer>();
		
		while(itr.hasNext()) {
			l.add(itr.next());
		}
		
		for(int j = l.size() - 1; j >= 0; j--) {
			int i = l.get(j);
			
			b.append(d);
			b.append(getCoefficient(i).toString());
			if(i > 0) {
				b.append(var).append("^");
				b.append(i);
			}
			d = " + ";
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return toString("x");
	}
	
}
