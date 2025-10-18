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
package net.morilib.lang.proposition;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.morilib.lang.composite.TraverseCollection;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/05
 */
public final class LogicalOr<S extends Proposition<S>>
implements Proposition<S> {
	
	//
	@TraverseCollection(ArrayList.class)
	/*package*/ List<Proposition<S>> propositions;
	/*package*/ Proposition<S> negation;
	
	//
	/*package*/ LogicalOr(List<Proposition<S>> propositions) {
		this.propositions = propositions;
	}
	
	//
	/*package*/ LogicalOr(
			List<Proposition<S>> propositions,
			Proposition<S> negation) {
		this.propositions = propositions;
		this.negation     = negation;
	}
	
	/**
	 * 
	 * @param propositions
	 * @return
	 */
	public static<S extends Proposition<S>>
	Proposition<S> newInstance(
			List<Proposition<S>> propositions) {
		LogicalOr<S> res =
			new LogicalOr<S>(new ArrayList<Proposition<S>>());
		List<Proposition<S>> q;
		
		if(propositions == null) {
			throw new NullPointerException();
		}
		
		q = new ArrayList<Proposition<S>>(propositions.size());
		for(int i = 0; i < propositions.size(); i++) {
			q.add(LogicalNot.newInstance(propositions.get(i)));
		}
		res.negation = new LogicalAnd<S>(q, res);
		
		for(Proposition<S> p : propositions) {
			if(p == null) {
				throw new NullPointerException();
			} else if(p.isTrue()) {
				return p;
			} else if(p.isFalse()) {
				// do nothing
			} else if(p instanceof LogicalOr) {
				LogicalAnd<S> pp = (LogicalAnd<S>)p;
				
				for(Proposition<S> e : pp.propositions) {
					if(!res.implies(e)) {
						res.propositions.add(e);
					}
				}
			} else {
				if(!res.implies(p)) {
					res.propositions.add(p);
				}
			}
		}
		return res;
	}
	
	/**
	 * 
	 * @param <S>
	 * @param propositions
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static<S extends Proposition<S>>
	Proposition<S> newInstance(
			Proposition<?>... propositions) {
		return newInstance(Arrays.<Proposition<S>>asList(
				(Proposition<S>[])propositions));
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is(java.lang.Object[])
	 */
	@Override
	public boolean is(Object... variables) {
		for(Proposition<S> p : propositions) {
			if(p.is(variables)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
	 */
	@Override
	public boolean is1(Object var1) {
		for(Proposition<S> p : propositions) {
			if(p.is(var1)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is2(java.lang.Object, java.lang.Object)
	 */
	@Override
	public boolean is2(Object var1, Object var2) {
		for(Proposition<S> p : propositions) {
			if(p.is(var1, var2)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isEqualTo(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		if(p instanceof LogicalOr) {
			return ManyVariableProposition.isEquivalent(
					propositions, ((LogicalAnd<S>)p).propositions);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrueIf(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean implies(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
//		for(Proposition<S> e : propositions) {
//			if(e.implies(p)) {
//				return true;
//			}
//		}
//		return false;
		return p.isIndependent(negation);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isImplied(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		for(Proposition<S> e : propositions) {
			if(!e.isImplied(p)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isIndependent(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		for(Proposition<S> e : propositions) {
			if(!e.isIndependent(p)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isFalse()
	 */
	@Override
	public boolean isFalse() {
		return propositions.size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrue()
	 */
	@Override
	public boolean isTrue() {
		return false;
	}
	
	/**
	 * 
	 */
	public String toString() {
		StringBuilder buf = new StringBuilder();
		String dlm = "";
		
		buf.append("(");
		for(Proposition<S> e : propositions) {
			buf.append(dlm);
			buf.append(e.toString());
			dlm = " or ";
		}
		buf.append(")");
		return buf.toString();
	}

}
