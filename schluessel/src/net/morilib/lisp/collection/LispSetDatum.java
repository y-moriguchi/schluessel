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
package net.morilib.lisp.collection;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum3;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.topology.ILispTopology;
import net.morilib.lisp.topology.LispCardinality;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/20
 */
public abstract class LispSetDatum extends Datum3
implements LispSet, LispLimitedCollection {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		ConsListBuilder b = new ConsListBuilder();

		for(Datum o : this) {
			b.append(o);
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a) {
		return contains(c2a) ? 1 : 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure p,
			Environment env, LispMessage mesg) {
//		if(col instanceof LispSet) {
//			if(size() != col.size()) {
//				return false;
//			} else {
//				outer: for(Datum e : this) {
//					for(Datum f : col) {
//						if(Scheme.callva(
//								p, env, mesg, e, f).isTrue()) {
//							continue outer;
//						}
//					}
//					return false;
//				}
//				return true;
//			}
//		} else {
//			return false;
//		}
		return LispCollections.equalTo(this, col, p, env, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#subset(net.morilib.lisp.collection.LispSet)
	 */
	public boolean subset(LispSet set) {
		outer: for(Datum e : this) {
			for(Datum f : set) {
				if(equivalence(e, f)) {
					continue outer;
				}
			}
			return false;
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public abstract LispSetDatum prototype();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public abstract LispSetDatum duplicate();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#addAll(net.morilib.lisp.collection.LispSet)
	 */
	public LispSetDatum addAll(LispSet s) throws ImmutableException {
		for(Datum d : s) {
			add(d);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#removeAll(net.morilib.lisp.collection.LispSet)
	 */
	public LispSetDatum removeAll(
			LispSet s) throws ImmutableException {
		for(Datum d : s) {
			if(contains(d)) {
				delete(d);
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#symmetricDifferenceModify(net.morilib.lisp.collection.LispSet)
	 */
	public Datum symmetricDifferenceModify(
			LispSet s) throws ImmutableException {
		for(Datum d : s) {
			if(contains(d)) {
				delete(d);
			} else {
				add(d);
			}
		}
		return this;
	}


	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum addFrom(LispBag s) throws ImmutableException {
		for(Datum d : s) {
			add(d);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFrom(LispBag s) throws ImmutableException {
		for(Datum d : s) {
			if(contains(d)) {
				delete(d);
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispSet mul(LispSet y) {
		LispSet r = prototype();
		boolean im = false;

		for(Datum dx : this) {
			for(Datum dy : y) {
				try {
					if(im) {
						r = (LispSet)r.copyAdd(new Cons(dx, dy));
					} else {
						r.add(new Cons(dx, dy));
					}
				} catch (ImmutableException e) {
					r = (LispSet)r.copyAdd(new Cons(dx, dy));
					im = true;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispSet add(LispSet y) {
		return (LispSet)union(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	public LispSet uminus() {
		return prototype();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	public LispSet sub(LispSet y) {
		return (LispSet)difference(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNeighborOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		return contains(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isContained(ILispTopology t) {
		for(Datum d : this) {
			if(!t.isNeighborhoodOf(d)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		for(Datum d : this) {
			if(t.isNeighborhoodOf(d)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isNotContained(ILispTopology t) {
		return !isContained(t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isNotIndependent(ILispTopology t) {
		return !isIndependent(t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#interior()
	 */
	public ILispTopology interior() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#closure()
	 */
	public ILispTopology closure() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isOpen()
	 */
	public boolean isOpen() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isClosed()
	 */
	public boolean isClosed() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#cardinality()
	 */
	public LispCardinality cardinality() {
		return LispCardinality.finiteValueOf(size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isEmpty()
	 */
	public boolean isEmpty() {
		return isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		return false;
	}

}
