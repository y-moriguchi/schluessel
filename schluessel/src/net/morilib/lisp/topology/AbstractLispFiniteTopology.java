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
package net.morilib.lisp.topology;

import java.util.HashSet;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.collection.LispHashSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/14
 */
public abstract class AbstractLispFiniteTopology
extends AbstractLispTopology implements ILispEnumerableTopology {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isContained(ILispTopology t) {
		if(t.isEmpty()) {
			return false;
		} else if(t.isUniverse()) {
			return true;
		} else {
			for(Datum d : getJavaSet()) {
				if(!t.isNeighborhoodOf(d)) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		if(t.isEmpty()) {
			return true;
		} else if(t.isUniverse()) {
			return false;
		} else {
			for(Datum d : getJavaSet()) {
				if(t.isNeighborhoodOf(d)) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	@Override
	public ILispTopology unionTopology(ILispTopology t) {
		if(t instanceof ILispEnumerableTopology) {
			Set<Datum> r = new HashSet<Datum>();

			r.addAll(getJavaSet());
			r.addAll(((ILispEnumerableTopology)t).getJavaSet());
			return new LispHashSet(r);
		} else {
			return super.unionTopology(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#intersectTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	@Override
	public ILispTopology intersectionTopology(ILispTopology t) {
		if(t instanceof ILispEnumerableTopology) {
			Set<Datum> r = new HashSet<Datum>();

			r.addAll(getJavaSet());
			r.retainAll(((ILispEnumerableTopology)t).getJavaSet());
			return new LispHashSet(r);
		} else {
			return super.intersectionTopology(t);
		}
	}

}
