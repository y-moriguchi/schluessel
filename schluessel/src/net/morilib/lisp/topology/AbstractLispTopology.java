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

import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/08
 */
public abstract class AbstractLispTopology extends Datum2
implements ILispTopology {

	//
	private Boolean isContained0(ILispTopology t) {
		if(t.isEmpty()) {
			return false;
		} else if(t.isUniverse()) {
			return true;
		} else if(t instanceof LispIntersection) {
			for(ILispTopology s : ((LispIntersection)t).topologies) {
				if(!s.isContained(t)) {
					return false;
				}
			}
			return true;
		} else {
			return null;
		}
	}

	//
	private Boolean isIndependent0(ILispTopology t) {
		if(t.isEmpty()) {
			return true;
		} else if(t.isUniverse()) {
			return false;
		} else if(t instanceof LispUnion) {
			for(ILispTopology s : ((LispUnion)t).topologies) {
				if(!s.isIndependent(t)) {
					return false;
				}
			}
			return true;
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isContained(ILispTopology t) {
		Boolean b = isContained0(t);

		return (b != null) && b.booleanValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		Boolean b = isIndependent0(t);

		return (b != null) && b.booleanValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isNotContained(ILispTopology t) {
		Boolean b;

		if(isContained(t)) {
			return false;
		} else if((b = isContained0(t)) != null) {
			return !b.booleanValue();
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isNotIndependent(ILispTopology t) {
		Boolean b;

		if(isIndependent(t)) {
			return false;
		} else if((b = isIndependent0(t)) != null) {
			return !b.booleanValue();
		} else {
			return false;
		}
	}

	//
	private boolean isTrue(Boolean b) {
		return b != null && b.booleanValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology unionTopology(ILispTopology t) {
		if(t.isEmpty()) {
			return this;
		} else if(t.isUniverse()) {
			return t;
		} else if(isTrue(isContained(t))) {
			return t;
		} else if(t.isContained(this)) {
			return this;
		} else {
			return LispUnion.newInstance(this, t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#intersectTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology intersectionTopology(ILispTopology t) {
		if(t.isEmpty()) {
			return t;
		} else if(t.isUniverse()) {
			return this;
		} else if(isTrue(isContained(t))) {
			return this;
		} else if(t.isContained(this)) {
			return t;
		} else {
			return LispIntersection.newInstance(this, t);
		}
	}

}
