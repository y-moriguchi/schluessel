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

import net.morilib.lisp.Datum;
import net.morilib.lisp.topology.ILispTopology;
import net.morilib.lisp.topology.LispUnion;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/20
 */
public abstract class LispMutableSetDatum extends LispSetDatum
implements LispPurelyMutableCollection {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public abstract LispMutableSetDatum prototype();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public abstract LispMutableSetDatum duplicate();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#addAll(net.morilib.lisp.collection.LispSet)
	 */
	public abstract Datum add(Datum s);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#removeAll(net.morilib.lisp.collection.LispSet)
	 */
	public abstract Datum delete(Datum s);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#intersection(net.morilib.lisp.collection.LispSet)
	 */
	public abstract LispSetDatum intersection(LispSet s);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#addAll(net.morilib.lisp.collection.LispSet)
	 */
	@Override
	public LispSetDatum addAll(LispSet s) {
		try {
			return super.addAll(s);
		} catch (ImmutableException e) {
			throw new RuntimeException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#removeAll(net.morilib.lisp.collection.LispSet)
	 */
	@Override
	public LispSetDatum removeAll(LispSet s) {
		try {
			return super.removeAll(s);
		} catch (ImmutableException e) {
			throw new RuntimeException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#symmetricDifferenceModify(net.morilib.lisp.collection.LispSet)
	 */
	@Override
	public Datum symmetricDifferenceModify(LispSet s) {
		try {
			return super.symmetricDifferenceModify(s);
		} catch (ImmutableException e) {
			throw new RuntimeException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	@Override
	public Datum addFrom(LispBag s) {
		try {
			return super.addFrom(s);
		} catch (ImmutableException e) {
			throw new RuntimeException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSetDatum#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	@Override
	public Datum deleteFrom(LispBag s) {
		try {
			return super.deleteFrom(s);
		} catch (ImmutableException e) {
			throw new RuntimeException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#copyAdd(net.morilib.lisp.Datum)
	 */
	public Datum copyAdd(Datum d) {
		return duplicate().add(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#copyDelete(net.morilib.lisp.Datum)
	 */
	public Datum copyDelete(Datum d) {
		return duplicate().delete(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#union(net.morilib.lisp.collection.LispSet)
	 */
	public LispSetDatum union(LispSet s) {
		return duplicate().addAll(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#difference(net.morilib.lisp.collection.LispSet)
	 */
	public LispSetDatum difference(LispSet s) {
		return duplicate().removeAll(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#symmetricDifference(net.morilib.lisp.collection.LispSet)
	 */
	public Datum symmetricDifference(LispSet s) {
		return duplicate().symmetricDifferenceModify(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyAddFrom(LispBag d) {
		return duplicate().addFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFrom(LispBag d) {
		return duplicate().deleteFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology unionTopology(ILispTopology t) {
		if(t instanceof LispSet) {
			return union((LispSet)t);
		} else if(t.isEmpty()) {
			return this;
		} else if(t.isUniverse()) {
			return t;
		} else {
			return LispUnion.newInstance(this, t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology intersectionTopology(ILispTopology t) {
		if(t instanceof LispSet) {
			return intersection((LispSet)t);
		} else if(t.isEmpty()) {
			return this;
		} else if(t.isUniverse()) {
			return t;
		} else {
			return LispUnion.newInstance(this, t);
		}
	}

}
