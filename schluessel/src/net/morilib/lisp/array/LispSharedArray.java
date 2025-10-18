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
package net.morilib.lisp.array;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.SExpressionDatum;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public class LispSharedArray extends Datum2 implements ILispArray {

	//
	private ILispArray array;
	private LispArrayShape shape;
	private Procedure proc;
	private Environment env;
	private LispMessage mesg;

	/**
	 * @param c1a
	 * @param c2a
	 * @param c3a
	 * @param env
	 * @param mesg
	 */
	/*package*/ LispSharedArray(ILispArray a, LispArrayShape s,
			Procedure p, Environment env, LispMessage mesg) {
		this.array = a;
		this.shape = s;
		this.proc  = p;
		this.env   = env;
		this.mesg  = mesg;
	}

	/**
	 * @param c1a
	 * @param c2a
	 * @param c3a
	 * @param env
	 * @param mesg
	 */
	/*package*/ LispSharedArray(ILispArray a, int[] ei,
			Procedure p, Environment env, LispMessage mesg) {
		int[] bi = new int[ei.length];

		Arrays.fill(bi, 0);
		this.array = a;
		this.shape = new LispArrayShape(bi, ei);
		this.proc  = p;
		this.env   = env;
		this.mesg  = mesg;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#rank()
	 */
	public int rank() {
		return shape.rank();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#startIndex(int)
	 */
	public int startIndex(int dim) {
		return shape.getStartIndex(dim);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#endIndex(int)
	 */
	public int endIndex(int dim) {
		return shape.getEndIndex(dim);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#toVector()
	 */
	public LispVector toVector() {
		int[]   is = new int[shape.rank()];
		Datum[] rl = new Datum[shape.length()];
		int     n  = 0;

		for(int i = 0; i < is.length; i++) {
			is[i] = shape.getStartIndex(i);
		}
		outer: while(true) {
			rl[n++] = getFromArray(is);
			for(int i = is.length - 1; i >= 0; i--) {
				is[i]++;
				if(is[i] < shape.getEndIndex(i)) {
					break;
				} else if(i == 0) {
					break outer;
				} else {
					is[i] = shape.getStartIndex(i);
				}
			}
		}
		return new LispVector(rl);
	}

	//
	private int[] translateIndices(int... is) {
		Datum[] isd;
		Datum   rp;
		int[]   it;

		if(!shape.isValidRange(is)) {
			throw new IndexOutOfBoundsException();
		}

		isd = new Datum[is.length];
		for(int i = 0; i < is.length; i++) {
			isd[i] = LispInteger.valueOf(is[i]);
		}

		rp = Scheme.callva(proc, env, mesg, isd);
		if(rp instanceof MultiValues) {
			List<Datum> l = ((MultiValues)rp).getValues();

			it = new int[l.size()];
			for(int i = 0; i < it.length; i++) {
				it[i] = SubrUtils.getSmallInt(l.get(i), mesg);
			}
		} else if(rp instanceof SExpressionDatum) {
			List<Datum> l = LispUtils.consToList(rp, mesg);

			it = new int[l.size()];
			for(int i = 0; i < it.length; i++) {
				it[i] = SubrUtils.getSmallInt(l.get(i), mesg);
			}
		} else {
			it = new int[1];
			it[0] = SubrUtils.getSmallInt(rp, mesg);
		}
		return it;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#get(int[])
	 */
	public Datum getFromArray(int... is) {
		return array.getFromArray(translateIndices(is));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#set(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		array.setToArray(d, translateIndices(is));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<array>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isIndexEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isIndexEqualTo(ILispArray a) {
		if(rank() != a.rank()) {
			return false;
		} else {
			for(int i = 0; i < rank(); i++) {
				if(startIndex(i) != a.startIndex(i)) {
					return false;
				} else if(endIndex(i) != a.endIndex(i)) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return array.getTypeSpecifier();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		return LispDefaultArray.isEqualTo(this, a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#fill(java.util.Iterator)
	 */
	public void fill(Iterator<Datum> itr) {
		int[] is = new int[rank()];

		for(int i = 0; itr.hasNext(); i++) {
			for(int j = rank() - 1, k = i; j >= 0; j--) {
				is[j] = k % (endIndex(j) - startIndex(j));
				k     = k / (endIndex(j) - startIndex(j));
			}
			setToArray(itr.next(), is);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getShape()
	 */
	public LispArrayShape getShape() {
		return shape;
	}

}
