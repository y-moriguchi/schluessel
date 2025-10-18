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

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Iterator;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public class LispDefaultArray extends SRFI25Array
implements java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/23
	 */
	public static class DefProto extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = Iterators.nextIf(itr);

			SubrUtils.checkTerminated(itr, body, mesg);
			return new Prototype((d == null) ? Undef.UNDEF : d);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/23
	 */
	public static class Prototype extends Datum2
	implements LispArrayPrototype {

		//
		private Datum datum;

		/**
		 * 
		 */
		public Prototype(Datum datum) {
			this.datum = datum;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.array.LispArrayPrototype#makeArray(int[])
		 */
		public ILispArray makeArray(int... is) {
			return malloc(is, datum);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<default prototype>");
		}

	}

	//
	private LispArrayShape shape;
	private Object array;

	//
	private LispDefaultArray(LispArrayShape d, Object array) {
		this.shape = d;
		this.array = array;
	}

	/**
	 * @param d
	 * @param o
	 * @return
	 */
	public static SRFI25Array malloc(LispArrayShape d, Datum o) {
		int[] is = new int[d.rank()];
		LispDefaultArray r;

		if(d.rank() == 0) {
			return new LispRank0Array(o);
		} else {
			for(int i = 0; i < is.length; i++) {
				is[i] = d.getEndIndex(i) - d.getStartIndex(i);
			}
			r = new LispDefaultArray(d,
					Array.newInstance(Datum.class, is));
			r.clear(o);
			return r;
		}
	}

	/**
	 * 
	 * @param bi
	 * @param ei
	 * @param o
	 * @return
	 */
	public static SRFI25Array malloc(int[] bi, int[] ei, Datum o) {
		return malloc(new LispArrayShape(bi, ei), o);
	}

	/**
	 * 
	 * @param bi
	 * @param ei
	 * @param o
	 * @return
	 */
	public static SRFI25Array malloc(int[] ei, Datum o) {
		int[] bi = new int[ei.length];

		Arrays.fill(bi, 0);
		return malloc(new LispArrayShape(bi, ei), o);
	}

	/**
	 * 
	 * @param itr
	 */
	public void fill(Iterator<Datum> itr) {
		int[] is = new int[shape.rank()];

		Arrays.fill(is, 0);
		while(itr.hasNext()) {
			Object a2 = array;
			Datum  d  = itr.next();

			for(int i = 0; i < is.length - 1; i++) {
				a2 = Array.get(a2, is[i]);
			}
			Array.set(a2, is[is.length - 1], d);

			for(int i = is.length - 1; i >= 0; i--) {
				is[i]++;
				if(is[i] + shape.getStartIndex(i) <
						shape.getEndIndex(i)) {
					break;
				} else if(i == 0) {
					return;
				} else {
					is[i] = 0;
				}
			}
		}
	}

	/**
	 * 
	 * @param itr
	 */
	public static boolean isEqualTo(ILispArray a, ILispArray b) {
		int[] is;

		if(!a.isIndexEqualTo(b)) {
			return false;
		}

		is = new int[a.rank()];
		for(int i = 0; i < a.rank(); i++) {
			is[i] = a.startIndex(i);
		}

		while(true) {
			if(!LispUtils.equals(
					a.getFromArray(is), b.getFromArray(is))) {
				return false;
			}

			for(int i = is.length - 1; i >= 0; i--) {
				is[i]++;
				if(is[i] < a.endIndex(i)) {
					break;
				} else if(i == 0) {
					return true;
				} else {
					is[i] = a.startIndex(i);
				}
			}
		}
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

		Arrays.fill(is, 0);
		outer: while(true) {
			Object a2 = array;

			for(int i = 0; i < is.length - 1; i++) {
				a2 = Array.get(a2, is[i]);
			}
			rl[n++] = (Datum)Array.get(a2, is[is.length - 1]);

			for(int i = is.length - 1; i >= 0; i--) {
				is[i]++;
				if(is[i] + shape.getStartIndex(i) <
						shape.getEndIndex(i)) {
					break;
				} else if(i == 0) {
					break outer;
				} else {
					is[i] = 0;
				}
			}
		}
		return new LispVector(rl);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#get(int[])
	 */
	public Datum getFromArray(int... is) {
		Object a2 = array;
		int i = 0;

		if(is.length != rank()) {
			throw new InvalidDimensionException();
		}

		for(; i < is.length - 1; i++) {
			a2 = Array.get(a2, is[i] - shape.getStartIndex(i));
		}
		return (Datum)Array.get(a2, is[i] - shape.getStartIndex(i));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#set(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		Object a2 = array;
		int i = 0;

		if(is.length != rank()) {
			throw new InvalidDimensionException();
		}

		for(; i < is.length - 1; i++) {
			a2 = Array.get(a2, is[i] - shape.getStartIndex(i));
		}
		Array.set(a2, is[i] - shape.getStartIndex(i), d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#clear(net.morilib.lisp.Datum)
	 */
	public void clear(Datum o) {
		int[] is = new int[shape.rank()];

		Arrays.fill(is, 0);
		outer: while(true) {
			Object a2 = array;

			for(int i = 0; i < is.length - 1; i++) {
				a2 = Array.get(a2, is[i]);
			}
			Array.set(a2, is[is.length - 1], o);

			for(int i = is.length - 1; i >= 0; i--) {
				is[i]++;
				if(is[i] + shape.getStartIndex(i) <
						shape.getEndIndex(i)) {
					break;
				} else if(i == 0) {
					break outer;
				} else {
					is[i] = 0;
				}
			}
		}
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
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		return isEqualTo(this, a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getShape()
	 */
	public LispArrayShape getShape() {
		return shape;
	}

}
