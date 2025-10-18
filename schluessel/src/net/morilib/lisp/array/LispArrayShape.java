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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public class LispArrayShape extends Datum2 {

	/**
	 * 
	 */
	public static final LispArrayShape RANK0 =
		new LispArrayShape(new int[0], new int[0]);

	//
	private int[] sIndices;
	private int[] eIndices;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/22
	 */
	public static class Shape extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			IntegerVector bin = new IntegerArrayVector();
			IntegerVector ein = new IntegerArrayVector();

			while(itr.hasNext()) {
				int b = SubrUtils.nextSmallInt(itr, mesg, body);
				int e = SubrUtils.nextSmallInt(itr, mesg, body);

				if(e < b) {
					throw mesg.getError(
							"err.srfi25.arraysize.invalid");
				}
				bin.add(b);
				ein.add(e);
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			return new LispArrayShape(bin.toIntArray(),
					ein.toIntArray());
		}

	}

	/**
	 * 
	 * @param eIndices
	 */
	public LispArrayShape(int[] eIndices) {
		this.eIndices = eIndices;
		this.sIndices = new int[eIndices.length];
		Arrays.fill(this.sIndices, 0);
	}

	/**
	 * 
	 * @param sIndices
	 * @param eIndices
	 */
	public LispArrayShape(int[] sIndices, int[] eIndices) {
		this.sIndices = sIndices;
		this.eIndices = eIndices;
	}

	//
	/*package*/ static int arraylength(int... is) {
		long r = 1;

		for(int i = 0; i < is.length; i++) {
			if(is[i] < 0) {
				throw new NegativeArraySizeException();
			} else if((r *= is[i]) > Integer.MAX_VALUE) {
				throw new NegativeArraySizeException();
			}
		}
		return (int)r;
	}

	//
	/*package*/ static int arrayindex(int[] eIndices, int[] is) {
		long p = 0, q = 1;

		if(is.length != eIndices.length) {
			throw new InvalidDimensionException();
		} else if(is.length == 0) {
//			throw new IndexOutOfBoundsException();
			return 0;
		}

		for(int i = is.length - 1; i >= 0; i--) {
			if(is[i] < 0 || is[i] >= eIndices[i]) {
				throw new IndexOutOfBoundsException();
			} else if((p += is[i] * q) > Integer.MAX_VALUE) {
				throw new IndexOutOfBoundsException();
			} else {
				q *= eIndices[i];
			}
		}
		return (int)p;
	}

	/**
	 * 
	 * @return
	 */
	public int rank() {
		return sIndices.length;
	}

	/**
	 * 
	 * @param dim
	 * @return
	 */
	public int getStartIndex(int dim) {
		return sIndices[dim];
	}

	/**
	 * 
	 * @param dim
	 * @return
	 */
	public int getEndIndex(int dim) {
		return eIndices[dim];
	}

	/**
	 * 
	 * @param is
	 * @return
	 */
	public boolean isValidRange(int... is) {
		if(is.length != rank()) {
			return false;
		}

		for(int i = 0; i < is.length; i++) {
			if(is[i] < sIndices[i]) {
				return false;
			} else if(is[i] >= eIndices[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public int length() {
		int l = 1;

		for(int i = 0; i < rank(); i++) {
			l *= eIndices[i] - sIndices[i];
		}
		return l;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<shape>");
	}

}
