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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/23
 */
public class LispArrayC32 extends SRFI47Array
implements java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/23
	 */
	public static class Ac32 extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = Iterators.nextIf(itr);

			SubrUtils.checkTerminated(itr, body, mesg);
			if(d == null || d instanceof LispComplex) {
				return new Prototype(
						(d == null) ? 0 : (float)d.getRealDouble(),
						(d == null) ? 0 : (float)d.getImagDouble());
			} else {
				throw mesg.getError("err.require.complex", d);
			}
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
		private float real, imag;

		/**
		 * 
		 */
		public Prototype(float real, float imag) {
			this.real = real;
			this.imag = imag;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.array.LispArrayPrototype#makeArray(int[])
		 */
		public ILispArray makeArray(int... is) {
			return new LispArrayC32(real, imag, is);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<ac32 prototype>");
		}

	}

	//
	private float[] real, imag;

	/**
	 * 
	 * @param is
	 */
	public LispArrayC32(float r, float i, int... is) {
		super(is);
		real     = new float[LispArrayShape.arraylength(is)];
		imag     = new float[LispArrayShape.arraylength(is)];
		Arrays.fill(real, r);
		Arrays.fill(imag, i);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#rank()
	 */
	public int rank() {
		return eIndices.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#startIndex(int)
	 */
	public int startIndex(int dim) {
		if(dim < 0 || dim >= eIndices.length) {
			throw new IndexOutOfBoundsException();
		}
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#endIndex(int)
	 */
	public int endIndex(int dim) {
		return eIndices[dim];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#toVector()
	 */
	public LispVector toVector() {
		Datum[] r = new Datum[LispArrayShape.arraylength(eIndices)];

		for(int i = 0; i < r.length; i++) {
			r[i] = LispComplex.newComplex(real[i], imag[i]);
		}
		return new LispVector(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getFromArray(int[])
	 */
	public Datum getFromArray(int... is) {
		int p = LispArrayShape.arrayindex(eIndices, is);

		return LispComplex.newComplex(real[p], imag[p]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#setToArray(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		if(!(d instanceof LispComplex)) {
			throw new ClassCastException();
		} else {
			LispComplex n = (LispComplex)d;
			int p = LispArrayShape.arrayindex(eIndices, is);

			real[p] = (float)n.getRealDouble();
			imag[p] = (float)n.getImagDouble();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		if(!isIndexEqualTo(a)) {
			return false;
		} else if(a instanceof LispArrayC32) {
			return (Arrays.equals(real, ((LispArrayC32)a).real) &&
					Arrays.equals(imag, ((LispArrayC32)a).imag));
		} else {
			return LispDefaultArray.isEqualTo(this, a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return "floC32b";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#fill(java.util.Iterator)
	 */
	public void fill(Iterator<Datum> itr) {
		for(int i = 0; i < real.length && itr.hasNext(); i++) {
			Datum x = itr.next();

			if(x instanceof LispComplex) {
				real[i] = (float)x.getRealDouble();
				imag[i] = (float)x.getImagDouble();
			} else {
				throw new ClassCastException();
			}
		}
	}

}
