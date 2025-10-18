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
package net.morilib.lisp.geography;

import net.morilib.geography.Longitude;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.math.algebra.ILispScalarMultipliable;
import net.morilib.lisp.math.algebra.ILispSubtractable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/10/20
 */
public class LispLongitude extends Datum2
implements ILispAddable<LispLongitude>, ILispSubtractable<LispLongitude>,
ILispScalarMultipliable<LispLongitude> {

	//
	Longitude longi;

	/**
	 * 
	 * @param lat
	 */
	public LispLongitude(Longitude longi) {
		this.longi = longi;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(longi.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispScalarMultipliable#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispLongitude mul(LispNumber x) {
		if(x instanceof LispSmallInt) {
			return new LispLongitude(
					longi.multiply(x.getExactSmallInt()));
		} else {
			return new LispLongitude(
					longi.multiply(x.getRealDouble()));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	@Override
	public LispLongitude sub(LispLongitude y) {
		return new LispLongitude(longi.subtract(y.longi));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	@Override
	public LispLongitude add(LispLongitude y) {
		return new LispLongitude(longi.add(y.longi));
	}

}
