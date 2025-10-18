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

import net.morilib.geography.Latitude;
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
public class LispLatitude extends Datum2
implements ILispAddable<LispLatitude>, ILispSubtractable<LispLatitude>,
ILispScalarMultipliable<LispLatitude> {

	//
	Latitude lat;

	/**
	 * 
	 * @param lat
	 */
	public LispLatitude(Latitude lat) {
		this.lat = lat;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(lat.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispScalarMultipliable#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispLatitude mul(LispNumber x) {
		if(x instanceof LispSmallInt) {
			return new LispLatitude(
					lat.multiply(x.getExactSmallInt()));
		} else {
			return new LispLatitude(lat.multiply(x.getRealDouble()));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	@Override
	public LispLatitude sub(LispLatitude y) {
		return new LispLatitude(lat.subtract(y.lat));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	@Override
	public LispLatitude add(LispLatitude y) {
		return new LispLatitude(lat.add(y.lat));
	}

}
