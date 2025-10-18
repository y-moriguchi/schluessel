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
package net.morilib.lisp.math.stat.xy;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;
import net.morilib.util.primitive.DoubleArrayVector;
import net.morilib.util.primitive.DoubleVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public abstract class SubrXyProperties extends Subr {

	/**
	 * @param dx
	 * @return
	 */
	protected abstract Datum lispXy(ILispXYData dx);

	/**
	 * @param doubleArray
	 * @param doubleArray2
	 * @return
	 */
	protected abstract Datum statdouble(double[] x, double[] y);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body), xtr, ytr;
		Datum dx = SubrUtils.nextIf(itr, mesg, body);
		Datum dy = Iterators.nextIf(itr);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(dx instanceof ILispXYData && dy == null) {
			return lispXy((ILispXYData)dx);
		} else if(dy != null) {
			DoubleVector x = new DoubleArrayVector();
			DoubleVector y = new DoubleArrayVector();

			xtr = new ConsIterator(dx);
			ytr = new ConsIterator(dy);
			while(xtr.hasNext() && ytr.hasNext()) {
				x.addDouble(SubrUtils.getDouble(xtr.next(), mesg));
				y.addDouble(SubrUtils.getDouble(ytr.next(), mesg));
			}

			if(xtr.hasNext() || ytr.hasNext()) {
				throw mesg.getError("err.stat.xydata.samesize");
			} else if(x.size() == 0) {
				throw mesg.getError("err.require.list.notnull");
			}
			return statdouble(x.toDoubleArray(), y.toDoubleArray());
		} else {
			throw mesg.getError("err.argument", body);
		}
	}

}
