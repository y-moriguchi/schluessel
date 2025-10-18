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
package net.morilib.lisp.math.stat.data;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.primitive.DoubleArrayVector;
import net.morilib.util.primitive.DoubleVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public class MakeStatisticsXyData extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		ConsIterator xtr = new ConsIterator(c1a);
		ConsIterator ytr = new ConsIterator(c2a);
		DoubleVector x = new DoubleArrayVector();
		DoubleVector y = new DoubleArrayVector();

		while(xtr.hasNext() && ytr.hasNext()) {
			x.addDouble(SubrUtils.getDouble(xtr.next(), mesg));
			y.addDouble(SubrUtils.getDouble(ytr.next(), mesg));
		}
		return new LispStatisticsXYData(
				x.toDoubleArray(), y.toDoubleArray());
	}

}
