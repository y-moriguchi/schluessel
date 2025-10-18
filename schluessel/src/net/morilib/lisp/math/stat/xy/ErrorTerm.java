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

import net.morilib.lisp.Datum;
import net.morilib.lisp.math.stat.data.LispStatisticsXYData;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public class ErrorTerm extends SubrXyProperties {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.SubrXyProperties#lispXy(net.morilib.lisp.math.stat.xy.ILispXYData)
	 */
	@Override
	protected Datum lispXy(ILispXYData dx) {
		return dx.errorTermData();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.SubrXyProperties#statdouble(double[], double[])
	 */
	@Override
	protected Datum statdouble(double[] x, double[] y) {
		return LispStatisticsXYData.newInstance(x, y).errorTermData();
	}

}
