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
package net.morilib.lisp.math.stat.test;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.stat.StatisticsUtils;
import net.morilib.math.stat.dist.continuous.TDistribution;
import net.morilib.util.primitive.DoubleArrayVector;
import net.morilib.util.primitive.DoubleVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/22
 */
public class IsTestT2Sample extends QuaternaryArgs {

	//
	private void todouble(ConsIterator itr, DoubleVector v,
			LispMessage mesg) {
		while(itr.hasNext()) {
			v.addDouble(SubrUtils.getDouble(itr.next(), mesg));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c4a, Datum c5a, Datum c6a,
			Environment env, LispMessage mesg) {
		TDistribution tdis;
		DoubleVector obs, ob2;
		double[] obsv, ob2v;
		String type = SubrUtils.getSymbolName(c5a, mesg);
		double sign = SubrUtils.getDouble(c6a, mesg);
		double t, max, min, mean, var, mn2, var2, sv1, sv2, df;
		int num, num2;

		obs  = new DoubleArrayVector();
		ob2  = new DoubleArrayVector();
		todouble(new ConsIterator(c1a), obs, mesg);
		todouble(new ConsIterator(c4a), ob2, mesg);
		obsv = obs.toDoubleArray();
		ob2v = ob2.toDoubleArray();

		num  = obs.size();
		num2 = ob2.size();
		mean = StatisticsUtils.mean(obsv);
		var  = StatisticsUtils.variance(obsv);
		mn2  = StatisticsUtils.mean(ob2v);
		var2 = StatisticsUtils.variance(ob2v);
		sv1  = var  / num;
		sv2  = var2 / num2;
		t    = (mean - mn2) / Math.sqrt(sv1 + sv2);
		df   = ((sv1 + sv2) * (sv1 + sv2) /
				(sv1 * sv1 / (num - 1) + sv2 * sv2 / (num2 - 1)));
		tdis = new TDistribution(df);

		if(type.equals(">")) {
			min = Double.NEGATIVE_INFINITY;
			max = -tdis.invCdf(sign);
		} else if(type.equals("<")) {
			min = tdis.invCdf(sign);
			max = Double.POSITIVE_INFINITY;
		} else {
			min = tdis.invCdf(sign / 2);
			max = -min;
		}
		return LispBoolean.getInstance(min < t && t < max);
	}

}
