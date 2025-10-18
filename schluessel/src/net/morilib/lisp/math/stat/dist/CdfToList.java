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
package net.morilib.lisp.math.stat.dist;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.stat.dist.Distribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/18
 */
public class CdfToList extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		Distribution dst;
		ConsListBuilder b = new ConsListBuilder();
		double min, max, dx;

		min = SubrUtils.getDouble(c2a, mesg);
		max = SubrUtils.getDouble(c3a, mesg);
		dx  = SubrUtils.getDouble(c4a, mesg);
		if(c1a instanceof AbstractLispDistribution) {
			dst = ((AbstractLispDistribution)c1a).getDistribution();
			for(double t = min; t <= max; t += dx) {
				b.append(new LispDouble(dst.cdf(t)));
			}
			return b.get();
		} else {
			throw mesg.getError("err.stat.require.distribution", c1a);
		}
	}

}
