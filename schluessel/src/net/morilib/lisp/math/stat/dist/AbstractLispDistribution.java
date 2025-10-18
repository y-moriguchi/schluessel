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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.math.stat.dist.Distribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public abstract class AbstractLispDistribution extends UnaryArgs
implements ILispDistribution {

	/**
	 * 
	 * @return
	 */
	public abstract Distribution getDistribution();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public abstract void toDisplayString(StringBuilder buf);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.analysis.ILispIntegrable1#integrate(net.morilib.lisp.LispReal, net.morilib.lisp.LispReal)
	 */
	public LispReal integrate(LispReal a, LispReal b) {
		return new LispDouble(getDistribution().cdf(
				a.doubleValue(), b.doubleValue()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		return new LispDouble(getDistribution().f(
				SubrUtils.getDouble(c1a, mesg)));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return getDistribution().hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		return (getClass().equals(o.getClass()) &&
				getDistribution().equals(((AbstractLispDistribution)
						o).getDistribution()));
	}

}
