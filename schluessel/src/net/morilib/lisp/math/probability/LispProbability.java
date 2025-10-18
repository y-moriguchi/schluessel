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
package net.morilib.lisp.math.probability;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ISubr;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.AbstractLispQuantity;
import net.morilib.lisp.math.ILispQuantity;
import net.morilib.lisp.math.ILispQuantityFactory;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.lisp.math.random.ILispRandomSource;
import net.morilib.lisp.math.random.LispMTRandomSource;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.random.MersenneTwisterRandom;
import net.morilib.math.random.RandomSource;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/10
 */
public class LispProbability extends AbstractLispQuantity
implements ILispMultipliable<LispProbability>, ILispQuantity, ISubr,
java.io.Serializable {

	//
	private static final ILispQuantityFactory
	FACTORY = new ILispQuantityFactory() {

		public ILispQuantity getInstance(LispReal r) {
			return new LispProbability(r);
		}

	};

	//
	private static final LispInteger ONE_HUNDRED =
		LispInteger.valueOf(100);

	//
	private LispReal p;

	/**
	 * 
	 * @param p
	 */
	public LispProbability(LispReal p) {
		if(p.compareTo(LispInteger.ZERO) < 0 ||
				p.compareTo(LispInteger.ONE) > 0) {
			throw new IllegalArgumentException();
		}
		this.p = p;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ISubr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum t = Iterators.nextIf(itr);
		Datum f, d;
		RandomSource rnd;

		if(t instanceof ILispRandomSource) {
			rnd = ((ILispRandomSource)t).getRandomSource();
			t = LispBoolean.TRUE;  f = LispBoolean.FALSE;
			SubrUtils.checkTerminated(itr, body, mesg);
		} else if(t != null) {
			f = SubrUtils.nextIf(itr, mesg, body);
			d = Iterators.nextIf(itr,
					LispMTRandomSource.getInstance());
			if(!(d instanceof ILispRandomSource)) {
				throw mesg.getError("err.srfi27.require.randomsource",
						d);
			} else {
				rnd = ((ILispRandomSource)d).getRandomSource();
			}
			SubrUtils.checkTerminated(itr, body, mesg);
		} else {
			rnd = MersenneTwisterRandom.getInstance();
			t = LispBoolean.TRUE;  f = LispBoolean.FALSE;
		}
		return rnd.nextDouble3() < p.doubleValue() ? t : f;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispProbability mul(LispProbability y) {
		return new LispProbability(p.multiply(y.p));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(p.multiply(ONE_HUNDRED)).append("%");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getReal()
	 */
	@Override
	public LispReal getReal() {
		return p;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispQuantity#factory()
	 */
	public ILispQuantityFactory factory() {
		return FACTORY;
	}

}
