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
package net.morilib.lisp.security.keypair;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class LispSignature extends Datum2
implements java.io.Serializable {

	//
	private static final Symbol DEF_ALGO_SYM =
		Symbol.getSymbol("*default-signature-algorithm*");
	private static final String DEF_ALGO = "SHA1withRSA";

	//
	byte[] digest;

	//
	LispSignature(byte[] digest) {
		this.digest = digest;
	}

	/**
	 * 
	 * @param digest
	 * @return
	 */
	public static String hexify(byte[] digest) {
		StringBuilder b = new StringBuilder();

		for(int i = 0; i < digest.length; i++) {
			b.append(Integer.toString(
					Bytes.ubyteToInt(digest[i]), 16));
		}
		return b.toString();
	}

	/**
	 * @return
	 */
	public static String defaultAlgorithm(Environment env) {
		Datum d = env.findDatum(DEF_ALGO_SYM);
		String s;
		Cons c;

		if(d instanceof Cons) {
			if((c = (Cons)d).getCar() instanceof Symbol) {
				s = ((Symbol)c.getCar()).getName().toUpperCase();
				if(c.getCdr() instanceof Cons) {
					c = (Cons)c.getCdr();
					if(c.getCar() instanceof Symbol) {
						s += "with";
						s += ((Symbol)
								c.getCar()).getName().toUpperCase();
						return s;
					}
				}
			}
		}
		return DEF_ALGO;
	}

	/**
	 * @param itr
	 * @param env
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static String nextAlgorithm(ConsIterator itr,
			Environment env, LispMessage mesg, Datum body) {
		String st = SubrUtils.nextSymbolName(itr, null, mesg);

		return (st != null) ? st : defaultAlgorithm(env);
	}

	/**
	 * 
	 * @return
	 */
	public String hexify() {
		StringBuilder b = new StringBuilder();

		for(int i = 0; i < digest.length; i++) {
			b.append(Integer.toString(
					Bytes.ubyteToInt(digest[i]), 16));
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<signature>");
	}

}
