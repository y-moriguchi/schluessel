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

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class GenerateKeyPair extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env, LispMessage mesg) {
		String al = SubrUtils.getSymbolName(c1a, mesg);
		KeyPairGenerator kg;
		KeyPair pr;

		try {
			kg = KeyPairGenerator.getInstance(al.toUpperCase());
			pr = kg.generateKeyPair();
			return MultiValues.newValues(
					new LispPublicKey(pr.getPublic()),
					new LispPrivateKey(pr.getPrivate()));
		} catch (NoSuchAlgorithmException e) {
			throw mesg.getError("err.keypair.algorithm.notfound", c1a);
		}
	}

}
