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

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.SignedObject;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/13
 */
public class VerifySignedDatum extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum  d1 = SubrUtils.nextIf(itr, mesg, body);
		Datum  d2 = SubrUtils.nextIf(itr, mesg, body);
		String al = LispSignature.nextAlgorithm(itr, env, mesg, body);
		SignedObject sd;
		Signature sg;

		try {
			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(d2 instanceof LispPublicKey)) {
				throw mesg.getError("err.keypair.require.key.public",
						d2);
			} else if(d1 instanceof LispSignedDatum) {
				sg = Signature.getInstance(al);
				sd = ((LispSignedDatum)d1).datum;
				return LispBoolean.getInstance(sd.verify(
						((LispPublicKey)d2).key, sg));
			} else {
				throw mesg.getError("err.keypair.require.signeddatum",
						d1);
			}
		} catch (NoSuchAlgorithmException e) {
			throw mesg.getError("err.digest.algorithm.notfound", al);
		} catch (InvalidKeyException e) {
			throw mesg.getError("err.keypair.key.invalid", d2);
		} catch (SignatureException e) {
			throw new RuntimeException(e);
		}
	}

}
