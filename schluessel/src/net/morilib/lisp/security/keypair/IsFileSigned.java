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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/12
 */
public class IsFileSigned extends Subr {

	//
	private static final int BUF_SIZE = 2048;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		InputStream ins = null;
		Datum c2a = LispString.EMPTY, c3a;
		String a1 = "";
		File fn;

		try {
			Signature sg;
			byte[] buf = new byte[BUF_SIZE];
			int sz;
			ConsIterator itr = new ConsIterator(body);

			fn  = LispFiles.nextFile(itr, env, mesg, body);
			c2a = SubrUtils.nextIf(itr, mesg, body);
			c3a = SubrUtils.nextIf(itr, mesg, body);
			a1  = SubrUtils.nextSymbolName(itr, null, mesg);

			if(a1 != null) {
				a1 = (a1.toUpperCase() + "with" +
						SubrUtils.nextSymbolName(itr, mesg, body)
						.toUpperCase());
			} else {
				a1 = LispSignature.defaultAlgorithm(env);
			}

			if(!(c2a instanceof LispPublicKey)) {
				throw mesg.getError("err.keypair.require.key.public",
						c2a);
			} else if(!(c3a instanceof LispSignature)) {
				throw mesg.getError("err.keypair.require.signature",
						c3a);
			}
			sg  = Signature.getInstance(a1);
			sg.initVerify(((LispPublicKey)c2a).key);
			ins = new BufferedInputStream(new FileInputStream(fn));
			while((sz = ins.read(buf)) >= 0) {
				sg.update(buf, 0, sz);
			}
			return LispBoolean.getInstance(sg.verify(
					((LispSignature)c3a).digest));
		} catch (IOException e) {
			throw mesg.getError("err.io");
		} catch (NoSuchAlgorithmException e) {
			throw mesg.getError("err.digest.algorithm.notfound", a1);
		} catch (InvalidKeyException e) {
			throw mesg.getError("err.keypair.key.invalid", c2a);
		} catch (SignatureException e) {
			throw new RuntimeException(e);
		} finally {
			IOs.close(ins);
		}
	}

}
