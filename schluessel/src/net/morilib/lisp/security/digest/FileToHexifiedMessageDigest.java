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
package net.morilib.lisp.security.digest;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class FileToHexifiedMessageDigest extends BinaryArgs {

	//
	private static final int BUF_SIZE = 1024;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		String s  = SubrUtils.getString(c1a, mesg);
		String al = SubrUtils.getSymbolName(c2a, mesg);
		MessageDigest md;
		InputStream ins = null;
		byte[] b = new byte[BUF_SIZE];
		int sz;

		try {
			md  = MessageDigest.getInstance(al.toUpperCase());
			ins = new BufferedInputStream(new FileInputStream(
					LispFiles.getFile(env, s)));
			while((sz = ins.read(b)) >= 0) {
				md.update(b, 0, sz);
			}
			return new LispString(
					LispMessageDigest.hexify(md.digest()));
		} catch (NoSuchAlgorithmException e) {
			throw mesg.getError("err.digest.algorithm.notfound", c2a);
		} catch (IOException e) {
			throw mesg.getError("err.io");
		} finally {
			IOs.close(ins);
		}
	}

}
