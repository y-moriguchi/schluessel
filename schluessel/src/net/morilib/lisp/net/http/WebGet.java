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
package net.morilib.lisp.net.http;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/23
 */
public class WebGet extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		String url = SubrUtils.getString(c1a, mesg);
		File   fl;
		HttpURLConnection con;
		URLConnection cn1;
		OutputStream ous = null;
		InputStream  ins = null;
		int cd;

		try {
			cn1 = new URL(url).openConnection();
			if(!(cn1 instanceof HttpURLConnection)) {
				throw mesg.getError("err.net.protocol.invalid");
			}

			con = (HttpURLConnection)cn1;
			con.setRequestMethod("GET");
			con.connect();
			if((cd = con.getResponseCode()) >= 400) {
				return MultiValues.newValues(
						LispBoolean.FALSE, LispInteger.valueOf(cd));
			}

			fl  = LispFiles.getFile(env, c2a, mesg);
			ins = con.getInputStream();
			ous = new FileOutputStream(fl);
			IOs.copy(ins, ous);
			return MultiValues.newValues(
					LispBoolean.TRUE, LispInteger.valueOf(cd));
		} catch (MalformedURLException e) {
			throw mesg.getError("err.net.url.malform", e.getMessage());
		} catch (IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		} finally {
			IOs.close(ins);
			IOs.close(ous);
		}
	}

}
