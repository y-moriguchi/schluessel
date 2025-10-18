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

import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.util.Map;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/17
 */
public class HttpPropertiesSetS extends Subr {

	//
	private static final Symbol ALLOW_USER_INTERACTION =
		Symbol.getSymbol("allow-user-interatction");
	private static final Symbol IF_MODIFIED_SINCE =
		Symbol.getSymbol("if-modified-since");
	private static final Symbol USE_CHCHES =
		Symbol.getSymbol("use-caches");
	private static final Symbol CHUNKED_STREAMING_MODE =
		Symbol.getSymbol("chunked-streaming-mode");
	private static final Symbol FIXED_LENGTH_STREAMING_MODE =
		Symbol.getSymbol("fixed-length-streaming-mode");
	private static final Symbol REQUEST_METHOD =
		Symbol.getSymbol("request-method");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env,
			LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		Map<Datum, Datum> prm;
		HttpURLConnection con;
		String mth = null;

		SubrUtils.checkTerminated(itr, body, mesg);
		if((prm = LispUtils.assocToMap(c2a)) == null) {
			throw mesg.getError("err.require.assoc", c2a);
		}

		if(c1a instanceof LispHttpConnection) {
			con = ((LispHttpConnection)c1a).connection;
			try {
				for(Map.Entry<Datum, Datum> e : prm.entrySet()) {
					if(e.getKey().equals(ALLOW_USER_INTERACTION)) {
						con.setAllowUserInteraction(
								e.getValue().isTrue());
					} else if(e.getKey().equals(IF_MODIFIED_SINCE)) {
						con.setIfModifiedSince(SubrUtils.getLongExact(
								e.getValue(), mesg));
					} else if(e.getKey().equals(USE_CHCHES)) {
						con.setUseCaches(e.getValue().isTrue());
					} else if(e.getKey().equals(
							CHUNKED_STREAMING_MODE)) {
						con.setChunkedStreamingMode(
								SubrUtils.getSmallInt(
										e.getValue(), mesg));
					} else if(e.getKey().equals(
							FIXED_LENGTH_STREAMING_MODE)) {
						int x0 = SubrUtils.getSmallInt(
								e.getValue(), mesg);

						if(x0 < 0) {
							throw mesg.getError(
									"err.require.int.nonnegative",
									e.getValue());
						}
						con.setFixedLengthStreamingMode(x0);
					} else if(e.getKey().equals(REQUEST_METHOD)) {
						if(e.getValue() instanceof Symbol) {
							mth = ((Symbol)e.getValue()
									).getName().toUpperCase();
						} else if(e.getValue() instanceof LispString) {
							mth = e.getValue(
									).getString().toUpperCase();
						} else {
							throw mesg.getError(
									"err.net.httpmethod.invalid",
									e.getValue());
						}
	
						try {
							con.setRequestMethod(mth);
						} catch (ProtocolException e1) {
							throw mesg.getError(
									"err.net.httpmethod.invalid",
									e.getValue());
						}
					} else if(e.getKey() instanceof LispString) {
						if(e.getValue() instanceof LispString) {
							con.setRequestProperty(
									e.getKey().getString(),
									e.getValue().getString());
						} else {
							throw mesg.getError("err.require.string",
									e.getValue());
						}
					} else {
						throw mesg.getError(
								"err.net.httprequestparameter.invalid",
								e.getKey());
					}
				}

				if(mth == null) {
					throw mesg.getError("err.net.protocol.invalid");
				} else if(mth.equals("POST")) {
					con.setDoOutput(true);
				}
				return Undef.UNDEF;
			} catch(IllegalStateException e) {
				throw mesg.getError("err.net.http.alreadyconnected");
			}
		} else {
			throw mesg.getError("err.net.require.httpconnection", c1a);
		}
	}

}
