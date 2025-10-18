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
package net.morilib.lisp.net.mail;

import java.io.IOException;
import java.net.ProtocolException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.net.LispInetAddress;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.mail.SMTP;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/24
 */
public class SendMail extends Subr {

	//
	/*package*/ static String defaultEncoding;

	static {
		String lang = Locale.getDefault().getLanguage();

		if(lang.equals(Locale.JAPANESE.getLanguage())) {
			defaultEncoding = "ISO-2022-JP";
		} else if(lang.equals(Locale.ENGLISH.getLanguage())) {
			defaultEncoding = null;
		} else {
			defaultEncoding = null;
		}
	}

	//
	private List<String> tolist(Datum b, LispMessage mesg) {
		ConsIterator i2 = new ConsIterator(b);
		List<String> s = new ArrayList<String>();

		while(i2.hasNext()) {
			s.add(SubrUtils.getString(i2.next(), mesg));
		}

		if(!i2.rest().isNil()) {
			throw mesg.getError("err.list", b);
		} else {
			return s;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum host = SubrUtils.nextIf(itr, mesg, body);
		Datum d2 = SubrUtils.nextIf(itr, mesg, body), d3;
		int port;
		String from, text, subj;
		List<String> to, cc, bcc;

		if(d2 instanceof LispSmallInt) {
			port = SubrUtils.getSmallInt(d2, mesg);
			d3 = SubrUtils.nextIf(itr, mesg, body);
		} else {
			port = 25;
			d3 = d2;
		}
		from = SubrUtils.getString(d3, mesg);
		to   = tolist(SubrUtils.nextIf(itr, mesg, body), mesg);
		cc   = tolist(SubrUtils.nextIf(itr, mesg, body), mesg);
		bcc  = tolist(SubrUtils.nextIf(itr, mesg, body), mesg);
		subj = SubrUtils.nextString(itr, mesg, body);
		text = SubrUtils.nextString(itr, mesg, body);
		SubrUtils.checkTerminated(itr, body, mesg);

		try {
			if(host instanceof LispString) {
				SMTP.send(host.getString(), port, subj, from,
						to, cc, bcc, text, defaultEncoding);
			} else if(host instanceof LispInetAddress) {
				SMTP.send(((LispInetAddress)host).getAddress(),
						port, subj, from,
						to, cc, bcc, text, defaultEncoding);
			}
		} catch (ProtocolException e) {
			throw mesg.getError("err.net.smtp", e.getMessage());
		} catch (IOException e) {
			throw mesg.getError("err.io");
		}
		return Undef.UNDEF;
	}

}
