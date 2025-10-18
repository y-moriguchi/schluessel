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
package net.morilib.lisp.net;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public final class SubrNetUtils {

	/**
	 * 
	 * @param d1
	 * @param mesg
	 * @return
	 * @throws UnknownHostException
	 */
	public static InetAddress getAddress(Datum d1,
			LispMessage mesg) throws UnknownHostException {
		if(d1 instanceof LispInetAddress) {
			return ((LispInetAddress)d1).getAddress();
		} else if(d1 instanceof LispString) {
			return InetAddress.getByName(d1.getString());
		} else {
			throw mesg.getError(
					"err.net.require.hostnameoraddress", d1);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param body
	 * @param mesg
	 * @return
	 * @throws UnknownHostException
	 */
	public static InetAddress nextAddress(Iterator<Datum> itr,
			Datum body, LispMessage mesg) throws UnknownHostException {
		return getAddress(SubrUtils.nextIf(itr, mesg, body), mesg);
	}

}
