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
package net.morilib.net.misc;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class DiscardTCPResponder extends NetworkResponder {

	//
	private static final int DEFAULT_TIMEOUT = 2000;

	/**
	 * 
	 * @param address
	 * @param port
	 */
	public DiscardTCPResponder(InetAddress address, int port) {
		super(address, port);
	}

	/* (non-Javadoc)
	 * @see nettest.Responder#send(java.lang.String)
	 */
	public String send(String s) throws IOException {
		Socket sok = null;
		OutputStream ous = null;

		try {
			sok = new Socket(address, port);
			sok.setSoTimeout(DEFAULT_TIMEOUT);
			ous = sok.getOutputStream();
			ous.write(s.getBytes());
			return null;
		} finally {
			if(ous != null)  ous.close();
			if(sok != null)  sok.close();
		}
	}

}
