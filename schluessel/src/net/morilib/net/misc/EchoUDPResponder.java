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
import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class EchoUDPResponder extends NetworkResponder {

	/**
	 * 
	 * @param addr
	 * @param port
	 */
	public EchoUDPResponder(InetAddress address, int port) {
		super(address, port);
	}

	/* (non-Javadoc)
	 * @see nettest.Responder#send(java.lang.String)
	 */
	public String send(String s) throws IOException {
		return Echo.sendByUDP(address, port, s);
	}

}
