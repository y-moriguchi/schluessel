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
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class DiscardUDPResponder extends NetworkResponder {

	//
	private static final int DEFAULT_TIMEOUT = 2000;

	/**
	 * 
	 * @param address
	 * @param port
	 */
	public DiscardUDPResponder(InetAddress address, int port) {
		super(address, port);
	}

	/* (non-Javadoc)
	 * @see nettest.Responder#send(java.lang.String)
	 */
	public String send(String s) throws IOException {
		DatagramSocket sok;
		DatagramPacket spk;
		byte[] a = s.getBytes();

		sok = new DatagramSocket();
		sok.setSoTimeout(DEFAULT_TIMEOUT);
		spk = new DatagramPacket(a, a.length, address, port);
		sok.send(spk);
		return null;
	}

}
