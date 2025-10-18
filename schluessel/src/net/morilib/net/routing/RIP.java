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
package net.morilib.net.routing;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/29
 */
public final class RIP {

	//
	static final int DEFAULT_PORT = 520;
	static final int TIMEOUT = 5000;
	static final byte REQUEST  = 1;
	static final byte RESPONSE = 2;
	static final short ADDRESS_FAMILY_INETV4 = 2;

	//
	private RIP() {}

	//
	private static InetAddress getByAddress(
			byte[] b) throws UnknownHostException {
		if(b[0] == 0 && b[1] == 0 && b[2] == 0 && b[3] == 0) {
			return null;
		} else {
			return InetAddress.getByAddress(b);
		}
	}

	//
	static RIPRouteInfo parseInfo(DataInputStream dis) {
		RIPRouteInfo r = new RIPRouteInfo();
		byte[] b = new byte[4];

		try {
			try {
				r.addressIdentifier = dis.readShort();
			} catch(EOFException e) {
				return null;
			}
			r.routeTag = dis.readShort();
			dis.read(b);  r.address = getByAddress(b);
			dis.read(b);  r.mask = getByAddress(b);
			r.nextHop = dis.readInt();
			if((r.metric = dis.readInt()) > 15)  r.metric = -1;
			return r;
		} catch (IOException e) {
			throw new RuntimeException();
		}
	}

	//
	static RIPRouteInfo[] parsePacket(byte[] b) {
		ByteArrayInputStream bis = new ByteArrayInputStream(b);
		DataInputStream dis = new DataInputStream(bis);
		List<RIPRouteInfo> res = new ArrayList<RIPRouteInfo>();
		RIPRouteInfo r0;

		try {
			if(dis.readByte() != RESPONSE)  return null;  // command
			dis.readByte();  // version
			dis.readShort();  // 0
			while((r0 = parseInfo(dis)) != null) {
				if(r0.metric != 0)  res.add(r0);
			}
			return res.toArray(new RIPRouteInfo[0]);
		} catch (IOException e) {
			throw new RuntimeException();
		}
		
	}

	/**
	 * 
	 * @param address
	 * @param version
	 * @return
	 * @throws IOException
	 */
	public static RIPRouteInfo[] request(InetAddress address,
			int version,
			InetAddress... route) throws IOException {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(bos);
		DatagramSocket sok;
		DatagramPacket pkt, pkr;
		byte[] b, d;

		if(version < 1 || version > 2) {
			throw new IllegalArgumentException();
		} else if(address.getAddress().length != 4) {
			throw new IllegalArgumentException();
		} else if(route.length >= 25) {
			throw new IllegalArgumentException();
		}

		try {
			dos.writeByte(REQUEST);
			dos.writeByte(version);
			dos.writeShort(0);
			for(InetAddress r : route) {
				dos.writeShort(ADDRESS_FAMILY_INETV4);
				dos.writeShort(0);
				dos.write(r.getAddress());
				dos.writeInt(0);
				dos.writeInt(0);
				dos.writeInt(0);
			}
		} catch (IOException e) {
			throw new RuntimeException();
		}

		sok = new DatagramSocket();
		sok.setSoTimeout(TIMEOUT);
		b   = bos.toByteArray();
		pkt = new DatagramPacket(b, b.length, address, DEFAULT_PORT);
		d   = new byte[4 + 20 * 25];
		pkr = new DatagramPacket(d, d.length);

		sok.send(pkt);
		sok.receive(pkr);
		return parsePacket(pkr.getData());
	}

}
