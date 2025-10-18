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
package net.morilib.net.snmp;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

import net.morilib.util.Tuple2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public final class SNMP {

	//
	private static final byte[] REQ_HEADER_1 = new byte[] {
		0x02, 0x01, 0x00
	};

	//
	private static final byte[] REQ_HEADER_2 = new byte[] {
		0x02, 0x01, 72, 0x02, 0x01, 0, 0x02, 0x01, 0
	};

	//
	private static final byte GET_REQUEST = (byte)0xa0;
	private static final byte GET_NEXT    = (byte)0xa1;

	//
	static byte[] lengthToBytes(int l) {
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		int c = l;

		if((c & 0xf0000000) != 0)  b.write((c >> 28) | 0x80);
		if((c & 0x0fe00000) != 0)  b.write((c >> 21) | 0x80);
		if((c & 0x001fc000) != 0)  b.write((c >> 14) | 0x80);
		if((c & 0x00003f80) != 0)  b.write((c >>  7) | 0x80);
		b.write(c & 0x7f);
		return b.toByteArray();
	}

	//
	private static void writeString(ByteArrayOutputStream bos,
			String s) throws IOException {
		byte[] b = s.getBytes();

		bos.write(0x04);
		bos.write(lengthToBytes(b.length));
		bos.write(b);
	}

	//
	private static void writeOID(ByteArrayOutputStream b2,
			ObjectIdentifier oid) throws IOException {
		ByteArrayOutputStream b1 = new ByteArrayOutputStream();
		byte[] s;

		s = oid.toBytes();
		b1.write(6);
		b1.write(lengthToBytes(s.length));
		b1.write(s);
		b1.write(5);  b1.write(0);   // NULL

		s = b1.toByteArray();
		b2.write(0x30);
		b2.write(lengthToBytes(s.length));
		b2.write(s);
	}

	//
	private static byte[] createGetRequest(ObjectIdentifier oid) {
		ByteArrayOutputStream b2 = new ByteArrayOutputStream();
		ByteArrayOutputStream b3 = new ByteArrayOutputStream();
		byte[] s;

		try {
			writeOID(b2, oid);
			s = b2.toByteArray();
			b3.write(REQ_HEADER_2);
			b3.write(0x30);
			b3.write(lengthToBytes(s.length));
			b3.write(s);
			return b3.toByteArray();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	private static byte[] createGetRequestHeader(
			byte req, String community, ObjectIdentifier oid) {
		ByteArrayOutputStream b1 = new ByteArrayOutputStream();
		ByteArrayOutputStream b2 = new ByteArrayOutputStream();
		byte[] s;

		try {
			b1.write(REQ_HEADER_1);
			writeString(b1, community);
			s = createGetRequest(oid);
			b1.write(req);
			b1.write(lengthToBytes(s.length));
			b1.write(s);

			s = b1.toByteArray();
			b2.write(0x30);
			b2.write(lengthToBytes(s.length));
			b2.write(s);
			return b2.toByteArray();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	private static SNMPPDU toPDU(Object o) throws ASN1Exception {
		SNMPPDU r = new SNMPPDU();
		List<?> l1, l2, l3, l4;
		List<Tuple2<ObjectIdentifier, Object>> ll;

		try {
//			System.out.println(o);
			l1 = (List<?>)o;
			r.setVersion(((Number)l1.get(0)).intValue());
			r.setCommunity((String)l1.get(1));

			l2 = (List<?>)l1.get(2);
			r.setId(((Number)l2.get(0)).intValue());
			r.setErrorCode(((Number)l2.get(1)).intValue());
			r.setErrorIndex(((Number)l2.get(2)).intValue());

			l3 = (List<?>)l2.get(3);
			ll = new ArrayList<Tuple2<ObjectIdentifier, Object>>();
			for(Object p : l3) {
				l4 = (List<?>)p;
				ll.add(new Tuple2<ObjectIdentifier, Object>(
						(ObjectIdentifier)l4.get(0), l4.get(1)));
			}
			r.setVariables(ll);
			return r;
		} catch(IndexOutOfBoundsException e) {
			throw new ASN1Exception(e);
		} catch(ClassCastException e) {
			throw new ASN1Exception(e);
		}
	}

	/**
	 * 
	 * @param req
	 * @param addr
	 * @param port
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU get(byte req, InetAddress addr,
			int port, String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		DatagramSocket sok = null;
		DatagramPacket pkt, lsk;
		ByteArrayInputStream bis;
		DataInputStream din;
		byte[] s, t;

		try {
			s   = createGetRequestHeader(req, community, oid);
			t   = new byte[8192];
			pkt = new DatagramPacket(s, s.length, addr, port);
			lsk = new DatagramPacket(t, t.length);
			sok = new DatagramSocket();
			sok.setSoTimeout(5000);

			sok.send(pkt);
			sok.receive(lsk);
			bis = new ByteArrayInputStream(lsk.getData());
			din = new DataInputStream(bis);
			return toPDU(ASN1.read(din));
		} finally {
			if(sok != null) {
				sok.close();
			}
		}
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getRequest(InetAddress addr, int port,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return get(GET_REQUEST, addr, port, community, oid);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getRequest(String host, int port,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return getRequest(InetAddress.getByName(host), port, community,
				oid);
	}

	/**
	 * 
	 * @param addr
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getRequest(InetAddress addr,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return getRequest(addr, 161, community, oid);
	}

	/**
	 * 
	 * @param host
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getRequest(String host,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return getRequest(InetAddress.getByName(host), 161, community,
				oid);
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getNext(InetAddress addr, int port,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return get(GET_NEXT, addr, port, community, oid);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getNext(String host, int port,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return getNext(InetAddress.getByName(host), port, community,
				oid);
	}

	/**
	 * 
	 * @param addr
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getNext(InetAddress addr,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return getNext(addr, 161, community, oid);
	}

	/**
	 * 
	 * @param host
	 * @param community
	 * @param oid
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static SNMPPDU getNext(String host,
			String community,
			ObjectIdentifier oid) throws IOException, ASN1Exception {
		return getNext(InetAddress.getByName(host), 161, community,
				oid);
	}

}
