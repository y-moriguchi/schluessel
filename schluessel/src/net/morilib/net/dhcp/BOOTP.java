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
package net.morilib.net.dhcp;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/19
 */
public final class BOOTP {

	//
	static final byte BOOTREQUEST = 1;
	static final byte BOOTREPLY = 2;
	static final byte ETHERNET_10MB = 1;
	static final byte HARDWARE_ADDRESS_LENGTH = 6;
	static final int  BOOTP_SERVER_PORT = 67;
	static final int  BOOTP_CLIENT_PORT = 68;
	static final int  BUFFER_SIZE = 300;
	static final int  TIMEOUT = 4000;

	//
	private static void fill(DataOutputStream dos,
			int bytes, byte fill) throws IOException {
		for(int i = 0; i < bytes; i++)  dos.writeByte(fill);
	}

	//
	static ByteArrayOutputStream createCommonFormat(
			byte[] hwaddr, InetAddress ciaddr, short secs, int[] xid) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(bos);

		xid[0] = (int)(Math.random() * Integer.MAX_VALUE);
		if(hwaddr.length != HARDWARE_ADDRESS_LENGTH) {
			throw new IllegalArgumentException();
		} else if(ciaddr != null && ciaddr.getAddress().length != 4) {
			throw new IllegalArgumentException();
		} else try {
			dos.writeByte(BOOTREQUEST);  // OP
			dos.writeByte(ETHERNET_10MB);  // htype
			dos.writeByte(HARDWARE_ADDRESS_LENGTH);  // hlen
			dos.writeByte(0);  // hops
			dos.writeInt(xid[0]); // xid
			dos.writeShort(secs);  // secs
			dos.writeShort(0);  // flags(DHCP)
			if(ciaddr == null) {  // client IP address
				dos.writeInt(0);
			} else {
				dos.write(ciaddr.getAddress());
			}
			dos.writeInt(0);  // 'your' IP address
			dos.writeInt(0);  // server IP address
			dos.writeInt(0);  // gateway IP address
			dos.write(hwaddr);  // hardware address
			fill(dos, 64, (byte)0);  // server name
			fill(dos, 128, (byte)0);  // file name
			return bos;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	static byte[] createBroadcastFormat(
			byte[] hwaddr, InetAddress ciaddr, short secs, int[] xid) {
		ByteArrayOutputStream bos =
			createCommonFormat(hwaddr, ciaddr, secs, xid);
		DataOutputStream dos = new DataOutputStream(bos);

		try {
			fill(dos, 64, (byte)0);  // vendor specific area
			return bos.toByteArray();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	static BOOTPResult parseCommonFormat(byte[] format, int xid,
			ByteArrayInputStream[] bis0) {
		ByteArrayInputStream bis = new ByteArrayInputStream(format);
		DataInputStream dis = new DataInputStream(bis);
		BOOTPResult r = new BOOTPResult();
		StringBuffer b = new StringBuffer();
		byte[] str = new byte[128], ip = new byte[4];
		int yip, sip;

		try {
			if(dis.readByte() != BOOTREPLY)  return null;
			dis.readByte();
			dis.readByte();
			dis.readByte();
			if(dis.readInt() != xid)  return null;
			dis.readShort();
			dis.readShort();
			dis.readInt();
			yip = dis.readInt();
			sip = dis.readInt();
			dis.readInt();
			dis.readLong();  dis.readLong();  // hardware address
			if(dis.read(str, 0,  64) !=  64)  return null;
			if(dis.read(str, 0, 128) != 128)  return null;

			for(int i = 0; i < str.length; i++) {
				if(str[i] == '\0')  break;
				b.append((char)str[i]);
			}
			r.bootFilePath = b.toString();
			ip[0] = (byte)((yip >> 24) & 0xff);
			ip[1] = (byte)((yip >> 24) & 0xff);
			ip[2] = (byte)((yip >> 24) & 0xff);
			ip[3] = (byte)((yip >> 24) & 0xff);
			r.myAddress = InetAddress.getByAddress(ip);
			ip[0] = (byte)((sip >> 24) & 0xff);
			ip[1] = (byte)((sip >> 24) & 0xff);
			ip[2] = (byte)((sip >> 24) & 0xff);
			ip[3] = (byte)((sip >> 24) & 0xff);
			r.serverAddress = InetAddress.getByAddress(ip);

			if(bis0 != null)  bis0[0] = bis;
			return r;
		} catch (EOFException e) {
			return null;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * @param hwaddr
	 * @param secs
	 * @return
	 * @throws IOException
	 */
	public static BOOTPResult request(
			InetAddress ipaddr,
			InetAddress ciaddr,
			byte[] hwaddr,
			short secs) throws IOException {
		DatagramSocket svs = null, cls = null;
		DatagramPacket svp, clp;
		InetAddress brd;
		byte[] buf, bur = new byte[BUFFER_SIZE];
		int[] xid = new int[1];

		try {
			brd = ipaddr;
			svs = new DatagramSocket();
			cls = new DatagramSocket(BOOTP_CLIENT_PORT);
			svs.connect(brd, BOOTP_SERVER_PORT);
			buf = createBroadcastFormat(hwaddr, ciaddr, secs, xid);
			svp = new DatagramPacket(
					buf, buf.length, brd, BOOTP_SERVER_PORT);
			svs.setBroadcast(true);
			cls.setBroadcast(true);

			clp = new DatagramPacket(bur, bur.length);
			cls.setSoTimeout(TIMEOUT);

			svs.send(svp);
			cls.receive(clp);
			return parseCommonFormat(clp.getData(), xid[0], null);
		} finally {
			if(svs != null)  svs.close();
			if(cls != null)  cls.close();
		}
	}

}
