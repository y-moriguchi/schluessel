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

import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/19
 */
public class BOOTPResult {

	//
	InetAddress myAddress, serverAddress;
	String bootFilePath;

	/**
	 * @return the myAddress
	 */
	public InetAddress getMyAddress() {
		return myAddress;
	}

	/**
	 * @return the serverAddress
	 */
	public InetAddress getServerAddress() {
		return serverAddress;
	}

	/**
	 * @return the bootFilePath
	 */
	public String getBootFilePath() {
		return bootFilePath;
	}

}
