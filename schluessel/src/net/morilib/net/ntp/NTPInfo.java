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
package net.morilib.net.ntp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public class NTPInfo {

	//
	private NTPLeapIndicator leapIndicator = NTPLeapIndicator.NORMAL;
	private byte version;
	private NTPMode mode;
	private NTPReferenceInfo referenceInfo;
	private int pollingInterval = 0;
	private int precision = 0;
	private int routeDelay = 0;
	private int routeDispersion = 0;
	private NTPTimestamp referenceTime;
	private NTPTimestamp originateTime;
	private NTPTimestamp receiveTime;
	private NTPTimestamp transmitTime;
	private int keyIdentifier;
	private byte[] messageDigest;
	private NTPTimestamp originateLocalTime;

	/**
	 * 
	 * @return
	 */
	public NTPLeapIndicator getLeapIndicator() {
		return leapIndicator;
	}

	/**
	 * 
	 * @param leapSecond
	 */
	public void setLeapIndicator(NTPLeapIndicator leapIndicator) {
		this.leapIndicator = leapIndicator;
	}

	/**
	 * 
	 * @return
	 */
	public byte getVersion() {
		return version;
	}

	/**
	 * 
	 * @param version
	 */
	public void setVersion(byte version) {
		this.version = version;
	}

	/**
	 * 
	 * @return
	 */
	public NTPMode getMode() {
		return mode;
	}

	/**
	 * 
	 * @param mode
	 */
	public void setMode(NTPMode mode) {
		this.mode = mode;
	}

	/**
	 * 
	 * @return
	 */
	public NTPReferenceInfo getReferenceInfo() {
		return referenceInfo;
	}

	/**
	 * 
	 * @param layer
	 */
	public void setReferenceInfo(NTPReferenceInfo referenceInfo) {
		this.referenceInfo = referenceInfo;
	}

	/**
	 * 
	 * @return
	 */
	public int getPollingInterval() {
		return pollingInterval;
	}

	/**
	 * 
	 * @param pollingInterval
	 */
	public void setPollingInterval(int pollingInterval) {
		this.pollingInterval = pollingInterval;
	}

	/**
	 * 
	 * @return
	 */
	public int getPrecision() {
		return precision;
	}

	/**
	 * 
	 * @param precision
	 */
	public void setPrecision(int precision) {
		this.precision = precision;
	}

	/**
	 * 
	 * @return
	 */
	public int getRouteDelay() {
		return routeDelay;
	}

	/**
	 * 
	 * @param routeDelay
	 */
	public void setRouteDelay(int routeDelay) {
		this.routeDelay = routeDelay;
	}

	/**
	 * 
	 * @return
	 */
	public int getRouteDispersion() {
		return routeDispersion;
	}

	/**
	 * 
	 * @param routeDispersion
	 */
	public void setRouteDispersion(int routeDispersion) {
		this.routeDispersion = routeDispersion;
	}

	/**
	 * 
	 * @return
	 */
	public NTPTimestamp getReferenceTime() {
		return referenceTime;
	}

	/**
	 * 
	 * @param refernceTime
	 */
	public void setReferenceTime(NTPTimestamp referenceTime) {
		this.referenceTime = referenceTime;
	}

	/**
	 * 
	 * @return
	 */
	public NTPTimestamp getOriginateTime() {
		return originateTime;
	}

	/**
	 * 
	 * @param originateTime
	 */
	public void setOriginateTime(NTPTimestamp originateTime) {
		this.originateTime = originateTime;
	}

	/**
	 * 
	 * @return
	 */
	public NTPTimestamp getReceiveTime() {
		return receiveTime;
	}

	/**
	 * 
	 * @param receiveTime
	 */
	public void setReceiveTime(NTPTimestamp receiveTime) {
		this.receiveTime = receiveTime;
	}

	/**
	 * 
	 * @return
	 */
	public NTPTimestamp getTransmitTime() {
		return transmitTime;
	}

	/**
	 * 
	 * @param transmitTime
	 */
	public void setTransmitTime(NTPTimestamp transmitTime) {
		this.transmitTime = transmitTime;
	}

	/**
	 * 
	 * @return
	 */
	public int getKeyIdentifier() {
		return keyIdentifier;
	}

	/**
	 * 
	 * @param keyIdentifier
	 */
	public void setKeyIdentifier(int keyIdentifier) {
		this.keyIdentifier = keyIdentifier;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] getMessageDigest() {
		byte[] b = new byte[messageDigest.length];

		System.arraycopy(messageDigest, 0, b, 0, b.length);
		return messageDigest;
	}

	/**
	 * 
	 * @param messageDigest
	 */
	public void setMessageDigest(byte[] messageDigest) {
		byte[] b = new byte[messageDigest.length];

		System.arraycopy(messageDigest, 0, b, 0, b.length);
		this.messageDigest = b;
	}

	/**
	 * @return the originateLocalTime
	 */
	public NTPTimestamp getOriginateLocalTime() {
		return originateLocalTime;
	}

	/**
	 * @param originateLocalTime the originateLocalTime to set
	 */
	public void setOriginateLocalTime(NTPTimestamp t) {
		this.originateLocalTime = t;
	}

}
