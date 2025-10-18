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

import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/29
 */
public class RIPRouteInfo {

	//
	int addressIdentifier;
	int routeTag;
	InetAddress address;
	InetAddress mask;
	int nextHop;
	int metric;

	/**
	 * @return the addressIdentifier
	 */
	public int getAddressIdentifier() {
		return addressIdentifier;
	}

	/**
	 * @return the routeTag
	 */
	public int getRouteTag() {
		return routeTag;
	}

	/**
	 * @return the address
	 */
	public InetAddress getAddress() {
		return address;
	}

	/**
	 * @return the mask
	 */
	public InetAddress getMask() {
		return mask;
	}

	/**
	 * @return the nextHop
	 */
	public int getNextHop() {
		return nextHop;
	}

	/**
	 * @return the metric
	 */
	public int getMetric() {
		return metric;
	}

}
