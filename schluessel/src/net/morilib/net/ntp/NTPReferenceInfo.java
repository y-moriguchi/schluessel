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

import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public class NTPReferenceInfo {

	public static class Unknown extends NTPReferenceInfo {

		//
		private String source;

		/**
		 * 
		 * @param layer
		 */
		public Unknown(int layer, String source) {
			super(layer);
			this.source = source;
		}

		/* (non-Javadoc)
		 * @see nettest.NTPReferenceInfo#getSourceCode()
		 */
		@Override
		public String getSourceCode() {
			return source;
		}

	}

	public static class Primary extends NTPReferenceInfo {

		//
		private String source;

		/**
		 * 
		 * @param layer
		 */
		public Primary(String source) {
			super(1);
			this.source = source;
		}

		/* (non-Javadoc)
		 * @see nettest.NTPReferenceInfo#getSourceCode()
		 */
		@Override
		public String getSourceCode() {
			return source;
		}

	}

	public static class Secondary extends NTPReferenceInfo {

		//
		private InetAddress source;

		/**
		 * 
		 * @param layer
		 */
		public Secondary(int layer, InetAddress source) {
			super(layer);
			this.source = source;
		}

		/* (non-Javadoc)
		 * @see nettest.NTPReferenceInfo#getSourceAddress()
		 */
		@Override
		public InetAddress getSourceAddress() {
			return source;
		}

	}

	//
	private int layer;

	/**
	 * 
	 * @param layer
	 */
	public NTPReferenceInfo(int layer) {
		this.layer = layer;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isValid() {
		return layer > 0;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isPrimary() {
		return layer == 1;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isSecondary() {
		return layer > 1 && layer <= 15;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isReserved() {
		return layer >= 16;
	}

	/**
	 * 
	 * @return
	 */
	public int getLayer() {
		return (layer > 0 && layer <= 15) ? layer : -1;
	}

	/**
	 * 
	 * @return
	 */
	public String getSourceCode() {
		return null;
	}

	/**
	 * 
	 * @return
	 */
	public InetAddress getSourceAddress() {
		return null;
	}

}
