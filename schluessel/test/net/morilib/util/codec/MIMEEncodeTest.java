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
package net.morilib.util.codec;

import java.io.UnsupportedEncodingException;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/23
 */
public class MIMEEncodeTest extends TC {

	private void enc3(String s, String d) {
		try {
			eq(MIME.encodeBase64(s, "Subject:", "ISO-2022-JP"), d);
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	private void enc3_1(String s, String d) {
		try {
			eq(MIME.encodeBase64(s, "Subje:", "ISO-2022-JP"), d);
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	public void testEncode3() {
		enc3("THE IDOLM@STER", "Subject: =?ISO-2022-JP?B?VEhFIElET0xNQFNURVI=?=");
		enc3("IDOLM@STER", "Subject: =?ISO-2022-JP?B?SURPTE1AU1RFUg==?=");
		enc3("IDOLM@STER 2", "Subject: =?ISO-2022-JP?B?SURPTE1AU1RFUiAy?=");
		enc3("天海春香", "Subject: =?ISO-2022-JP?B?GyRCRTczJD1VOWEbKEI=?=");
		enc3("如月千早", "Subject: =?ISO-2022-JP?B?GyRCRyE3bkBpQWEbKEI=?=");
		enc3("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzISI7ZCRIMGw9bxsoQg==?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEtEOkVAISJMXDtYJDckXiQ3JGckJiEqTHNCKyRHJDkbKEI=?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJGghIkxzQisbKEIhIQ==?=");
		enc3("プロデューサーさん,私と一緒に頂点,目指しましょう!約束ですよ,約束!!DomeDesu",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzGyhCLBskQjtkGyhC?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEgwbD1vJEtEOkVAGyhCLBskQkxcO1gkNyReJDckZyQmGyhC?=\r\n" +
				" =?ISO-2022-JP?B?IRskQkxzQiskRyQ5JGgbKEIsGyRCTHNCKxsoQiEhRG9tZURlc3U=?=");
		enc3("プロデューサーさん,私と一緒に頂点,目指しましょう!約束ですよ,約束!!DomeDesuY",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzGyhCLBskQjtkGyhC?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEgwbD1vJEtEOkVAGyhCLBskQkxcO1gkNyReJDckZyQmGyhC?=\r\n" +
				" =?ISO-2022-JP?B?IRskQkxzQiskRyQ5JGgbKEIsGyRCTHNCKxsoQiEhRG9tZURlc3VZ?=");
		enc3("プロデューサーさん,私と一緒に頂点,目指しましょう!約束ですよ,約束!!DomeDesuYo",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzGyhCLBskQjtkGyhC?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEgwbD1vJEtEOkVAGyhCLBskQkxcO1gkNyReJDckZyQmGyhC?=\r\n" +
				" =?ISO-2022-JP?B?IRskQkxzQiskRyQ5JGgbKEIsGyRCTHNCKxsoQiEhRG9tZURlc3VZ?=\r\n" +
				" =?ISO-2022-JP?B?bw==?=");
		enc3_1("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"Subje: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzISI7ZCRIMGw9bxsoQg==?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEtEOkVAISJMXDtYJDckXiQ3JGckJiEqTHNCKyRHJDkbKEI=?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJGghIkxzQisbKEIhIQ==?=");
	}

}
