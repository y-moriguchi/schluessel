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

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/23
 */
public class MIMEDecodeTest extends TC {

	private void dec3(String s, String d) throws MIMEException {
		eq(MIME.decode(d), s);
	}

	public void testEncode3() throws MIMEException {
		dec3("THE IDOLM@STER", "Subject: =?ISO-2022-JP?B?VEhFIElET0xNQFNURVI=?=");
		dec3("IDOLM@STER", "Subject: =?ISO-2022-JP?B?SURPTE1AU1RFUg==?=");
		dec3("IDOLM@STER 2", "Subject: =?ISO-2022-JP?B?SURPTE1AU1RFUiAy?=");
		dec3("天海春香", "Subject: =?ISO-2022-JP?B?GyRCRTczJD1VOWEbKEI=?=");
		dec3("如月千早", "Subject: =?ISO-2022-JP?B?GyRCRyE3bkBpQWEbKEI=?=");
		dec3("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzISI7ZCRIMGw9bxsoQg==?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEtEOkVAISJMXDtYJDckXiQ3JGckJiEqTHNCKyRHJDkbKEI=?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJGghIkxzQisbKEIhIQ==?=");
		dec3("プロデューサーさん,私と一緒に頂点,目指しましょう!約束ですよ,約束!!DomeDesu",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzGyhCLBskQjtkGyhC?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEgwbD1vJEtEOkVAGyhCLBskQkxcO1gkNyReJDckZyQmGyhC?=\r\n" +
				" =?ISO-2022-JP?B?IRskQkxzQiskRyQ5JGgbKEIsGyRCTHNCKxsoQiEhRG9tZURlc3U=?=");
		dec3("プロデューサーさん,私と一緒に頂点,目指しましょう!約束ですよ,約束!!DomeDesuY",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzGyhCLBskQjtkGyhC?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEgwbD1vJEtEOkVAGyhCLBskQkxcO1gkNyReJDckZyQmGyhC?=\r\n" +
				" =?ISO-2022-JP?B?IRskQkxzQiskRyQ5JGgbKEIsGyRCTHNCKxsoQiEhRG9tZURlc3VZ?=");
		dec3("プロデューサーさん,私と一緒に頂点,目指しましょう!約束ですよ,約束!!DomeDesuYo",
				"Subject: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzGyhCLBskQjtkGyhC?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEgwbD1vJEtEOkVAGyhCLBskQkxcO1gkNyReJDckZyQmGyhC?=\r\n" +
				" =?ISO-2022-JP?B?IRskQkxzQiskRyQ5JGgbKEIsGyRCTHNCKxsoQiEhRG9tZURlc3VZ?=\r\n" +
				" =?ISO-2022-JP?B?bw==?=");
		dec3("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"Subje: =?ISO-2022-JP?B?GyRCJVclbSVHJWUhPCU1ITwkNSRzISI7ZCRIMGw9bxsoQg==?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJEtEOkVAISJMXDtYJDckXiQ3JGckJiEqTHNCKyRHJDkbKEI=?=\r\n" +
				" =?ISO-2022-JP?B?GyRCJGghIkxzQisbKEIhIQ==?=");
	}

}
