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
import net.morilib.util.codec.Base64Decoder;
import net.morilib.util.codec.Base64Encoder;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/19
 */
public class Base64Test extends TC {

	private void enc(String s, String d) {
		eq(Base64Encoder.STANDARD.encode(s.getBytes()), d);
	}

	private void enc2(String s, String d) {
		eq(Base64Encoder.RE.encode(s.getBytes()), d);
	}

	private void dec(String s, String d) throws Exception {
		eq(Base64Decoder.STANDARD.decode(d), s.getBytes());
	}

	private void dec2(String s, String d) throws Exception {
		eq(Base64Decoder.RE.decode(d), s.getBytes());
	}

	public void testEncode1() {
		enc("THE IDOLM@STER", "VEhFIElET0xNQFNURVI=");
		enc("IDOLM@STER", "SURPTE1AU1RFUg==");
		enc("IDOLM@STER 2", "SURPTE1AU1RFUiAy");
		enc("天海春香", "5aSp5rW35pil6aaZ");
		enc("如月千早", "5aaC5pyI5Y2D5pep");
		enc("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"44OX44Ot44OH44Ol44O844K144O844GV44KT44CB56eB44Go5LiA57eS44Gr6aCC\r\n" +
				"54K544CB55uu5oyH44GX44G+44GX44KH44GG77yB57SE5p2f44Gn44GZ44KI44CB\r\n" +
				"57SE5p2fISE=");
		enc("三浦あずさ 紹介: 心も体をビッグサイズ！？天然系セクシーお姉さん！",
				"5LiJ5rWm44GC44Ga44GVIOe0ueS7izog5b+D44KC5L2T44KS44OT44OD44Kw44K1\r\n" +
				"44Kk44K677yB77yf5aSp54S257O744K744Kv44K344O844GK5aeJ44GV44KT77yB\r\n");
		enc("菊地真", "6I+K5Zyw55yf");
		eq(Base64Encoder.STANDARD.encode(new byte[]{ (byte)0xff, (byte)0xff, (byte)0xff}),
				"////");
	}

	public void testEncode2() {
		enc2("THE IDOLM@STER", "VEhFIElET0xNQFNURVI");
		enc2("IDOLM@STER", "SURPTE1AU1RFUg");
		enc2("IDOLM@STER 2", "SURPTE1AU1RFUiAy");
		enc2("天海春香", "5aSp5rW35pil6aaZ");
		enc2("如月千早", "5aaC5pyI5Y2D5pep");
		enc2("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"44OX44Ot44OH44Ol44O844K144O844GV44KT44CB56eB44Go5LiA57eS44Gr6aCC" +
				"54K544CB55uu5oyH44GX44G!44GX44KH44GG77yB57SE5p2f44Gn44GZ44KI44CB" +
				"57SE5p2fISE");
		enc2("三浦あずさ 紹介: 心も体をビッグサイズ！？天然系セクシーお姉さん！",
				"5LiJ5rWm44GC44Ga44GVIOe0ueS7izog5b!D44KC5L2T44KS44OT44OD44Kw44K1" +
				"44Kk44K677yB77yf5aSp54S257O744K744Kv44K344O844GK5aeJ44GV44KT77yB");
		enc2("菊地真", "6I!K5Zyw55yf");
		eq(Base64Encoder.RE.encode(new byte[]{ (byte)0xff, (byte)0xff, (byte)0xff}),
				"----");
	}

	public void testDecode1() throws Exception {
		dec("THE IDOLM@STER", "VEhFIElET0xNQFNURVI=");
		dec("IDOLM@STER", "SURPTE1AU1RFUg==");
		dec("IDOLM@STER 2", "SURPTE1AU1RFUiAy");
		dec("天海春香", "5aSp5rW35pil6aaZ");
		dec("如月千早", "5aaC5pyI5Y2D5pep");
		dec("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"44OX44Ot44OH44Ol44O844K144O844GV44KT44CB56eB44Go5LiA57eS44Gr6aCC\r\n" +
				"54K544CB55uu5oyH44GX44G+44GX44KH44GG77yB57SE5p2f44Gn44GZ44KI44CB\r\n" +
				"57SE5p2fISE=");
		dec("三浦あずさ 紹介: 心も体をビッグサイズ！？天然系セクシーお姉さん！",
				"5LiJ5rWm44GC44Ga44GVIOe0ueS7izog5b+D44KC5L2T44KS44OT44OD44Kw44K1\r\n" +
				"44Kk44K677yB77yf5aSp54S257O744K744Kv44K344O844GK5aeJ44GV44KT77yB\r\n");
		dec("菊地真", "6I+K5Zyw55yf");
	}

	public void testDecode2() throws Exception {
		dec2("THE IDOLM@STER", "VEhFIElET0xNQFNURVI");
		dec2("IDOLM@STER", "SURPTE1AU1RFUg");
		dec2("IDOLM@STER 2", "SURPTE1AU1RFUiAy");
		dec2("天海春香", "5aSp5rW35pil6aaZ");
		dec2("如月千早", "5aaC5pyI5Y2D5pep");
		dec2("プロデューサーさん、私と一緒に頂点、目指しましょう！約束ですよ、約束!!",
				"44OX44Ot44OH44Ol44O844K144O844GV44KT44CB56eB44Go5LiA57eS44Gr6aCC" +
				"54K544CB55uu5oyH44GX44G!44GX44KH44GG77yB57SE5p2f44Gn44GZ44KI44CB" +
				"57SE5p2fISE");
		dec2("三浦あずさ 紹介: 心も体をビッグサイズ！？天然系セクシーお姉さん！",
				"5LiJ5rWm44GC44Ga44GVIOe0ueS7izog5b!D44KC5L2T44KS44OT44OD44Kw44K1" +
				"44Kk44K677yB77yf5aSp54S257O744K744Kv44K344O844GK5aeJ44GV44KT77yB");
		dec2("菊地真", "6I!K5Zyw55yf");
		eq(Base64Encoder.RE.encode(new byte[]{ (byte)0xff, (byte)0xff, (byte)0xff}),
				"----");
	}

}
