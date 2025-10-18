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
package net.morilib.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.test.TC;
import net.morilib.parser.json.JSONParseException;
import net.morilib.parser.json.JSONParser;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/21
 */
public class JSONTest extends TC {

	//
	private static void er(String s) {
		try {
			JSONParser.parseExact(s);
			fail();
		} catch(JSONParseException e) {}
	}

	//
	private static void ep(String s, Object t) throws Exception {
		eq(JSONParser.parseExact(s), t);
	}

	//
	private static void eq(String s, Object t) throws Exception {
		eq(JSONParser.parseInexact(s), t);
	}

	//
	private static List<Object> arr(Object... objects) {
		return new ArrayList<Object>(Arrays.asList(objects));
	}

	//
	private static Map<Object, Object> map(Object... objects) {
		Map<Object, Object> m = new HashMap<Object, Object>();

		for(int i = 0; i < objects.length; i += 2) {
			m.put(objects[i], objects[i + 1]);
		}
		return m;
	}

	//
	private static void en(String s) throws Exception {
		assertNull(JSONParser.parseExact(s));
	}

	public void testString() throws Exception {
		ep("\"aA0あ\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0075\\u01Ad\"",
				"aA0あ\"\\/\u0008\u000c\n\r\tu\u01ad");
		ep("\"\"", "");
		er("\"");
		er("\"\\u000");
		er("\"\\u000g\"");
		er("\"\\g\"");
	}

	public void testInteger() throws Exception {
		ep("1", BigInteger.valueOf(1));
		ep("0", BigInteger.valueOf(0));
		ep("-1", BigInteger.valueOf(-1));
		ep("123456789012345678901234567890",
				new BigInteger("123456789012345678901234567890"));
		er("-");
	}

	public void testFraction() throws Exception {
		ep("1.1", new BigDecimal("1.1"));
		ep("-1.1", new BigDecimal("-1.1"));
		ep("11.11", new BigDecimal("11.11"));
		ep("-11.11", new BigDecimal("-11.11"));
		ep(".1", new BigDecimal("0.1"));
		ep("-.1", new BigDecimal("-0.1"));
		er("1.");
		er("-1.");
	}

	public void testExp() throws Exception {
		ep("1e2", new BigDecimal("1e2"));
		ep("-1e2", new BigDecimal("-1e2"));
		ep("1e10", new BigDecimal("1e10"));
		ep("-1e10", new BigDecimal("-1e10"));
		ep("1e+10", new BigDecimal("1e10"));
		ep("1e-10", new BigDecimal("0.0000000001"));
		ep("1.1e+10", new BigDecimal("1.1e+10"));
		ep("11.11e+10", new BigDecimal("11.11e+10"));
		ep(".1e+10", new BigDecimal("1e9"));
		er("e1");
		er("e+1");
		er("e-1");
		er(".e+1");
		er("1.1e");
		er("1.1e+1a");
	}

	public void testLiteral() throws Exception {
		ep("true", Boolean.TRUE);
		ep("false", Boolean.FALSE);
		en("null");
		er("error");
	}

	public void testArray() throws Exception {
		eq("[1, \"aaa\", 2.3]", arr(1l, "aaa", 2.3));
		eq("[]", arr());
		eq("[ 1, \"aaa\", 2.3 ]", arr(1l, "aaa", 2.3));
		eq("[ ]", arr());
		eq("[1, \"aaa\", [2.3, 3, 4]]",
				arr(1l, "aaa", arr(2.3, 3l, 4l)));
		eq("[[]]", arr(arr()));
		eq("[1,[]]", arr(1l, arr()));
		eq("[[],1]", arr(arr(), 1l));
		eq("[ [ ] ]", arr(arr()));
		er("[1, 2,]");
		er("[1");
		er("[,2]");
		er("[1,,2]");
	}

	public void testObject() throws Exception {
		eq("{\"a\" : 1}", map("a", 1l));
		eq("{ \"a\" : 1 }", map("a", 1l));
		eq("{\"a\" : 1, \"b\" : 2}", map("a", 1l, "b", 2l));
		eq("{ \"a\" : 1, \"b\" : 2 }", map("a", 1l, "b", 2l));
		eq("{}", map());
		eq("{ }", map());
		eq("{\"a\" : {\"b\" : 1}, \"c\" : [1, 2, 3], \"d\" : {}}",
				map("a", map("b", 1l), "c", arr(1l, 2l, 3l), "d", map()));
		eq("{\"a\" : {}, \"d\" : {\"e\" : 1, \"f\" : 2}}",
				map("a", map(), "d", map("e", 1l, "f", 2l)));
		eq("{{} : \"a\", {\"e\" : 1, \"f\" : 2} : \"d\"}",
				map(map(), "a", map("e", 1l, "f", 2l), "d"));
		eq("{[1, 2, 3] : 1, [] : 4}", map(arr(1l, 2l, 3l), 1l, arr(), 4l));
		er("{1, 2}");
		er("{ 1, 2 }");
		er("{1 : {},}");
		er("{1 : {}, }");
		er("{1 : {} : 2}");
		er("{: 1, 2 : 3}");
	}

}
