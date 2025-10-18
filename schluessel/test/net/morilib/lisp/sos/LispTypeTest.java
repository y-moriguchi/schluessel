/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.sos;

import java.util.Arrays;

import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.sos.LispTypeException;
import net.morilib.lisp.test.TCSubr;

public class LispTypeTest extends TCSubr {
	
	public void testPrecedenceList1() throws Exception {
		LispType sList    = new LispType(LispType.TOP);
		LispType food     = new LispType(sList);
		LispType spice    = new LispType(food, sList);
		LispType fruit    = new LispType(food, sList);
		LispType cinnamon = new LispType(spice, sList);
		LispType apple    = new LispType(fruit, sList);
		LispType pie      = new LispType(apple, cinnamon);
		LispType[] res = new LispType[] {
				pie, apple, cinnamon, fruit, spice, food, sList,
				LispType.TOP
		};
		
		eq(pie.getCPL(), Arrays.asList(res));
	}
	
	public void testPrecedenceList2() {
		LispType sList;
		LispType food;
		LispType fruit;
		LispType apple;
		
		try {
			sList    = new LispType(LispType.TOP);
			food     = new LispType(sList);
			fruit    = new LispType(food);
			apple    = new LispType(fruit);
		} catch(LispTypeException e) {
			throw new RuntimeException(e);
		}
		
		try {
			new LispType(fruit, apple);
			fail();
		} catch (LispTypeException e) {
			// ok
		}
	}
	
}
