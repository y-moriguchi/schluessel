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
package net.morilib.lisp.test.accessor;

import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Undef;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/26
 */
public class RefTest extends TCSubr {

	public void testRef() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(ref '(1 3 5) 0)", 1);
		eqi  (l,"(ref '(1 3 5) 1)", 3);
		eqi  (l,"(ref '(1 3 5) 2)", 5);
		lperr(l,"(ref '(1 3 5) -1)");
		lperr(l,"(ref '(1 3 5) 3)");
		lperr(l,"(ref '() 0)");

		eqi  (l,"(ref '#(1 3 5) 0)", 1);
		eqi  (l,"(ref '#(1 3 5) 1)", 3);
		eqi  (l,"(ref '#(1 3 5) 2)", 5);
		lperr(l,"(ref '#(1 3 5) -1)");
		lperr(l,"(ref '#(1 3 5) 3)");
		lperr(l,"(ref '#() 0)");

		eqv  (l,"(ref \"135\" 0)", chr('1'));
		eqv  (l,"(ref \"135\" 1)", chr('3'));
		eqv  (l,"(ref \"135\" 2)", chr('5'));
		lperr(l,"(ref \"135\" -1)");
		lperr(l,"(ref \"135\" 3)");
		lperr(l,"(ref \"\" 0)");

		eqi  (l,"(ref '#u8(1 3 5) 0)", 1);
		eqi  (l,"(ref '#u8(1 3 5) 1)", 3);
		eqi  (l,"(ref '#u8(1 3 5) 2)", 5);
		lperr(l,"(ref '#u8(1 3 5) -1)");
		lperr(l,"(ref '#u8(1 3 5) 3)");
		lperr(l,"(ref '#u8() 0)");

		eqi  (l,"(ref '#u16(1 3 5) 0)", 1);
		eqi  (l,"(ref '#u16(1 3 5) 1)", 3);
		eqi  (l,"(ref '#u16(1 3 5) 2)", 5);
		lperr(l,"(ref '#u16(1 3 5) -1)");
		lperr(l,"(ref '#u16(1 3 5) 3)");
		lperr(l,"(ref '#u16() 0)");

		eqi  (l,"(ref '#u32(1 3 5) 0)", 1);
		eqi  (l,"(ref '#u32(1 3 5) 1)", 3);
		eqi  (l,"(ref '#u32(1 3 5) 2)", 5);
		lperr(l,"(ref '#u32(1 3 5) -1)");
		lperr(l,"(ref '#u32(1 3 5) 3)");
		lperr(l,"(ref '#u32() 0)");

		eqi  (l,"(ref '#u64(1 3 5) 0)", 1);
		eqi  (l,"(ref '#u64(1 3 5) 1)", 3);
		eqi  (l,"(ref '#u64(1 3 5) 2)", 5);
		lperr(l,"(ref '#u64(1 3 5) -1)");
		lperr(l,"(ref '#u64(1 3 5) 3)");
		lperr(l,"(ref '#u64() 0)");

		eqi  (l,"(ref '#s8(1 3 5) 0)", 1);
		eqi  (l,"(ref '#s8(1 3 5) 1)", 3);
		eqi  (l,"(ref '#s8(1 3 5) 2)", 5);
		lperr(l,"(ref '#s8(1 3 5) -1)");
		lperr(l,"(ref '#s8(1 3 5) 3)");
		lperr(l,"(ref '#s8() 0)");

		eqi  (l,"(ref '#s16(1 3 5) 0)", 1);
		eqi  (l,"(ref '#s16(1 3 5) 1)", 3);
		eqi  (l,"(ref '#s16(1 3 5) 2)", 5);
		lperr(l,"(ref '#s16(1 3 5) -1)");
		lperr(l,"(ref '#s16(1 3 5) 3)");
		lperr(l,"(ref '#s16() 0)");

		eqi  (l,"(ref '#s32(1 3 5) 0)", 1);
		eqi  (l,"(ref '#s32(1 3 5) 1)", 3);
		eqi  (l,"(ref '#s32(1 3 5) 2)", 5);
		lperr(l,"(ref '#s32(1 3 5) -1)");
		lperr(l,"(ref '#s32(1 3 5) 3)");
		lperr(l,"(ref '#s32() 0)");

		eqi  (l,"(ref '#s64(1 3 5) 0)", 1);
		eqi  (l,"(ref '#s64(1 3 5) 1)", 3);
		eqi  (l,"(ref '#s64(1 3 5) 2)", 5);
		lperr(l,"(ref '#s64(1 3 5) -1)");
		lperr(l,"(ref '#s64(1 3 5) 3)");
		lperr(l,"(ref '#s64() 0)");

		eqv  (l,"(ref '#f32(1 3 5) 0)", newR(1.0f));
		eqv  (l,"(ref '#f32(1 3 5) 1)", newR(3.0f));
		eqv  (l,"(ref '#f32(1 3 5) 2)", newR(5.0f));
		lperr(l,"(ref '#f32(1 3 5) -1)");
		lperr(l,"(ref '#f32(1 3 5) 3)");
		lperr(l,"(ref '#f32() 0)");

		eqv  (l,"(ref '#f64(1 3 5) 0)", newR(1.0));
		eqv  (l,"(ref '#f64(1 3 5) 1)", newR(3.0));
		eqv  (l,"(ref '#f64(1 3 5) 2)", newR(5.0));
		lperr(l,"(ref '#f64(1 3 5) -1)");
		lperr(l,"(ref '#f64(1 3 5) 3)");
		lperr(l,"(ref '#f64() 0)");

		eqi  (l,"(ref (u8-list->bytevector '(1 3 5)) 0)", 1);
		eqi  (l,"(ref (u8-list->bytevector '(1 3 5)) 1)", 3);
		eqi  (l,"(ref (u8-list->bytevector '(1 3 5)) 2)", 5);
		lperr(l,"(ref (u8-list->bytevector '(1 3 5)) -1)");
		lperr(l,"(ref (u8-list->bytevector '(1 3 5)) 3)");
		lperr(l,"(ref (u8-list->bytevector '()) 0)");

		eqv  (l,"(ref 3 0)", Nil.NIL);
		eqv  (l,"(ref 3 1)", Nil.NIL);
		eqv  (l,"(ref 3 2)", Nil.NIL);
		lperr(l,"(ref 3 -1)");
		lperr(l,"(ref 3 3)");
		lperr(l,"(ref 0 0)");

		l.input("(define h (make-eqv-hashtable))");
		l.input("(hashtable-set! h 0 1)");
		l.input("(hashtable-set! h 1 3)");
		l.input("(hashtable-set! h 2 5)");
		eqi  (l,"(ref h 0)", 1);
		eqi  (l,"(ref h 1)", 3);
		eqi  (l,"(ref h 2)", 5);
		equal(l,"(ref h -1)", Undef.UNDEF);
		equal(l,"(ref h 3)", Undef.UNDEF);

		eq   (l,"(referrable? '(1 2 4))", T);
		eq   (l,"(referrable? '#(1 2 4))", T);
		eq   (l,"(referrable? '#u8(1 2 4))", T);
		eq   (l,"(referrable? '#u16(1 2 4))", T);
		eq   (l,"(referrable? '#u32(1 2 4))", T);
		eq   (l,"(referrable? '#u64(1 2 4))", T);
		eq   (l,"(referrable? '#s8(1 2 4))", T);
		eq   (l,"(referrable? '#s16(1 2 4))", T);
		eq   (l,"(referrable? '#s32(1 2 4))", T);
		eq   (l,"(referrable? '#s64(1 2 4))", T);
		eq   (l,"(referrable? (u8-list->bytevector '()))", T);
		eq   (l,"(referrable? (make-eqv-hashtable))", T);
		eq   (l,"(referrable? 1.0)", F);
	}

}
