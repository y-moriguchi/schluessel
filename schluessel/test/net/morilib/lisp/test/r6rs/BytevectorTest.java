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
package net.morilib.lisp.test.r6rs;

import java.math.BigInteger;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.r6rs.bytevector.LispBytevector;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/20
 */
public class BytevectorTest extends TCSubr {

	static void eqbv(Scheme l, String s, byte... bs) {
		eq(((LispBytevector)l.input(s)).toByteArray(), bs);
	}

	public void testGeneralOperations() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(endianness big)", sym("big"));
		eq   (l,"(endianness little)", sym("little"));
		lperr(l,"(endianness foo)");
//		eq   (l,"(native-endianness)", sym("big"));
		eq   (l,"(native-endianness)", sym("little"));

		eq   (l,"(bytevector? (make-bytevector 0))", T);
		eq   (l,"(bytevector? #(1))", F);

		eqbv (l,"(make-bytevector 3)", (byte)0, (byte)0, (byte)0);
		eqbv (l,"(make-bytevector 2 72)", (byte)72, (byte)72);
		eqbv (l,"(make-bytevector 2 255)", (byte)255, (byte)255);
		eqbv (l,"(make-bytevector 2 -128)", (byte)-128, (byte)-128);
		eqbv (l,"(make-bytevector 0)");
		lperr(l,"(make-bytevector -1)");
		lperr(l,"(make-bytevector 2 256)");
		lperr(l,"(make-bytevector 2 -129)");
		lperr(l,"(make-bytevector 2 0 1)");

		eqi  (l,"(bytevector-length (make-bytevector 10))", 10);
		eqi  (l,"(bytevector-length (make-bytevector 0))", 0);

		eq   (l,"(bytevector=?" +
				"  (make-bytevector 3 0)" +
				"  (make-bytevector 3 0))", T);
		eq   (l,"(bytevector=?" +
				"  (make-bytevector 3 0)" +
				"  (make-bytevector 2 0))", F);
		eq   (l,"(bytevector=?" +
				"  (make-bytevector 3 0)" +
				"  (make-bytevector 3 1))", F);
		eq   (l,"(bytevector=?" +
				"  (make-bytevector 3 0)" +
				"  (make-bytevector 3 0)" +
				"  (make-bytevector 3 0))", T);
		eq   (l,"(bytevector=?" +
				"  (make-bytevector 3 0)" +
				"  (make-bytevector 2 0)" +
				"  (make-bytevector 3 0))", F);
		eq   (l,"(bytevector=? (make-bytevector 3 0))", T);
		eq   (l,"(bytevector=?)", T);

		l.input("(define b (u8-list->bytevector '(1 2 3 4 5 6 7 8)))");
		l.input("(define c (make-bytevector 8 0))");
		l.input("(bytevector-copy! b 0 c 0 8)");
		eqbv (l,"c",
				(byte)1, (byte)2, (byte)3, (byte)4,
				(byte)5, (byte)6, (byte)7, (byte)8);
		l.input("(define c (make-bytevector 8 0))");
		l.input("(bytevector-copy! b 0 c 4 4)");
		eqbv (l,"c",
				(byte)0, (byte)0, (byte)0, (byte)0,
				(byte)1, (byte)2, (byte)3, (byte)4);
		l.input("(bytevector-copy! b 0 b 3 4)");
		eqbv (l,"b",
				(byte)1, (byte)2, (byte)3, (byte)1,
				(byte)2, (byte)3, (byte)4, (byte)8);
		l.input("(define b (u8-list->bytevector '(1 2 3 4 5 6 7 8)))");
		l.input("(bytevector-copy! b 3 b 0 4)");
		eqbv (l,"b",
				(byte)4, (byte)5, (byte)6, (byte)7,
				(byte)5, (byte)6, (byte)7, (byte)8);
		l.input("(bytevector-copy! b 8 b 0 0)");
		eqbv (l,"b",
				(byte)4, (byte)5, (byte)6, (byte)7,
				(byte)5, (byte)6, (byte)7, (byte)8);
		lperr(l,"(bytevector-copy! b -1 c  0 1)");
		lperr(l,"(bytevector-copy! b  7 c  0 2)");
		lperr(l,"(bytevector-copy! b  0 c -1 1)");
		lperr(l,"(bytevector-copy! b  0 c  7 2)");
		lperr(l,"(bytevector-copy! b  0 c  0 9)");
		lperr(l,"(bytevector-copy! b  0 c  0 -1)");
		lperr(l,"(bytevector-copy! 1  0 c  0 8)");
		lperr(l,"(bytevector-copy! b  0 1  0 8)");

		l.input("(define b (u8-list->bytevector '(1 2 3 4 5 6 7 8)))");
		eqbv (l,"(bytevector-copy b)",
				(byte)1, (byte)2, (byte)3, (byte)4,
				(byte)5, (byte)6, (byte)7, (byte)8);
		eqbv (l,"(bytevector-copy b 0 4)",
				(byte)1, (byte)2, (byte)3, (byte)4);
		eqbv (l,"(bytevector-copy b 4 8)",
				(byte)5, (byte)6, (byte)7, (byte)8);
		eqbv (l,"(bytevector-copy b 4 4)");
		lperr(l,"(bytevector-copy b -1 1)");
		lperr(l,"(bytevector-copy b 0 9)");
		lperr(l,"(bytevector-copy b -2 -1)");
		lperr(l,"(bytevector-copy b 9 10)");
		lperr(l,"(bytevector-copy b 4 3)");
		lperr(l,"(bytevector-copy b 2 3 1)");
	}

	public void testOperationsOnBytesAndOctets() {
		Scheme l = Scheme.newInstance();

		l.input("(define b (u8-list->bytevector '(72 184 255 127 128 0)))");
		eqi  (l,"(bytevector-u8-ref b 0)", 72);
		eqi  (l,"(bytevector-u8-ref b 1)", 184);
		eqi  (l,"(bytevector-u8-ref b 2)", 255);
		eqi  (l,"(bytevector-u8-ref b 3)", 127);
		eqi  (l,"(bytevector-u8-ref b 4)", 128);
		lperr(l,"(bytevector-u8-ref b -1)");
		lperr(l,"(bytevector-u8-ref b 6)");

		eqi  (l,"(bytevector-s8-ref b 0)", 72);
		eqi  (l,"(bytevector-s8-ref b 1)", -72);
		eqi  (l,"(bytevector-s8-ref b 2)", -1);
		eqi  (l,"(bytevector-s8-ref b 3)", 127);
		eqi  (l,"(bytevector-s8-ref b 4)", -128);
		lperr(l,"(bytevector-s8-ref b -1)");
		lperr(l,"(bytevector-s8-ref b 6)");

		l.input("(bytevector-u8-set! b 5 184)");
		eqi  (l,"(bytevector-u8-ref b 5)", 184);
		l.input("(bytevector-u8-set! b 5 255)");
		eqi  (l,"(bytevector-u8-ref b 5)", 255);
		l.input("(bytevector-u8-set! b 5 0)");
		eqi  (l,"(bytevector-u8-ref b 5)", 0);
		lperr(l,"(bytevector-u8-set! b -1 0)");
		lperr(l,"(bytevector-u8-set! b 6 0)");
		lperr(l,"(bytevector-u8-set! b 5 256)");
		lperr(l,"(bytevector-u8-set! b 5 -1)");

		l.input("(bytevector-s8-set! b 5 -72)");
		eqi  (l,"(bytevector-u8-ref b 5)", 184);
		l.input("(bytevector-s8-set! b 5 127)");
		eqi  (l,"(bytevector-u8-ref b 5)", 127);
		l.input("(bytevector-s8-set! b 5 -128)");
		eqi  (l,"(bytevector-u8-ref b 5)", 128);
		lperr(l,"(bytevector-s8-set! b -1 0)");
		lperr(l,"(bytevector-s8-set! b 6 0)");
		lperr(l,"(bytevector-s8-set! b 5 128)");
		lperr(l,"(bytevector-s8-set! b 5 -129)");

		l.input("(define b (u8-list->bytevector '(72 184 255 127 128 0)))");
		equal(l,"(bytevector->u8-list b)", list(72, 184, 255, 127, 128, 0));

		lperr(l,"(u8-list->bytevector '(1 -1))");
		lperr(l,"(u8-list->bytevector '(1 256))");
	}

	public void testOperationsOnIntegersOfArbitrarySize() {
		Scheme l = Scheme.newInstance();

		l.input("(define b (u8-list->bytevector" +
				"  '(253 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 255)))");
		l.input("(define c (u8-list->bytevector" +
				"  '(255 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 253)))");

		equal(l,"(bytevector-uint-ref b 0 (endianness little) 16)",
				newZ(new BigInteger("fffffffffffffffffffffffffffffffd", 16)));
		equal(l,"(bytevector-uint-ref b 0 (endianness big) 16)",
				newZ(new BigInteger("fdffffffffffffffffffffffffffffff", 16)));
		equal(l,"(bytevector-uint-ref b 0 (endianness little) 8)",
				newZ(new BigInteger("fffffffffffffffd", 16)));
		equal(l,"(bytevector-uint-ref b 0 (endianness big) 8)",
				newZ(new BigInteger("fdffffffffffffff", 16)));
		lperr(l,"(bytevector-uint-ref b 0 (endianness little) 17)");
		lperr(l,"(bytevector-uint-ref b 0 (endianness little) 0)");
		lperr(l,"(bytevector-uint-ref b 16 (endianness little) 1)");

		eqi  (l,"(bytevector-sint-ref b 0 (endianness little) 16)", -3);
		eqi  (l,"(bytevector-sint-ref c 0 (endianness big) 16)", -3);
		eqi  (l,"(bytevector-sint-ref b 0 (endianness little) 8)", -3);
		eqi  (l,"(bytevector-sint-ref c 8 (endianness big) 8)", -3);
		lperr(l,"(bytevector-sint-ref b 0 (endianness little) 17)");
		lperr(l,"(bytevector-sint-ref b 0 (endianness little) 0)");
		lperr(l,"(bytevector-sint-ref b 16 (endianness little) 1)");

		l.input("(define d (make-bytevector 16 0))");
		l.input("(bytevector-uint-set!" +
				"  d" +
				"  0" +
				"  #xfffffffffffffffffffffffffffffffd" +
				"  (endianness little)" +
				"  16)");
		eq   (l,"(bytevector=? b d)", T);
		l.input("(bytevector-uint-set!" +
				"  d" +
				"  0" +
				"  #xfffffffffffffffffffffffffffffffd" +
				"  (endianness big)" +
				"  16)");
		eq   (l,"(bytevector=? c d)", T);
		l.input("(define d (make-bytevector 16 0))");
		l.input("(bytevector-uint-set!" +
				"  d" +
				"  2" +
				"  #xfffffffffffffffd" +
				"  (endianness little)" +
				"  8)");
		equal(l,"(bytevector->u8-list d)",
				list(0, 0, 253, 255, 255, 255, 255, 255,
						255, 255, 0, 0, 0, 0, 0, 0));
		lperr(l,"(bytevector-uint-set! d 0 0 (endianness little) 17)");
		lperr(l,"(bytevector-uint-set! d 0 0 (endianness little) 0)");
		lperr(l,"(bytevector-uint-set! d 16 0 (endianness little) 1)");
		lperr(l,"(bytevector-uint-set! d 0 -1 (endianness little) 0)");

		l.input("(define d (make-bytevector 16 0))");
		l.input("(bytevector-sint-set! d 0 -3 (endianness little) 16)");
		eq   (l,"(bytevector=? b d)", T);
		l.input("(bytevector-sint-set! d 0 -3 (endianness big) 16)");
		eq   (l,"(bytevector=? c d)", T);
		l.input("(define d (make-bytevector 16 0))");
		l.input("(bytevector-sint-set! d 2 -3 (endianness little) 8)");
		equal(l,"(bytevector->u8-list d)",
				list(0, 0, 253, 255, 255, 255, 255, 255,
						255, 255, 0, 0, 0, 0, 0, 0));
		lperr(l,"(bytevector-sint-set! d 0 0 (endianness little) 17)");
		lperr(l,"(bytevector-sint-set! d 0 0 (endianness little) 0)");
		lperr(l,"(bytevector-sint-set! d 16 0 (endianness little) 1)");

		equal(l,"(bytevector->uint-list b (endianness little) 2)",
				list(65533, 65535, 65535, 65535, 65535, 65535, 65535, 65535));
		equal(l,"(bytevector->uint-list b (endianness big) 2)",
				list(65023, 65535, 65535, 65535, 65535, 65535, 65535, 65535));
		equal(l,"(bytevector->uint-list c (endianness big) 2 8 16)",
				list(65535, 65535, 65535, 65533));
		lperr(l,"(bytevector->uint-list b (endianness big) 2 0 18)");
		lperr(l,"(bytevector->uint-list b (endianness big) 2 -1 15)");
		lperr(l,"(bytevector->uint-list b (endianness big) 2 17 19)");
		lperr(l,"(bytevector->uint-list b (endianness big) 2 10 8)");
		lperr(l,"(bytevector->uint-list b (endianness big) 2 0 3)");

		equal(l,"(bytevector->sint-list b (endianness little) 2)",
				list(-3, -1, -1, -1, -1, -1, -1, -1));
		equal(l,"(bytevector->sint-list b (endianness big) 2)",
				list(-513, -1, -1, -1, -1, -1, -1, -1));
		equal(l,"(bytevector->sint-list c (endianness big) 2 8 16)",
				list(-1, -1, -1, -3));
		lperr(l,"(bytevector->sint-list b (endianness big) 2 0 18)");
		lperr(l,"(bytevector->sint-list b (endianness big) 2 -1 15)");
		lperr(l,"(bytevector->sint-list b (endianness big) 2 17 19)");
		lperr(l,"(bytevector->sint-list b (endianness big) 2 10 8)");
		lperr(l,"(bytevector->sint-list b (endianness big) 2 0 3)");

		equal(l,"(bytevector->u8-list (uint-list->bytevector" +
				"  '(65023 65535 65535 65535 65535 65535 65535 65535)" +
				"  (endianness big)" +
				"  2))",
				list(253, 255, 255, 255, 255, 255, 255, 255,
						255, 255, 255, 255, 255, 255, 255, 255));
		equal(l,"(bytevector->u8-list (uint-list->bytevector" +
				"  '(65533 65535 65535 65535 65535 65535 65535 65535)" +
				"  (endianness little)" +
				"  2))",
				list(253, 255, 255, 255, 255, 255, 255, 255,
						255, 255, 255, 255, 255, 255, 255, 255));
		lperr(l,"(uint-list->bytevector '(1 -1) (endianness big) 2)");

		equal(l,"(bytevector->u8-list (sint-list->bytevector" +
				"  '(-513 -1 -1 -1 -1 -1 -1 -1)" +
				"  (endianness big)" +
				"  2))",
				list(253, 255, 255, 255, 255, 255, 255, 255,
						255, 255, 255, 255, 255, 255, 255, 255));
		equal(l,"(bytevector->u8-list (sint-list->bytevector" +
				"  '(-3 -1 -1 -1 -1 -1 -1 -1)" +
				"  (endianness little)" +
				"  2))",
				list(253, 255, 255, 255, 255, 255, 255, 255,
						255, 255, 255, 255, 255, 255, 255, 255));
	}

	public void testOperationsOn16bitIntegers() {
		Scheme l = Scheme.newInstance();

		l.input("(define b (u8-list->bytevector" +
				"  '(253 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 255)))");
		l.input("(define c (u8-list->bytevector" +
				"  '(255 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 253)))");

		eqi  (l,"(bytevector-u16-ref b 0 (endianness little))", 65533);
		eqi  (l,"(bytevector-u16-ref b 0 (endianness big))", 65023);
		eqi  (l,"(bytevector-u16-ref b 2 (endianness little))", 65535);
		eqi  (l,"(bytevector-u16-ref b 2 (endianness big))", 65535);
		lperr(l,"(bytevector-u16-ref b 16 (endianness little))");

		eqi  (l,"(bytevector-s16-ref b 0 (endianness little))", -3);
		eqi  (l,"(bytevector-s16-ref c 14 (endianness big))", -3);
		lperr(l,"(bytevector-s16-ref b 16 (endianness little))");

		l.input("(define d (make-bytevector 4 0))");
		l.input("(bytevector-u16-set! d 0 65533 (endianness big))");
		l.input("(bytevector-u16-set! d 2 65533 (endianness little))");
		equal(l,"(bytevector->u8-list d)", list(255, 253, 253, 255));
		l.input("(bytevector-u16-set! d 0 0 (endianness big))");
		l.input("(bytevector-u16-set! d 2 65535 (endianness big))");
		equal(l,"(bytevector->u8-list d)", list(0, 0, 255, 255));
		lperr(l,"(bytevector-u16-set! d 4 0 (endianness little))");
		lperr(l,"(bytevector-u16-set! d 0 -1 (endianness little))");
		lperr(l,"(bytevector-u16-set! d 0 65536 (endianness little))");

		l.input("(define d (make-bytevector 4 0))");
		l.input("(bytevector-s16-set! d 0 -3 (endianness big))");
		l.input("(bytevector-s16-set! d 2 -3 (endianness little))");
		equal(l,"(bytevector->u8-list d)", list(255, 253, 253, 255));
		l.input("(bytevector-s16-set! d 0 -32768 (endianness big))");
		l.input("(bytevector-s16-set! d 2 32767 (endianness big))");
		equal(l,"(bytevector->u8-list d)", list(128, 0, 127, 255));
		lperr(l,"(bytevector-s16-set! d 4 0 (endianness little))");
		lperr(l,"(bytevector-s16-set! d 0 -32769 (endianness little))");
		lperr(l,"(bytevector-s16-set! d 0 32768 (endianness little))");
	}

	public void testOperationsOn32bitIntegers() {
		Scheme l = Scheme.newInstance();

		l.input("(define b (u8-list->bytevector" +
				"  '(253 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 255)))");
		l.input("(define c (u8-list->bytevector" +
				"  '(255 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 253)))");

		eql  (l,"(bytevector-u32-ref b 0 (endianness little))", 0xfffffffdl);
		eql  (l,"(bytevector-u32-ref b 0 (endianness big))", 0xfdffffffl);
		eql  (l,"(bytevector-u32-ref b 2 (endianness little))", 0xffffffffl);
		eql  (l,"(bytevector-u32-ref b 2 (endianness big))", 0xffffffffl);
		lperr(l,"(bytevector-u32-ref b 16 (endianness little))");

		eql  (l,"(bytevector-s32-ref b 0 (endianness little))", -3);
		eql  (l,"(bytevector-s32-ref c 12 (endianness big))", -3);
		lperr(l,"(bytevector-s32-ref b 16 (endianness little))");

		l.input("(define d (make-bytevector 8 0))");
		l.input("(bytevector-u32-set! d 0 #xfffffffd (endianness big))");
		l.input("(bytevector-u32-set! d 4 #xfffffffd (endianness little))");
		equal(l,"(bytevector->u8-list d)",
				list(255, 255, 255, 253, 253, 255, 255, 255));
		l.input("(bytevector-u32-set! d 0 0 (endianness big))");
		l.input("(bytevector-u32-set! d 4 #xffffffff (endianness big))");
		equal(l,"(bytevector->u8-list d)",
				list(0, 0, 0, 0, 255, 255, 255, 255));
		lperr(l,"(bytevector-u32-set! d 8 0 (endianness little))");
		lperr(l,"(bytevector-u32-set! d 0 -1 (endianness little))");
		lperr(l,"(bytevector-u32-set! d 0 #x100000000 (endianness little))");

		l.input("(define d (make-bytevector 8 0))");
		l.input("(bytevector-s32-set! d 0 -3 (endianness big))");
		l.input("(bytevector-s32-set! d 4 -3 (endianness little))");
		equal(l,"(bytevector->u8-list d)",
				list(255, 255, 255, 253, 253, 255, 255, 255));
		l.input("(bytevector-s32-set! d 0 -2147483648 (endianness big))");
		l.input("(bytevector-s32-set! d 4 2147483647 (endianness big))");
		equal(l,"(bytevector->u8-list d)",
				list(128, 0, 0, 0, 127, 255, 255, 255));
		lperr(l,"(bytevector-s32-set! d 8 0 (endianness little))");
		lperr(l,"(bytevector-s32-set! d 0 -2147483649 (endianness little))");
		lperr(l,"(bytevector-s32-set! d 0 2147483648 (endianness little))");
	}

	public void testOperationsOn64bitIntegers() {
		Scheme l = Scheme.newInstance();

		l.input("(define b (u8-list->bytevector" +
				"  '(253 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 255)))");
		l.input("(define c (u8-list->bytevector" +
				"  '(255 255 255 255 255 255 255 255" +
				"    255 255 255 255 255 255 255 253)))");

		eqv  (l,"(bytevector-u64-ref b 0 (endianness little))",
				newZ(new BigInteger("fffffffffffffffd", 16)));
		eqv  (l,"(bytevector-u64-ref b 0 (endianness big))",
				newZ(new BigInteger("fdffffffffffffff", 16)));
		eqv  (l,"(bytevector-u64-ref b 2 (endianness little))",
				newZ(new BigInteger("ffffffffffffffff", 16)));
		eqv  (l,"(bytevector-u64-ref b 2 (endianness big))",
				newZ(new BigInteger("ffffffffffffffff", 16)));
		lperr(l,"(bytevector-u64-ref b 16 (endianness little))");

		eqi  (l,"(bytevector-s64-ref b 0 (endianness little))", -3);
		eqi  (l,"(bytevector-s64-ref c 8 (endianness big))", -3);
		lperr(l,"(bytevector-s64-ref b 16 (endianness little))");

		l.input("(define d (make-bytevector 16 0))");
		l.input("(bytevector-u64-set! d 0 #xfffffffffffffffd (endianness big))");
		l.input("(bytevector-u64-set! d 8 #xfffffffffffffffd (endianness little))");
		equal(l,"(bytevector->u8-list d)",
				list(255, 255, 255, 255, 255, 255, 255, 253,
						253, 255, 255, 255, 255, 255, 255, 255));
		l.input("(bytevector-u64-set! d 0 0 (endianness big))");
		l.input("(bytevector-u64-set! d 8 #xffffffffffffffff (endianness big))");
		equal(l,"(bytevector->u8-list d)",
				list(0, 0, 0, 0, 0, 0, 0, 0,
						255, 255, 255, 255, 255, 255, 255, 255));
		lperr(l,"(bytevector-u64-set! d 16 0 (endianness little))");
		lperr(l,"(bytevector-u64-set! d 0 -1 (endianness little))");
		lperr(l,"(bytevector-u64-set! d 0 #x10000000000000000 (endianness little))");

		l.input("(define d (make-bytevector 16 0))");
		l.input("(bytevector-s64-set! d 0 -3 (endianness big))");
		l.input("(bytevector-s64-set! d 8 -3 (endianness little))");
		equal(l,"(bytevector->u8-list d)",
				list(255, 255, 255, 255, 255, 255, 255, 253,
						253, 255, 255, 255, 255, 255, 255, 255));
		l.input("(bytevector-s64-set! d 0 -9223372036854775808 (endianness big))");
		l.input("(bytevector-s64-set! d 8 9223372036854775807 (endianness big))");
		equal(l,"(bytevector->u8-list d)",
				list(128, 0, 0, 0, 0, 0, 0, 0,
						127, 255, 255, 255, 255, 255, 255, 255));
		lperr(l,"(bytevector-s64-set! d 16 0 (endianness little))");
		lperr(l,"(bytevector-s64-set! d 0 -9223372036854775809 (endianness little))");
		lperr(l,"(bytevector-s64-set! d 0 9223372036854775808 (endianness little))");
	}

	public void testOperationsOnIEEE754Representations() {
		Scheme l = Scheme.newInstance();

		l.input("(define d (uint-list->bytevector" +
				"  '(4649946560167946486 1145007636) (endianness big) 8))");

		eqv  (l,"(bytevector-ieee-single-ref d 12 (endianness big))",
				newR(765.72f));
		eqv  (l,"(bytevector-ieee-double-ref d 0 (endianness big))",
				newR(765.72));
		lperr(l,"(bytevector-ieee-single-ref d 13 (endianness big))");
		lperr(l,"(bytevector-ieee-double-ref d 9 (endianness big))");

		l.input("(define f (make-bytevector 16 0))");
		l.input("(bytevector-ieee-single-set! f 12 765.72 (endianness big))");
		l.input("(bytevector-ieee-double-set! f 0 765.72 (endianness big))");
		eq   (l,"(bytevector=? d f)", T);
		lperr(l,"(bytevector-ieee-single-set! f 13 0 (endianness big))");
		lperr(l,"(bytevector-ieee-double-set! f 9 0 (endianness big))");
	}

	public void testOperationsOnStrings() {
		Scheme l = Scheme.newInstance();

		l.input("(define u0  (u8-list->bytevector '()))");
		l.input("(define u8  (u8-list->bytevector '(55 54 53)))");
		l.input("(define u16 (u8-list->bytevector '(00 55 00 54 00 53)))");
		l.input("(define u17 (u8-list->bytevector '(55 00 54 00 53 00)))");
		l.input("(define u18 (u8-list->bytevector '(254 255 00 55 00 54 00 53)))");
		l.input("(define u19 (u8-list->bytevector '(255 254 55 00 54 00 53 00)))");
		l.input("(define u32 (u8-list->bytevector" +
				"  '(00 00 00 55 00 00 00 54 00 00 00 53)))");
		l.input("(define u33 (u8-list->bytevector" +
				"  '(55 00 00 00 54 00 00 00 53 00 00 00)))");
		l.input("(define u34 (u8-list->bytevector" +
				"  '(00 00 254 255 00 00 00 55 00 00 00 54 00 00 00 53)))");
		l.input("(define u35 (u8-list->bytevector" +
				"  '(255 254 00 00 55 00 00 00 54 00 00 00 53 00 00 00)))");

		eq   (l,"(bytevector=? (string->utf8 \"765\") u8)", T);
		eq   (l,"(bytevector=? (string->utf8 \"\") u0)", T);
		eq   (l,"(bytevector=? (string->utf16 \"765\") u16)", T);
		eq   (l,"(bytevector=? (string->utf16 \"765\" (endianness big)) u16)", T);
		eq   (l,"(bytevector=? (string->utf16 \"765\" (endianness little)) u17)", T);
		eq   (l,"(bytevector=? (string->utf16 \"\") u0)", T);
		eq   (l,"(bytevector=? (string->utf32 \"765\") u32)", T);
		eq   (l,"(bytevector=? (string->utf32 \"765\" (endianness big)) u32)", T);
		eq   (l,"(bytevector=? (string->utf32 \"765\" (endianness little)) u33)", T);
		eq   (l,"(bytevector=? (string->utf32 \"\") u0)", T);

		eqs  (l,"(utf8->string u8)", "765");
		eqs  (l,"(utf8->string u0)", "");

		eqs  (l,"(utf16->string u16 (endianness big) #f)", "765");
		eqs  (l,"(utf16->string u17 (endianness little))", "765");
		eqs  (l,"(utf16->string u18 (endianness little))", "765");
		eqs  (l,"(utf16->string u19 (endianness big) #f)", "765");
		eqs  (l,"(utf16->string u16 (endianness big) #t)", "765");
		eqs  (l,"(utf16->string u17 (endianness little) #t)", "765");
		eqs  (l,"(utf16->string u0 (endianness big) #t)", "");
		eqs  (l,"(utf16->string u0 (endianness big))", "");

		eqs  (l,"(utf32->string u32 (endianness big) #f)", "765");
		eqs  (l,"(utf32->string u33 (endianness little))", "765");
		eqs  (l,"(utf32->string u34 (endianness little))", "765");
		eqs  (l,"(utf32->string u35 (endianness big) #f)", "765");
		eqs  (l,"(utf32->string u32 (endianness big) #t)", "765");
		eqs  (l,"(utf32->string u33 (endianness little) #t)", "765");
		eqs  (l,"(utf32->string u0 (endianness big) #t)", "");
		eqs  (l,"(utf32->string u0 (endianness big))", "");
	}

}
