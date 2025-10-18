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
package net.morilib.lisp.json;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Nil;
import net.morilib.parser.json.JSONParseHandler;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/14
 */
public class JsonToListHandler implements JSONParseHandler {

	/**
	 * 
	 */
	public static final JsonToListHandler EXACT =
		new JsonToListHandler(true);

	/**
	 * 
	 */
	public static final JsonToListHandler INEXACT =
		new JsonToListHandler(false);

	//
	/*package*/ Stack<Object> stack = new Stack<Object>();
	private boolean exact;

	//
	private JsonToListHandler(boolean exact) {
		this.exact = exact;
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#begin()
	 */
	public void begin() {
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#eof()
	 */
	public Object eof() {
		return stack.pop();
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#beginArray()
	 */
	public void beginArray() {
		stack.push(new ArrayList<Datum>());
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#beginObject()
	 */
	public void beginObject() {
		stack.push(new ConsListBuilder());
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#nextArray()
	 */
	@SuppressWarnings("unchecked")
	public void nextArray() {
		Datum o = (Datum)stack.pop();
		List<Datum> l = (List<Datum>)stack.peek();

		l.add(o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#endArray()
	 */
	@SuppressWarnings("unchecked")
	public void endArray() {
		Datum o = (Datum)stack.pop();
		List<Datum> l = (List<Datum>)stack.pop();

		l.add(o);
		stack.push(new LispVector(l));
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#nextValue()
	 */
	public void nextValue() {
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#nextObject()
	 */
	public void nextObject() {
		Datum v = (Datum)stack.pop();
		Datum k = (Datum)stack.pop();
		ConsListBuilder l = (ConsListBuilder)stack.peek();

		l.append(new Cons(k, v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#endObject()
	 */
	public void endObject() {
		Datum v = (Datum)stack.pop();
		Datum k = (Datum)stack.pop();
		ConsListBuilder l = (ConsListBuilder)stack.pop();

		l.append(new Cons(k, v));
		stack.push(l.get());
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#setString(java.lang.String)
	 */
	public void setString(String s) {
		stack.push(LispString.valueOf(s));
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#setTrue()
	 */
	public void setTrue() {
		stack.push(LispBoolean.TRUE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#setFalse()
	 */
	public void setFalse() {
		stack.push(LispBoolean.FALSE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#setNull()
	 */
	public void setNull() {
		stack.push(Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#setInteger(java.math.BigInteger)
	 */
	public void setInteger(BigInteger x) {
		stack.push(LispInteger.valueOf(x));
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#setFraction(java.math.BigDecimal)
	 */
	public void setFraction(BigDecimal x) {
		if(exact) {
			stack.push(LispUtils.bigDecimalToRational(x));
		} else {
			stack.push(new LispDouble(x.floatValue()));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#emptyArray()
	 */
	public void emptyArray() {
		stack.pop();
		stack.push(new LispVector());
	}

	/* (non-Javadoc)
	 * @see net.morilib.parser.json.JSONParseHandler#emptyObject()
	 */
	public void emptyObject() {
		stack.pop();
		stack.push(Nil.NIL);
	}

}
