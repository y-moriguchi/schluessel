/* 
 * A Common Packages
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/17
 */
package net.morilib.automata.lr;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import net.morilib.automata.lr.ContextFreeGrammar;
import net.morilib.automata.lr.ContextFreeReduceAction;
import net.morilib.automata.lr.ContextFreeRule;
import net.morilib.automata.lr.LALR1Items;
import net.morilib.automata.lr.LALR1Table;
import net.morilib.automata.lr.LRParseException;
import net.morilib.automata.lr.LRParser;
import net.morilib.automata.lr.LexicalAnalyser;
import net.morilib.automata.lr.SemanticAttributes;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/17
 */
public class LRParserRun extends TCGrammar {
	
	static GST star  = new GST("*");
	static GST plus  = new GST("+");
	static GST lpar  = new GST("(");
	static GST rpar  = new GST(")");
	static GST eqal  = new GST("=");
	static GST num0  = new GST("n");
	static GST id00  = new GST("id");
	
	//
	static class Lexer implements LexicalAnalyser<Integer> {
		
		//
		private String str;
		private int ptr = 0;
		
		//
		Lexer(String str) {
			this.str = str;
		}

		//
		public boolean isEnded() {
			return ptr >= str.length();
		}

		//
		public Token<Integer> nextToken() {
			if(ptr >= str.length()) {
				return Token.endMarker();
			}
			
			int c = str.charAt(ptr++);
			if(c == '*') {
				return new Token<Integer>(star, null);
			} else if(c == '+') {
				return new Token<Integer>(plus, null);
			} else if(c == '(') {
				return new Token<Integer>(lpar, null);
			} else if(c == ')') {
				return new Token<Integer>(rpar, null);
			} else if(c >= '0' && c <= '9') {
				return new Token<Integer>(num0, c - '0');
			}
			return null;
		}
		
	}
	
	//
	static class Lexer2 implements LexicalAnalyser<Void> {
		
		//
		private String str;
		private int ptr = 0;
		
		//
		Lexer2(String str) {
			this.str = str;
		}

		//
		public boolean isEnded() {
			return ptr >= str.length();
		}

		//
		public Token<Void> nextToken() {
			if(ptr >= str.length()) {
				return Token.endMarker();
			}
			
			int c = str.charAt(ptr++);
			if(c == '*') {
				return new Token<Void>(star, null);
			} else if(c == '=') {
				return new Token<Void>(eqal, null);
			} else {
				return new Token<Void>(id00, null);
			}
		}
		
	}
	
	static Map<Integer, Lexer3.Token<Void>> tame =
		new HashMap<Integer, Lexer3.Token<Void>>();
	
	//
	static class Lexer3 implements LexicalAnalyser<Void> {
		
		//
		private String str;
		private int ptr = 0;
		
		//
		Lexer3(String str) {
			this.str = str;
		}

		//
		public boolean isEnded() {
			return ptr >= str.length();
		}

		//
		public Token<Void> nextToken() {
			if(ptr >= str.length()) {
				return Token.endMarker();
			}
			
			int c = str.charAt(ptr++);
			char[] c2 = new char[1];
			c2[0] = (char)c;
			
			Token<Void> res = tame.get(c);
			if(res == null) {
				res = new Token<Void>(new GST(new String(c2)), null);
				tame.put(c, res);
			}
			return res;
		}
		
	}
	
	/**
	 * @throws LRParseException
	 * 
	 *
	 */
	public void testParser1() throws LRParseException {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN E = new GSN("E");
		GSN F = new GSN("F");
		//s.add("E"); s.add("F");
		//t.add("*"); t.add("+"); t.add("n");
		
		ContextFreeRule e1 = new ContextFreeRule(E, E, plus, F);
		ContextFreeRule e2 = new ContextFreeRule(E, F);
		ContextFreeRule f1 = new ContextFreeRule(F, F, star, num0);
		ContextFreeRule f2 = new ContextFreeRule(F, num0);
		
		/*ContextFreeRule e1 = new ContextFreeRule("E",
				new Object[] { "E", "+", "F" });
		ContextFreeRule e2 = new ContextFreeRule("E",
				new Object[] { "F" });
		ContextFreeRule f1 = new ContextFreeRule("F",
				new Object[] { "F", "*", "n" });
		ContextFreeRule f2 = new ContextFreeRule("F",
				new Object[] { "n" });*/
		
		rules.add(e1);
		rules.add(e2);
		rules.add(f1);
		rules.add(f2);
		
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, E);
		//LR0Items  items = LR0Items.build(g);
		//SLR1Table table = new SLR1Table(items);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		//
		//System.out.println(g.follow("E"));
		//System.out.println(g.follow("F"));
		
		//
		/*for(int i = 0; i < table.actionTable.length; i++) {
			System.out.println(table.actionTable[i]);
		}
		System.out.println("-----------------------------");
		for(int i = 0; i < table.goToTable.length; i++) {
			System.out.println(table.goToTable[i]);
		}*/
		
		LRParser<Integer> parser = new LRParser<Integer>(table);
		parser.setAction(e1, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				Integer r1 = attrs.get(0);
				Integer r2 = attrs.get(2);
				
				return r1.intValue() + r2.intValue();
			}
			
		});
		
		parser.setAction(e2, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				return attrs.get(0);
			}
			
		});
		
		parser.setAction(f1, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				Integer r1 = attrs.get(0);
				Integer r2 = attrs.get(2);
				
				return r1.intValue() * r2.intValue();
			}
			
		});
		
		parser.setAction(f2, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				return attrs.get(0);
			}
			
		});
		
		System.out.println(parser.parse(new Lexer("3+2*4+1")));
	}
	
	
	public void testParser2() throws Exception {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN E = new GSN("E");
		GSN F = new GSN("F");
		GSN T = new GSN("T");
		GSN E2 = new GSN("E'");
		GSN T2 = new GSN("T'");
		//s.add("E");   s.add("T");   s.add("F");
		//s.add("E'");  s.add("T'");
		//t.add("+");   t.add("*");   t.add("n");
		//t.add("(");   t.add(")");
		
		ContextFreeRule e1 = new ContextFreeRule(E, T, E2);
		ContextFreeRule e2 = new ContextFreeRule(E2, plus, T, E2);
		ContextFreeRule e3 = new ContextFreeRule(E2);
		ContextFreeRule t1 = new ContextFreeRule(T, F, T2);
		ContextFreeRule t2 = new ContextFreeRule(T2, star, F, T2);
		ContextFreeRule t3 = new ContextFreeRule(T2);
		ContextFreeRule f1 = new ContextFreeRule(F, lpar, E, rpar);
		ContextFreeRule f2 = new ContextFreeRule(F, num0);
		
		rules.add(e1);
		rules.add(e2);
		rules.add(e3);
		rules.add(t1);
		rules.add(t2);
		rules.add(t3);
		rules.add(f1);
		rules.add(f2);
		
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, E);
		//LR0Items  items = LR0Items.build(g);
		//SLR1Table table = new SLR1Table(items);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		//
		LRParser<Integer> parser = new LRParser<Integer>(table);
		parser.setAction(e1, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				Integer r1 = attrs.get(0);
				Integer r2 = attrs.get(1);
				
				return (r1.intValue() + r2.intValue());
			}
			
		});
		
		parser.setAction(e2, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				Integer r1 = attrs.get(1);
				Integer r2 = attrs.get(2);
				
				return (r1.intValue() + r2.intValue());
			}
			
		});
		
		parser.setAction(e3, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				return 0;
			}
			
		});
		
		parser.setAction(t1, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				Integer r1 = attrs.get(0);
				Integer r2 = attrs.get(1);
				
				return (r1.intValue() * r2.intValue());
			}
			
		});
		
		parser.setAction(t2, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				Integer r1 = attrs.get(1);
				Integer r2 = attrs.get(2);
				
				return (r1.intValue() * r2.intValue());
			}
			
		});
		
		parser.setAction(t3, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				return 1;
			}
			
		});
		
		parser.setAction(f1, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				return attrs.get(1);
			}
			
		});
		
		parser.setAction(f2, new ContextFreeReduceAction<Integer>() {

			public Integer action(
					ContextFreeRule event,
					SemanticAttributes<Integer> attrs) {
				return attrs.get(0);
			}
			
		});
		
		System.out.println(parser.parse(new Lexer("(3+2)*4+1")));
	}
	
	/**
	 * 
	 *
	 */
	public void testLALR2() throws Exception {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN L = new GSN("L");
		GSN R = new GSN("R");
		//s.add("S"); s.add("L"); s.add("R");
		
		//t.add("=");  t.add("*");  t.add("id");
		
		rules.add(new ContextFreeRule(S, L, eqal, R));
		rules.add(new ContextFreeRule(S, R));
		rules.add(new ContextFreeRule(L, star, R));
		rules.add(new ContextFreeRule(L, id00));
		rules.add(new ContextFreeRule(R, L));
		
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, S);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		System.out.println(table.getConflicts());
		
		LRParser<Void> parser = new LRParser<Void>(table);
		parser.parse(new Lexer2("*a=**b"));
	}
	
	/**
	 * 
	 *
	 */
	public void testLALR3() throws Exception {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN A = new GSN("A");
		//s.add("A");
		
		GST a = new GST("a");
		GST b = new GST("b");
		GST c = new GST("c");
		GST d = new GST("d");
		tame.put((int)'a', new Lexer3.Token<Void>(a, null));
		tame.put((int)'b', new Lexer3.Token<Void>(b, null));
		tame.put((int)'c', new Lexer3.Token<Void>(c, null));
		tame.put((int)'d', new Lexer3.Token<Void>(d, null));
		//t.add("a"); t.add("b"); t.add("c"); t.add("d");
		
		ContextFreeRule r1 = new ContextFreeRule(A, A, c);
		ContextFreeRule r2 = new ContextFreeRule(A, A, a, d);
		ContextFreeRule r3 = new ContextFreeRule(A, b, d);
		ContextFreeRule r4 = new ContextFreeRule(A);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		
		//
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, A);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		System.out.println(table.getConflicts());
		
		LRParser<Void> parser = new LRParser<Void>(table);
		parser.parse(new Lexer3("adcc"));
		parser.parse(new Lexer3("bdadcc"));
	}
	
	/**
	 * 
	 *
	 */
	public void testLALR4() throws Exception {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN E = new GSN("E");
		//s.add("S"); s.add("E");
		
		GST i = new GST("i");
		GST t = new GST("t");
		GST e = new GST("e");
		GST a = new GST("a");
		GST b = new GST("b");
		tame.put((int)'i', new Lexer3.Token<Void>(i, null));
		tame.put((int)'t', new Lexer3.Token<Void>(t, null));
		tame.put((int)'r', new Lexer3.Token<Void>(e, null));
		tame.put((int)'a', new Lexer3.Token<Void>(a, null));
		tame.put((int)'b', new Lexer3.Token<Void>(b, null));
		//t.add("i"); t.add("t"); t.add("e"); t.add("a"); t.add("b");
		
		ContextFreeRule r1 = new ContextFreeRule(S, i, E, t, S);
		ContextFreeRule r2 = new ContextFreeRule(S, i, E, t, S, e, S);
		ContextFreeRule r3 = new ContextFreeRule(S, a);
		ContextFreeRule r4 = new ContextFreeRule(E, b);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		
		//
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, S);
		//LR0Items  items = LR0Items.build(g);
		//SLR1Table table = new SLR1Table(items);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		System.out.println(table.getConflicts());
	}
	
	/**
	 * 
	 *
	 */
	public void testLALR5() throws Exception {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN A = new GSN("A");
		GSN B = new GSN("B");
		//s.add("S"); s.add("A"); s.add("B");
		
		GST a = new GST("a");
		GST b = new GST("b");
		GST c = new GST("c");
		tame.put((int)'a', new Lexer3.Token<Void>(a, null));
		tame.put((int)'b', new Lexer3.Token<Void>(b, null));
		tame.put((int)'c', new Lexer3.Token<Void>(c, null));
		//t.add("a"); t.add("b"); t.add("c"); t.add("*");
		
		ContextFreeRule r1 = new ContextFreeRule(S, A, B);
		ContextFreeRule r2 = new ContextFreeRule(A, A, a);
		ContextFreeRule r3 = new ContextFreeRule(A, a);
		ContextFreeRule r4 = new ContextFreeRule(B, B, b);
		ContextFreeRule r5 = new ContextFreeRule(B);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		rules.add(r5);
		
		//
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, S);
		//LR0Items  items = LR0Items.build(g);
		//SLR1Table table = new SLR1Table(items);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		System.out.println(table.getConflicts());
		
		LRParser<Void> parser = new LRParser<Void>(table);
		parser.parse(new Lexer3("aaaaaabbb"));
		parser.parse(new Lexer3("aaaaaab"));
	}
	
	/**
	 * 
	 *
	 */
	public void testLALR6() throws Exception {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN C = new GSN("C");
		//s.add("S"); s.add("C");
		
		GST c = new GST("c");
		GST d = new GST("d");
		tame.put((int)'c', new Lexer3.Token<Void>(c, null));
		tame.put((int)'d', new Lexer3.Token<Void>(d, null));
		//t.add("c"); t.add("d");
		
		ContextFreeRule r1 = new ContextFreeRule(S, C, C);
		ContextFreeRule r2 = new ContextFreeRule(C, C, c);
		ContextFreeRule r3 = new ContextFreeRule(C, d);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		
		//
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, S);
		//LR0Items  items = LR0Items.build(g);
		//SLR1Table table = new SLR1Table(items);
		LALR1Items items = LALR1Items.newLALR(g);
		LALR1Table table = new LALR1Table(items);
		
		System.out.println(table.getConflicts());
		
		LRParser<Void> parser = new LRParser<Void>(table);
		parser.parse(new Lexer3("dccccdcc"));
	}

}
