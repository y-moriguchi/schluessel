/* 
 * A Common Packages
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/29
 */
package net.morilib.automata.lr;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.morilib.automata.lr.ContextFreeGrammar;
import net.morilib.automata.lr.ContextFreeRule;
import net.morilib.automata.lr.GrammarSymbol;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/29
 */
public class ContextFreeGrammarTest extends TCGrammar {
	
	/**
	 * 
	 *
	 */
	public void testFindRules() {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN A = new GSN("A");
		GSN B = new GSN("B");
		//s.add("S"); s.add("A"); s.add("B");
		
		GST a  = new GST("a");
		GST b  = new GST("b");
		//t.add("a"); t.add("b");
		
		ContextFreeRule r1 = new ContextFreeRule(S, A, B);
		ContextFreeRule r2 = new ContextFreeRule(S, A);
		ContextFreeRule r3 = new ContextFreeRule(A, a);
		ContextFreeRule r4 = new ContextFreeRule(B, b);
		ContextFreeRule r5 = new ContextFreeRule(B);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		rules.add(r5);
		
		//
		ContextFreeGrammar test =
			new ContextFreeGrammar(rules, S);
		
		Set<ContextFreeRule> res = test.findRules(S);
		ok(res.contains(r1));
		ok(res.contains(r2));
		eq(res.size(), 2);
		
		//try {
		//	test.findRules("a");
		//	fail();
		//} catch(IllegalArgumentException e) {}
	}
	
	/**
	 * 
	 *
	 */
	public void testNestedFirst() {
		ContextFreeGrammar.First fs = new ContextFreeGrammar.First();
		ContextFreeGrammar.First gs = new ContextFreeGrammar.First();
		ContextFreeGrammar.First hs = new ContextFreeGrammar.First();
		ContextFreeGrammar.First is = new ContextFreeGrammar.First();
		
		GST a  = new GST("a");
		GST b  = new GST("b");
		fs.add(a); fs.add(b);
		fs.setNullable(true);
		gs.add(a); gs.add(b);
		gs.setNullable(false);
		hs.add(a); hs.add(b);
		hs.setNullable(true);
		is.add(a); is.add(b);
		is.setNullable(true);
		
		ok(fs.equals(hs));
		ok(hs.equals(is));  ok(fs.equals(is));
		ok(hs.equals(fs));
		ng(fs.equals(null));
		ng(fs.equals(gs));
		
		eq(fs.hashCode(), hs.hashCode());
		eq(fs.hashCode(), is.hashCode());
		ne(fs.hashCode(), gs.hashCode());
	}
	
	/**
	 * 
	 *
	 */
	public void testFirst() {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN A = new GSN("A");
		GSN B = new GSN("B");
		//s.add("S"); s.add("A"); s.add("B");
		
		GST a  = new GST("a");
		GST b  = new GST("b");
		GST c  = new GST("c");
		GST star  = new GST("*");
		//t.add("a"); t.add("b"); t.add("c"); t.add("*");
		
		ContextFreeRule r1 = new ContextFreeRule(S, B, c);
		ContextFreeRule r2 = new ContextFreeRule(S, A, star);
		ContextFreeRule r3 = new ContextFreeRule(A, a, star);
		ContextFreeRule r4 = new ContextFreeRule(B, B, b);
		ContextFreeRule r5 = new ContextFreeRule(B);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		rules.add(r5);
		
		//
		ContextFreeGrammar test =
			new ContextFreeGrammar(rules, S);
		
		test.computeFirst();
		ok(test.first(S).contains(a));
		ok(test.first(S).contains(b));
		ok(test.first(S).contains(c));
		ng(test.first(S).isNullable());
		eq(test.first(S).size(), 3);
		ok(test.first(A).contains(a));
		ng(test.first(A).isNullable());
		eq(test.first(A).size(), 1);
		ok(test.first(B).contains(b));
		ok(test.first(B).isNullable());
		eq(test.first(B).size(), 1);
		
		//
		List<GrammarSymbol> in2 =
			Arrays.asList(new GrammarSymbol[] { B, A });
		ContextFreeGrammar.First f = test.firstAll(in2);
		ok(f.contains(a));
		ok(f.contains(b));
		ng(f.isNullable());
		eq(f.size(), 2);
	}
	
	/**
	 * 
	 *
	 */
	public void testFollow() {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		HashSet<ContextFreeRule> rules = new HashSet<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN A = new GSN("A");
		GSN B = new GSN("B");
		//s.add("S"); s.add("A"); s.add("B");
		
		GST a  = new GST("a");
		GST b  = new GST("b");
		//t.add("a"); t.add("b"); t.add("c"); t.add("*");
		
		ContextFreeRule r1 = new ContextFreeRule(S, A, B);
		ContextFreeRule r2 = new ContextFreeRule(A, A, a);
		ContextFreeRule r3 = new ContextFreeRule(B, B, b);
		ContextFreeRule r4 = new ContextFreeRule(B);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		//rules.add(r5);
		
		//
		ContextFreeGrammar test =
			new ContextFreeGrammar(rules, S);
		
		test.computeFirst();
		test.computeFollow();
		ok(test.follow(S).contains(ContextFreeGrammar.ENDMARKER));
		eq(test.follow(S).size(), 1);
		//System.out.println(test.follow("A"));
		ok(test.follow(A).contains(a));
		ok(test.follow(A).contains(b));
		ok(test.follow(A).contains(ContextFreeGrammar.ENDMARKER));
		eq(test.follow(A).size(), 3);
		ok(test.follow(B).contains(b));
		ok(test.follow(B).contains(ContextFreeGrammar.ENDMARKER));
		eq(test.follow(B).size(), 2);
	}

}
