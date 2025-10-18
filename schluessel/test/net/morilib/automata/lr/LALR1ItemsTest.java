/* 
 * A Common Packages
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/30
 */
package net.morilib.automata.lr;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import net.morilib.automata.lr.ContextFreeGrammar;
import net.morilib.automata.lr.ContextFreeRule;
import net.morilib.automata.lr.LALR1Items;
import net.morilib.automata.lr.Terminal;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/30
 */
public class LALR1ItemsTest extends TCGrammar {
	
	/**
	 * 
	 *
	 */
	public void testNestedItem() {
		GSN B = new GSN("B");
		GST b = new GST("c");
		GST c = new GST("c");
		
		ContextFreeRule r6 = new ContextFreeRule(B, B, c);
		ContextFreeRule r7 = new ContextFreeRule(B, B, c);
		HashSet<Terminal> la2 = new HashSet<Terminal>();
		la2.add(b);
		HashSet<Terminal> la3 = new HashSet<Terminal>();
		la3.add(c);
		HashSet<Terminal> la4 = new HashSet<Terminal>();
		la4.add(b);
		
		LALR1Items.Item i1 = new LALR1Items.Item(r6, la2);
		LALR1Items.Item i2 = new LALR1Items.Item(r7, la4);
		
		ok(i1.equals(i2));
		eq(i1.hashCode(), i2.hashCode());
	}
	
	/**
	 * 
	 *
	 */
	public void testItemClosure() {
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
		GST d  = new GST("d");
		GST star  = new GST("*");
		GST plus  = new GST("+");
		//t.add("a"); t.add("b"); t.add("c"); t.add("d");
		//t.add("*"); t.add("+");
		
		ContextFreeRule r1 = new ContextFreeRule(S, plus, A);
		ContextFreeRule r2 = new ContextFreeRule(S, A, d);
		ContextFreeRule r3 = new ContextFreeRule(A, a, star);
		ContextFreeRule r4 = new ContextFreeRule(A, B, b);
		ContextFreeRule r5 = new ContextFreeRule(A);
		ContextFreeRule r6 = new ContextFreeRule(B, B, c);
		ContextFreeRule r7 = new ContextFreeRule(B, plus);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		rules.add(r5);
		rules.add(r6);
		rules.add(r7);
		
		//
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, S);
		
		//
		LALR1Items items = new LALR1Items(g);
		HashSet<Terminal> la0 = new HashSet<Terminal>();
		la0.add(b);
		LALR1Items.Item i1 = new LALR1Items.Item(
				r2, la0);
		
		HashSet<Terminal> la1 = new HashSet<Terminal>();
		la1.add(d);
		HashSet<Terminal> la2 = new HashSet<Terminal>();
		la2.add(b);
		HashSet<Terminal> la3 = new HashSet<Terminal>();
		la3.add(c); la3.add(b);
		
		LALR1Items.Item i2 = new LALR1Items.Item(r6, la3);
		
		Set<LALR1Items.Item> res1 = items.computeItemClosure(
				Collections.singleton(i1));
		
		eq(res1.size(), 6);
		ok(res1.contains(i1));
		ok(res1.contains(new LALR1Items.Item(r3, la1)));
		ok(res1.contains(new LALR1Items.Item(r4, la1)));
		ok(res1.contains(new LALR1Items.Item(r5, la1)));
		ok(res1.contains(i2));
		ok(res1.contains(new LALR1Items.Item(r7, la3)));
	}

}
