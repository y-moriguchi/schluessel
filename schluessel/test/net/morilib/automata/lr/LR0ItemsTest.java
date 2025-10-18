/* 
 * A Common Packages
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/30
 */
package net.morilib.automata.lr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.morilib.automata.lr.ContextFreeGrammar;
import net.morilib.automata.lr.ContextFreeRule;
import net.morilib.automata.lr.GrammarSymbol;
import net.morilib.automata.lr.LR0Items;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/30
 */
public class LR0ItemsTest extends TCGrammar {
	
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
		GST star  = new GST("*");
		//t.add("a"); t.add("b"); t.add("c"); t.add("*");
		
		ContextFreeRule r1 = new ContextFreeRule(S, A, c);
		ContextFreeRule r2 = new ContextFreeRule(S, A, star);
		ContextFreeRule r3 = new ContextFreeRule(A, a, star);
		ContextFreeRule r4 = new ContextFreeRule(A, B, b);
		ContextFreeRule r5 = new ContextFreeRule(A);
		ContextFreeRule r6 = new ContextFreeRule(B, B, c);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		rules.add(r5);
		rules.add(r6);
		
		//
		ContextFreeGrammar g =
			ContextFreeGrammar.newInstance(rules, S);
		
		//
		LR0Items items = new LR0Items(g);
		LR0Items.Item i1 = new LR0Items.Item(r1);
		Set<LR0Items.Item> res1 = items.computeItemClosure(
				Collections.singleton(i1));
		
		eq(res1.size(), 5);
		ok(res1.contains(i1));
		ok(res1.contains(new LR0Items.Item(r3)));
		ok(res1.contains(new LR0Items.Item(r4)));
		ok(res1.contains(new LR0Items.Item(r5)));
		ok(res1.contains(new LR0Items.Item(r6)));
	}
	
	/**
	 * 
	 *
	 */
	public void testExtractGoToState() {
		//HashSet s = new HashSet();
		//HashSet t = new HashSet();
		ArrayList<ContextFreeRule> rules =
			new ArrayList<ContextFreeRule>();
		
		GSN S = new GSN("S");
		GSN A = new GSN("A");
		GSN B = new GSN("B");
		//s.add("S"); s.add("A"); s.add("B");
		
		GST a  = new GST("a");
		GST b  = new GST("b");
		GST c  = new GST("c");
		GST star  = new GST("*");
		//t.add("a"); t.add("b"); t.add("c"); t.add("*");
		
		ContextFreeRule r1 = new ContextFreeRule(S, A, c);
		ContextFreeRule r2 = new ContextFreeRule(S, A, star);
		ContextFreeRule r3 = new ContextFreeRule(A, a, star);
		ContextFreeRule r4 = new ContextFreeRule(A, B, b);
		ContextFreeRule r5 = new ContextFreeRule(A);
		ContextFreeRule r6 = new ContextFreeRule(B, B, c);
		rules.add(r1);
		rules.add(r2);
		rules.add(r3);
		rules.add(r4);
		rules.add(r5);
		rules.add(r6);
		
		//
		HashSet<LR0Items.Item> in1 = new HashSet<LR0Items.Item>();
		in1.add(new LR0Items.Item(r3));
		in1.add(new LR0Items.Item(r4));
		in1.add(new LR0Items.Item(r5));
		in1.add(new LR0Items.Item(r6));
		
		HashSet<LR0Items.Item> res1 = new HashSet<LR0Items.Item>();
		res1.add(new LR0Items.Item(r3, 1));
		HashSet<LR0Items.Item> res2 = new HashSet<LR0Items.Item>();
		res2.add(new LR0Items.Item(r4, 1));
		res2.add(new LR0Items.Item(r6, 1));
		
		Map<GrammarSymbol, Set<LR0Items.Item>> mp =
			LR0Items.extractGoToState(in1);
		eq(mp.size(), 2);
		eq(mp.get(a), res1);
		eq(mp.get(B), res2);
	}

}
