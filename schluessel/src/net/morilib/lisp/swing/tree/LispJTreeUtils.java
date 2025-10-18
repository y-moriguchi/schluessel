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
package net.morilib.lisp.swing.tree;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/14
 */
public final class LispJTreeUtils {

	//
	private LispJTreeUtils() {}

	//
	static void setRoot(Datum c1a, MyMutableTreeNode o,
			LispMessage mesg) {
		JTree t;

		t = ((LispJTree)c1a).tree;
		((DefaultTreeModel)t.getModel()).setRoot(o);
	}

	//
	static void addNode(Datum c1a, Datum c2a,
			MyMutableTreeNode o, LispMessage mesg) {
		MyMutableTreeNode n;
		JTree t;

		t = ((LispJTree)c1a).tree;
		n = ((LispJTreeNode)c2a).node;
		((DefaultTreeModel)t.getModel()).insertNodeInto(
				o, n, n.getChildCount());
	}

	//
	static void insertNode(Datum c1a, Datum c2a, int i,
			MyMutableTreeNode o, LispMessage mesg) {
		MyMutableTreeNode n;
		JTree t;

		t = ((LispJTree)c1a).tree;
		n = ((LispJTreeNode)c2a).node;
		try {
			((DefaultTreeModel)t.getModel()).insertNodeInto(o, n, i);
		} catch(ArrayIndexOutOfBoundsException e) {
			throw mesg.getError("err.swing.jtree.outofrange",
					Integer.toString(i));
		}
	}

	//
	static Datum pathToS(TreePath p, Datum cdr) {
		ConsListBuilder b = new ConsListBuilder();

		if(p == null)  return LispBoolean.FALSE;
		for(Object o : p.getPath()) {
			b.append(new LispJTreeNode((MyMutableTreeNode)o));
		}
		return b.get(cdr);
	}

	//
	static Datum pathToS(TreePath p) {
		return pathToS(p, Nil.NIL);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static MyMutableTreeNode sToNode(Datum root) {
		Stack2<MyMutableTreeNode> stn =
				new ArrayListStack<MyMutableTreeNode>();
		Stack2<Datum> stk = new ArrayListStack<Datum>();
		MyMutableTreeNode r, s, t;
		ConsIterator itr;
		Datum d;

		r = s = null;
		stk.push(root);
		while(!stk.isEmpty()) {
			if((d = stk.pop()) == null) {
				s = stn.pop();
			} else if(d instanceof Cons) {
				itr = new ConsIterator(d);
				if(itr.hasNext()) {
					t = new MyMutableTreeNode(itr.next());
					if(s == null) {
						r = t;
						stn.push(r);
					} else {
						s.insert(t, 0);
						stn.push(s);
					}
					s = t;
					stk.push(null);
					while(itr.hasNext()) {
						stk.push(itr.next());
					}
				}
			} else {
				t = new MyMutableTreeNode(d);
				s.insert(t, 0);
			}
		}
		return r;
	}

	/**
	 * 
	 * @param root
	 * @return
	 */
	public static Datum nodeToS(DefaultMutableTreeNode root) {
		Stack2<DefaultMutableTreeNode> stk =
				new ArrayListStack<DefaultMutableTreeNode>();
		Stack2<ConsListBuilder> stb =
				new ArrayListStack<ConsListBuilder>();
		Stack2<Integer>  stp = new ArrayListStack<Integer>();
		DefaultMutableTreeNode n = null, m;
		ConsListBuilder b = null, c;
		int i = -1;

		if(root.isLeaf())  return (Datum)root.getUserObject();
		stk.push(root);
		while(true) {
			if(n == null) {
				n = stk.pop();
				b = new ConsListBuilder();
			} else if(i >= n.getChildCount()) {
				if(stk.isEmpty()) {
					break;
				} else {
					c = b;
					n = stk.pop();
					b = stb.pop();
					i = stp.pop() + 1;
					b.append(c.get());
				}
			} else if(i < 0) {
				b.append((Datum)n.getUserObject());
				i = 0;
			} else if((m = (DefaultMutableTreeNode)n.getChildAt(i))
					.isLeaf()) {
				b.append((Datum)m.getUserObject());
				i++;
			} else {
				stk.push(n);
				stb.push(b);
				stp.push(i);
				n = m;
				b = new ConsListBuilder();
				i = -1;
			}
		}
		return b.get();
	}

}
