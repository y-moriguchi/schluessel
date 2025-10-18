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
package net.morilib.lisp.painter;

import java.awt.Image;
import java.util.List;
import java.util.Stack;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public class DisplayPainter extends Subr {

	//
	private MutablePoint2D paintImage(
			SchlushFrame f,
			ImagePainter p,
			double x,
			double y,
			double rx,
			double ry,
			LispMessage mesg) {
		Image img = p.getImageFactory().getImage();
		int w = f.getImageWidth(img);
		int h = f.getImageHeight(img);
		MutablePoint2D ps;
		
		if(f.getCoordinate() != null) {
			ps = f.getCoordinate().invertSize(
					f.getWidth(), f.getHeight(), w, h);
		} else {
			ps = new MutablePoint2D(w, h);
		}
		
		try {
			f.addImage(new SchlushFrame.ImageInfo(
					img, x, y,
					ps.getX() * rx,
					ps.getY() * ry));
		} catch (InterruptedException e) {
			throw mesg.getError("err.interrupted");
		}
		
		//return new Point2D(w, h);
		return ps;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		
		if(l.size() < 4 || l.size() > 6 || l.size() == 5) {
			throw mesg.getError("err.argument", symbolName);
		} else if(!(l.get(0) instanceof SchlushFrame)) {
			throw mesg.getError("err.require.frame", l.get(0));
		} else if(!(l.get(1) instanceof Painter)) {
			throw mesg.getError("err.require.painter", l.get(1));
		} else if(!(l.get(2) instanceof LispNumber)) {
			throw mesg.getError("err.require.number", l.get(2));
		} else if(!((LispNumber)l.get(2)).isReal()) {
			throw mesg.getError("err.require.real", l.get(2));
		} else if(!(l.get(3) instanceof LispNumber)) {
			throw mesg.getError("err.require.number", l.get(3));
		} else if(!((LispNumber)l.get(3)).isReal()) {
			throw mesg.getError("err.require.real", l.get(3));
		} else if(l.size() < 5) {
			// do nothing
		} else if(!(l.get(4) instanceof LispNumber)) {
			throw mesg.getError("err.require.number", l.get(4));
		} else if(!((LispNumber)l.get(4)).isReal()) {
			throw mesg.getError("err.require.real", l.get(4));
		} else if(!(l.get(5) instanceof LispNumber)) {
			throw mesg.getError("err.require.number", l.get(5));
		} else if(!((LispNumber)l.get(5)).isReal()) {
			throw mesg.getError("err.require.real", l.get(5));
		}
		
		//
		SchlushFrame f;
		Painter p;
		double x, y, rx, ry;
		
		f  = (SchlushFrame)l.get(0);
		p  = (Painter)l.get(1);
		x  = ((LispNumber)l.get(2)).getRealDouble();
		y  = ((LispNumber)l.get(3)).getRealDouble();
		rx = (l.size() < 5) ?
				1.0 : ((LispNumber)l.get(4)).getRealDouble();
		ry = (l.size() < 5) ?
				1.0 : ((LispNumber)l.get(5)).getRealDouble();
		
		Stack<Painter> stk = new Stack<Painter>();
		Stack<Integer> stw = new Stack<Integer>();
		Stack<Integer> sth = new Stack<Integer>();
		Stack<Double>  stx = new Stack<Double>();
		Stack<Double>  sty = new Stack<Double>();
		Stack<Painter> stz = new Stack<Painter>();
		stk.push(p);  stz.push(null);
		stw.push(1);  sth.push(1);
		stx.push(x);  sty.push(y);
		while(!stk.isEmpty()) {
			Painter p2 = stk.pop();
			int w2 = stw.pop();
			int h2 = sth.pop();
			double x2 = stx.pop();
			double y2 = sty.pop();
			Painter bef = stz.pop();
			
			if(p2 instanceof ImagePainter) {
				MutablePoint2D p0 = paintImage(
						f, (ImagePainter)p2, x2, y2,
						rx / w2, ry / h2, mesg);
				if(stk.isEmpty()) {
					// do nothing
				} else if(bef instanceof BesidePainter) {
					stx.pop();
					stx.push(x2 + p0.getX() / w2);
				} else if(bef instanceof BelowPainter) {
					sty.pop();
					sty.push(y2 + p0.getY() / h2);
				}
			} else if(p2 instanceof BesidePainter) {
				stk.push(((BesidePainter)p2).getRight());
				stw.push(w2 * 2);
				sth.push(h2);
				stx.push(x2);  sty.push(y2);
				stz.push(p2);
				stk.push(((BesidePainter)p2).getLeft());
				stw.push(w2 * 2);
				sth.push(h2);
				stx.push(x2);  sty.push(y2);
				stz.push(p2);
			} else if(p2 instanceof BelowPainter) {
				stk.push(((BelowPainter)p2).getBelow());
				stw.push(w2);
				sth.push(h2 * 2);
				stx.push(x2);  sty.push(y2);
				stz.push(p2);
				stk.push(((BelowPainter)p2).getAbove());
				stw.push(w2);
				sth.push(h2 * 2);
				stx.push(x2);  sty.push(y2);
				stz.push(p2);
			}
		}
		return Undef.UNDEF;
	}

}
