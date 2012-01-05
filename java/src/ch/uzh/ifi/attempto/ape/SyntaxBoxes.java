// This file is part of the Attempto Parsing Engine (APE).
// Copyright 2008-2012, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later version.
//
// The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.ape;

import java.util.Stack;

/**
 * This class generates the colored syntax boxes in HTML notation on the basis of the syntax tree
 * of an ACE sentence. Noun phrases are represented by blue boxes, verb phrases by yellow boxes,
 * and (subordinated) sentences by gray boxes.
 * 
 * @author Tobias Kuhn
 */
public class SyntaxBoxes {

	private static final String[] sNodes = new String[] {"s", "cond_s", "neg_s", "s_coord", "top_s", "question", "command"};
	private static final String[] vpNodes = new String[] {"vp", "vp_coord", "neg_vp"};
	private static final String[] npNodes = new String[] {"np"};

	private final boolean showS, showVP, showNP;
	private Stack<String> stack = new Stack<String>();

	private SyntaxBoxes(boolean showS, boolean showVP, boolean showNP) {
		this.showS = showS;
		this.showVP = showVP;
		this.showNP = showNP;
	}

	/**
	 * This method takes the syntax tree of a parsing result and generates the HTML representation of the
	 * syntax boxes showing all three types of boxes.
	 * 
	 * @param parserResult The parsing result.
	 * @return The HTML representation of the syntax boxes.
	 */
	public static String getBoxesHtml(ACEParserResult parserResult) {
		return getBoxesHtml(parserResult, true, true, true);
	}

	/**
	 * This method takes the syntax tree of a parsing result and generates the HTML representation of the
	 * syntax boxes. The three types of boxes can individually be switched on or off.
	 * 
	 * @param parserResult The parsing result.
	 * @param showS true if the gray boxes for (subordinated) sentences should be shown.
	 * @param showVP true if the yellow boxes for verb phrases should be shown.
	 * @param showNP true if the blue boxes for noun phrases should be shown.
	 * @return The HTML representation of the syntax boxes.
	 */
	public static String getBoxesHtml(ACEParserResult parserResult, boolean showS, boolean showVP, boolean showNP) {
		String s = new SyntaxBoxes(showS, showVP, showNP).createBoxes(parserResult.get(OutputType.SYNTAX));
		String c = s.replace("'", "").replace("{", "").replace("}", "").replace("_that_", "that");
		String r =
			"<table>\n" +
			"<style type=\"text/css\">\n" +
			"table.np { padding: 0.1em 0.1em 0.1em 0.1em; border: 1px solid #AAF; background-color: #DDF; white-space: nowrap }\n" +
			"table.vp { padding: 0.1em 0.1em 0.1em 0.1em; border: 1px solid #CC4; background-color: #FFA; white-space: nowrap }\n" +
			"table.s  { padding: 0.1em 0.1em 0.1em 0.1em; border: 1px solid #CCC; background-color: #EEE; white-space: nowrap }\n" +
			"</style>\n\n" +
			"<tr>\n" +
			c +
			"</tr>\n" +
			"</table>";
		// This style-element is actually not allowed at this position, but it works in all tested browsers.

		return r;
	}

	private String createBoxes(String p) {
		if (p.length() == 0) {
			return "";
		} else if (p.startsWith(" ")) {
			return createBoxes(p.substring(1));
		} else if (p.startsWith(",")) {
			return createBoxes(p.substring(1));
		} else if (p.startsWith("[[")) {
			stack.push("");
			return createBoxes(p.substring(1));
		} else if (p.startsWith("[]")) {
			return createBoxes(p.substring(2));
		} else if (p.startsWith("[")) {
			if (showS) {
				for (String s : sNodes) {
					if (p.startsWith("[" + s + ",")) {
						stack.push("</tr></table></td>");
						return "<td><table class=\"s\"><tr>" + createBoxes(p.substring(s.length()+2));
					}
				}
			}
			if (showVP) {
				for (String s : vpNodes) {
					if (p.startsWith("[" + s + ",")) {
						stack.push("</tr></table></td>");
						return "<td><table class=\"vp\"><tr>" + createBoxes(p.substring(s.length()+2));
					}
				}
			}
			if (showNP) {
				for (String s : npNodes) {
					if (p.startsWith("[" + s + ",")) {
						stack.push("</tr></table></td>");
						return "<td><table class=\"np\"><tr>" + createBoxes(p.substring(s.length()+2));
					}
				}
			}
			if (p.indexOf(",") > 0) {
				stack.push("");
				return createBoxes(p.substring(p.indexOf(",")+1));
			}
		} else if (p.startsWith("]")) {
			String t = stack.pop();
			return t + createBoxes(p.substring(1));
		} else if (p.indexOf("]") > 0) {
			int ci = p.indexOf(",");
			int bi = p.indexOf("]");
			int i = bi;
			if (ci > 0 && ci < bi) i = ci;
			String a = p.substring(0, i);
			String b = p.substring(i);
			return "<td>" + a + "</td> " + createBoxes(b);
		}
		return " ERROR(" + p + ")ERROR ";
	}

}
