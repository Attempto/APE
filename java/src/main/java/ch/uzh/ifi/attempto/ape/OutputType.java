// This file is part of the Attempto Parsing Engine (APE).
// Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
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

/**
 * This enumeration lists all the possible outputs of the ACE parser.
 * 
 * @author Tobias Kuhn
 */
public enum OutputType {

	/**
	 * The discourse representation structure (DRS) as a Prolog term.
	 */
	DRS,

	/**
	 * The DRS in XML.
	 */
	DRSXML,

	/**
	 * The DRS in pretty-printed form in plain text.
	 */
	DRSPP,

	/**
	 * The DRS in pretty-printed form in HTML.
	 */
	DRSHTML,

	/**
	 * A paraphrase which is a "best-effort" combination of PARAPHRASE1 and PARAPHRASE2.
	 */
	PARAPHRASE,

	/**
	 * A paraphrase which uses full sentences instead of relative clauses.
	 */
	PARAPHRASE1,

	/**
	 * A paraphrase which uses relative clauses instead of full sentences.
	 */
	PARAPHRASE2,

	/**
	 * Tokens as a Prolog list of lists.
	 */
	TOKENS,

	/**
	 * Sentences as a Prolog list.
	 */
	SENTENCES,

	/**
	 * Simplified syntax trees as a Prolog list.
	 */
	SYNTAX,

	/**
	 * Simplified syntax trees in pretty-printed form.
	 */
	SYNTAXPP,

	/**
	 * Plain syntax trees as a Prolog list (for debugging).
	 */
	SYNTAXD,

	/**
	 * Plain syntax trees in pretty-printed form (for debugging).
	 */
	SYNTAXDPP,

	/**
	 * OWL/SWRL in the Functional-Style Syntax representation as a Prolog term.
	 */
	OWLFSS,

	/**
	 * OWL/SWRL in the Functional-Style Syntax representation in a pretty-printed form.
	 */
	OWLFSSPP,

	/**
	 * OWL/SWRL in the XML representation.
	 */
	OWLXML,

	/**
	 * RuleML representation of the DRS.
	 */
	RULEML,

	/**
	 * Standard first-order logic representation (default form) of the DRS as a Prolog term.
	 */
	FOL,

	/**
	 * Standard first-order logic representation (prenex normal form) of the DRS as a Prolog term.
	 */
	PNF,

	/**
	 * TPTP representation of the DRS.
	 */
	TPTP;

	/**
	 * 
	 * @return flag used to pass the parameter to the parser in the solo-mode (e.g. "fol")
	 */
	public String toSoloFlag() {
		return toString().toLowerCase();
	}

	/**
	 * 
	 * @return flag used to pass the parameter to the parser in the multi-mode (e.g. "cfol")
	 */
	public String toMultiFlag() {
		return "c" + toSoloFlag();
	}
}