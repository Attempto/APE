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

/**
 * This class represents a single lexicon entry.
 * The static methods can be used to construct a lexical entry on the basis of
 * the word form, logical symbol, and possibly gender information and prepositional particles.
 * All these input components will be represented as Prolog atoms.
 * 
 * @author Tobias Kuhn
 * @author Kaarel Kaljurand
 */
public class LexiconEntry {

	private final String lexiconTerm;

	/**
	 * Creates a new lexicon entry on the basis of a string that is a serialization of a Prolog term.
	 * 
	 * @param lexiconTerm A string that is a serialized Prolog term representing a lexicon entry.
	 */
	private LexiconEntry(String lexiconTerm) {
		this.lexiconTerm = lexiconTerm;
	}
	
	/**
	 * Creates a new lexicon entry on the basis of a string that is a serialization of a Prolog term.
	 * The string is not checked for wellformedness. Whenever possible, one of the other static constructor
	 * methods should be used instead of this method.
	 * 
	 * @param lexiconTerm A string that is a serialized Prolog term representing a lexicon entry.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createEntry(String lexiconTerm) {
		return new LexiconEntry(lexiconTerm);
	}
	
	/**
	 * Creates a new lexicon entry that defines the positive form of an adverb, for example "manually" or "fast".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createAdvEntry(String wordForm, String symbol) {
		return new LexiconEntry("adv(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the comparative form of an adverb, for example "faster".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createAdvCompEntry(String wordForm, String symbol) {
		return new LexiconEntry("adv_comp(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the superlative form of an adverb, for example "fastest".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createAdvSupEntry(String wordForm, String symbol) {
		return new LexiconEntry("adv_sup(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the positive form of an adjective, for example "rich".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createAdjEntry(String wordForm, String symbol) {
		return new LexiconEntry("adj_itr(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the comparative form of an adjective, for example "richer".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createAdjCompEntry(String wordForm, String symbol) {
		return new LexiconEntry("adj_itr_comp(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the superlative form of an adjective, for example "richest".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createAdjSupEntry(String wordForm, String symbol) {
		return new LexiconEntry("adj_itr_sup(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the positive form of a transitive adjective, for example "fond-of".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param preposition The preposition of the transitive adjective, for example "of" in the case of "fond-of".
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createTrAdjEntry(String wordForm, String symbol, String preposition) {
		return new LexiconEntry("adj_tr(" + escape(wordForm) + ", " + escape(symbol) + ", " + escape(preposition) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the comparative form of a transitive adjective, for example "fonder-of".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param preposition The preposition of the transitive adjective, for example "of" in the case of "fonder-of".
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createTrAdjCompEntry(String wordForm, String symbol, String preposition) {
		return new LexiconEntry("adj_tr_comp(" + escape(wordForm) + ", " + escape(symbol) + ", " + escape(preposition) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the superlative form of a transitive adjective, for example "fondest-of".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param preposition The preposition of the transitive adjective, for example "of" in the case of "fondest-of".
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createTrAdjSupEntry(String wordForm, String symbol, String preposition) {
		return new LexiconEntry("adj_tr_sup(" + escape(wordForm) + ", " + escape(symbol) + ", " + escape(preposition) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the singular form of a countable noun, for example "country".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the noun.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createNounSgEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("noun_sg(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the plural form of a countable noun, for example "countries".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the noun.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createNounPlEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("noun_pl(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines a mass noun, for example "money".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the noun.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createNounMassEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("noun_mass(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the singular form of a measurement noun, for example "mile", "km".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createMeasureNounSgEntry(String wordForm, String symbol) {
		return new LexiconEntry("mn_sg(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the plural form of a measurement noun, for example "miles", "km".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createMeasureNounPlEntry(String wordForm, String symbol) {
		return new LexiconEntry("mn_pl(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines a singular proper name, for example "Switzerland".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the proper name.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createPropernameSgEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("pn_sg(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines a plural proper name, for example "United-States".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the proper name.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createPropernamePlEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("pn_pl(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines a singular proper name to be used with the definite article
	 * "the", for example "the Nile".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the proper name.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createPropernameDefSgEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("pndef_sg(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines a plural proper name to be used with the definite article
	 * "the", for example "the United-Nations".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param gender The gender of the proper name.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createPropernameDefPlEntry(String wordForm, String symbol, Gender gender) {
		return new LexiconEntry("pndef_pl(" + escape(wordForm) + ", " + escape(symbol) + ", " + gender + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the third singular form of an intransitive verb, for example "waits".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createItrVerbThirdEntry(String wordForm, String symbol) {
		return new LexiconEntry("iv_finsg(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the bare infinitive form of an intransitive verb, for example "wait".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createItrVerbInfEntry(String wordForm, String symbol) {
		return new LexiconEntry("iv_infpl(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the third singular form of a transitive verb, for example "contains".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createTrVerbThirdEntry(String wordForm, String symbol) {
		return new LexiconEntry("tv_finsg(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the bare infinitive form of a transitive verb, for example "contain".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createTrVerbInfEntry(String wordForm, String symbol) {
		return new LexiconEntry("tv_infpl(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the past participle form of a transitive verb, for example "contained".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createTrVerbPPEntry(String wordForm, String symbol) {
		return new LexiconEntry("tv_pp(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the third singular form of a ditransitive verb, for example "gives".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param preposition The preposition for the indirect object.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createDitrVerbThirdEntry(String wordForm, String symbol, String preposition) {
		return new LexiconEntry("dv_finsg(" + escape(wordForm) + ", " + escape(symbol) + ", " + escape(preposition) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the bare infinitive form of a ditransitive verb, for example "give".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param preposition The preposition for the indirect object.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createDitrVerbInfEntry(String wordForm, String symbol, String preposition) {
		return new LexiconEntry("dv_infpl(" + escape(wordForm) + ", " + escape(symbol) + ", " + escape(preposition) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines the past participle form of a ditransitive verb, for example "given".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @param preposition The preposition for the indirect object.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createDitrVerbPPEntry(String wordForm, String symbol, String preposition) {
		return new LexiconEntry("dv_pp(" + escape(wordForm) + ", " + escape(symbol) + ", " + escape(preposition) + ")");
	}

	/**
	 * Creates a new lexicon entry that defines a preposition, for example "for".
	 * 
	 * @param wordForm The word form how it should appear in the ACE texts.
	 * @param symbol The symbol how it should appear in the logical representations.
	 * @return The new lexicon entry.
	 */
	public static LexiconEntry createPrepEntry(String wordForm, String symbol) {
		return new LexiconEntry("prep(" + escape(wordForm) + ", " + escape(symbol) + ")");
	}

	/**
	 * Returns the plain text serialization for this lexicon entry.
	 * 
	 * @return The plain text serialization for this lexicon entry.
	 */
	@Override
	public String toString() {
		return lexiconTerm;
	}
	
	private static String escape(String str) {
		return PrologUtils.escape(str);
	}

    @Override
	public boolean equals(Object obj) {
		if (obj instanceof LexiconEntry) {
			LexiconEntry other = (LexiconEntry) obj;
			return this.toString().equals(other.toString());
		} else {
			return false;
		}
	}

    @Override
    public int hashCode() {
        return lexiconTerm.hashCode();
    }
}