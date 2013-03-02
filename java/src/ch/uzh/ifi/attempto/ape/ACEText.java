package ch.uzh.ifi.attempto.ape;

/**
 * WARNING: This class is experimental and is incomplete (i.e. covers only a subset of the lexicon entries).
 * <p/>
 * This class reflects the fact that an ACE text is really a combination of
 * a string and a lexicon. The lexicon is used to interpret the content words
 * in the string: assign them word classes, features (e.g. gender) and map them
 * to logical symbols.
 * <p/>
 * We experiment with a representation where the string and the lexicon are
 * packaged into a single string:
 * <p/>
 * John|pn_sg|John_PN|masc buys|tv_finsg|buy_V2 everything that isn't a present|noun_sg|present_N|neutr .
 *
 * @author Kaarel Kaljurand
 */
public class ACEText {

	public static final String WORDCLASS_PN_SG = "pn_sg";
	public static final String WORDCLASS_PNDEF_SG = "pndef_sg";
	public static final String WORDCLASS_PNDEF_PL = "pndef_pl";
	public static final String WORDCLASS_NOUN_SG = "noun_sg";
	public static final String WORDCLASS_NOUN_PL = "noun_pl";
	public static final String WORDCLASS_TV_FINSG = "tv_finsg";
	public static final String WORDCLASS_TV_INFPL = "tv_infpl";
	public static final String WORDCLASS_TV_PP = "tv_pp";
	public static final String WORDCLASS_ADJ_TR = "adj_tr";

	private final StringBuilder mText = new StringBuilder();
	private final Lexicon mLexicon = new Lexicon();

	public ACEText(String text) {
		tokenize(text);
	}

	public String getText() {
		return mText.toString();
	}

	public Lexicon getLexicon() {
		return mLexicon;
	}


	/**
	 * Tokenizes that given string assuming that tokens are separated by sequences of spaces.
	 * Each token is split by a vertical bar (|). If this splitting results in at least 3 units,
	 * then they are assumed to be: word form, word class identifier, logical symbol. With nouns
	 * a gender argument can also follow, but this is optional and by default the UNDEF gender is assumed.
	 * If the word class does not match a known ACE word class then we assume that the token
	 * is a regular ACE token.
	 * <p/>
	 * TODO: currently only AceWiki word classes are covered
	 * TODO: correctly handle ACE quoted strings (which contain the vertical bar character)
	 */
	private void tokenize(String str) {
		for (String tok : str.split("\\s+")) {
			String[] splits = tok.split("\\|");

			if (splits.length <= 2) {
				mText.append(' ');
				mText.append(tok);
				continue;
			}

			String wordForm = splits[0];
			String wordClass = splits[1];
			String logicalSymbol = splits[2];

			if (WORDCLASS_PN_SG.equals(wordClass)) {
				if (splits.length > 3) {
					add(wordForm, LexiconEntry.createPropernameSgEntry(wordForm, logicalSymbol, Gender.create(splits[3])));
				} else {
					add(wordForm, LexiconEntry.createPropernameSgEntry(wordForm, logicalSymbol, Gender.UNDEF));
				}
			} else if (WORDCLASS_PNDEF_SG.equals(wordClass)) {
				if (splits.length > 3) {
					add(wordForm, LexiconEntry.createPropernameDefSgEntry(wordForm, logicalSymbol, Gender.create(splits[3])));
				} else {
					add(wordForm, LexiconEntry.createPropernameDefSgEntry(wordForm, logicalSymbol, Gender.UNDEF));
				}
			} else if (WORDCLASS_PNDEF_PL.equals(wordClass)) {
				if (splits.length > 3) {
					add(wordForm, LexiconEntry.createPropernameDefPlEntry(wordForm, logicalSymbol, Gender.create(splits[3])));
				} else {
					add(wordForm, LexiconEntry.createPropernameDefPlEntry(wordForm, logicalSymbol, Gender.UNDEF));
				}
			} else if (WORDCLASS_NOUN_SG.equals(wordClass)) {
				if (splits.length > 3) {
					add(wordForm, LexiconEntry.createNounSgEntry(wordForm, logicalSymbol, Gender.create(splits[3])));
				} else {
					add(wordForm, LexiconEntry.createNounSgEntry(wordForm, logicalSymbol, Gender.UNDEF));
				}
			} else if (WORDCLASS_NOUN_PL.equals(wordClass)) {
				if (splits.length > 3) {
					add(wordForm, LexiconEntry.createNounPlEntry(wordForm, logicalSymbol, Gender.create(splits[3])));
				} else {
					add(wordForm, LexiconEntry.createNounPlEntry(wordForm, logicalSymbol, Gender.UNDEF));
				}
			} else if (WORDCLASS_TV_FINSG.equals(wordClass)) {
				add(wordForm, LexiconEntry.createTrVerbThirdEntry(wordForm, logicalSymbol));
			} else if (WORDCLASS_TV_INFPL.equals(wordClass)) {
				add(wordForm, LexiconEntry.createTrVerbInfEntry(wordForm, logicalSymbol));
			} else if (WORDCLASS_TV_PP.equals(wordClass)) {
				add(wordForm, LexiconEntry.createTrVerbPPEntry(wordForm, logicalSymbol));
			} else if (WORDCLASS_ADJ_TR.equals(wordClass)) {
				// Note that the preposition is used as an independent token only in comparative constructs, e.g.
				// "Mary is more fond-of Bill than of John.".
				// For the simple ADJ_TR we could also ignore it.
				String prep = addPrep(splits);
				add(wordForm, LexiconEntry.createTrAdjEntry(wordForm, logicalSymbol, prep));
			} else {
				mText.append(' ');
				mText.append(tok);
			}
		}
	}


	private void add(String tok, LexiconEntry le) {
		mLexicon.addEntry(le);
		mText.append(' ');
		mText.append("`");
		mText.append(tok);
		mText.append("`");
	}


	private String addPrep(String[] splits) {
		if (splits.length > 3) {
			String prep = splits[3];
			if (prep.length() > 0) {
				mLexicon.addEntry(LexiconEntry.createPrepEntry(prep, prep));
			}
			return prep;
		}
		return "";
	}
}