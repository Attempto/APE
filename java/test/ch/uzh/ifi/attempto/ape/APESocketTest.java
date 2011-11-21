package ch.uzh.ifi.attempto.ape;

import static org.junit.Assert.*;
import org.junit.Test;

public class APESocketTest {	
	private static final ACEParser parser = new APESocket(5000);

	private static final String ACETEXT1 = "Every dog's friend is an animal.";
	private static final String ACETEXT1_CORE_ACE = "If there is a friend X1 of a dog then the friend X1 is an animal.";
	private static final String ACETEXT2 = "Every dooog's friend is an animal.";
	private static final String ACETEXT2_CORE_ACE = "If there is a friend X1 of a n:dooog then the friend X1 is an animal.";

	@Test
	public final void testGetSoloOutput() {
		parser.setGuessingEnabled(false);
		String response = null;
		try {
			response = parser.getSoloOutput(ACETEXT1, OutputType.PARAPHRASE1);
		} catch (ACEParserException e) {
			fail(e.getMessageContainer().toString());
		}
		assertEquals(ACETEXT1_CORE_ACE, response);
	}


	@Test
	public final void testGetMultiOutput() {
		//Lexicon lexicon = new Lexicon();
		//lexicon.addEntry(LexiconEntry.createNounSgEntry("dooog", "DOOOG", Gender.NEUTRAL));
		parser.setGuessingEnabled(true);
		ACEParserResult response = parser.getMultiOutput(ACETEXT2, OutputType.PARAPHRASE1);
		assertEquals(ACETEXT2_CORE_ACE, response.get(OutputType.PARAPHRASE1).trim());
	}
}