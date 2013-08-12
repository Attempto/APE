package ch.uzh.ifi.attempto.ape;

import org.junit.Test;

import static org.junit.Assert.*;

public class APESocketTest {
    private static final ACEParser parser = new APESocket(5000);

    @Test
    public final void testGetSoloOutput() {
        parser.setGuessingEnabled(false);
        String response = null;
        try {
            response = parser.getSoloOutput(Testcase.ACETEXT1, OutputType.PARAPHRASE1);
        } catch (ACEParserException e) {
            fail(e.getMessageContainer().toString());
        }
        assertEquals(Testcase.ACETEXT1_CORE_ACE, response.trim());
    }


    @Test
    public final void testGetMultiOutput() {
        //Lexicon lexicon = new Lexicon();
        //lexicon.addEntry(LexiconEntry.createNounSgEntry("dooog", "DOOOG", Gender.NEUTRAL));
        parser.setGuessingEnabled(true);
        ACEParserResult response = parser.getMultiOutput(Testcase.ACETEXT2, OutputType.PARAPHRASE1);
        assertEquals(Testcase.ACETEXT2_CORE_ACE, response.get(OutputType.PARAPHRASE1).trim());
    }


    @Test
    public final void testGetMultiOutput3() {
        parser.setGuessingEnabled(true);
        ACEParserResult response = parser.getMultiOutput(Testcase.ACETEXT, OutputType.DRSPP);
        assertEquals(Testcase.ACETEXT_DRSPP, response.get(OutputType.DRSPP).trim());
    }
}