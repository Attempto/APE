package ch.uzh.ifi.attempto.ape;

import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class APELocalTest extends Testcase {

    private static ACEParser parser;

    @BeforeClass public static void setUp() {
        parser = getApeLocal();
    }

    private static APELocal getApeLocal() {
        APELocal.init("../ape.exe", true);
        return APELocal.getInstance();
    }

    @Test
    public void testDRS() throws ACEParserException {
        APELocal apeLocal = getApeLocal();
        apeLocal.setGuessingEnabled(true);
        String drs = apeLocal.getSoloOutput(ACETEXT, OutputType.DRS);
        assertEquals("DRS mismatch", ACETEXT_DRS, drs);
    }

    @Test
    public final void testGetSoloOutput() throws ACEParserException {
        parser.setGuessingEnabled(false);
        String response = null;
            response = parser.getSoloOutput(Testcase.ACETEXT1, OutputType.PARAPHRASE1);

        assertEquals(Testcase.ACETEXT1_CORE_ACE, response.trim());
    }
    @Test
    public final void testGetMultiOutput() {
        ACEParserResult result = parser.getMultiOutput(Testcase.ACETEXT, OutputType.DRS, OutputType.TPTP);
        assertEquals(Testcase.ACETEXT_DRS, result.get(OutputType.DRS));
        assertEquals(Testcase.ACETEXT_TPTP, result.get(OutputType.TPTP));
    }
    @Test
    public final void testGetMultiOutput3() {
        parser.setGuessingEnabled(true);
        ACEParserResult response = parser.getMultiOutput(Testcase.ACETEXT, OutputType.DRSPP);
        assertEquals(Testcase.ACETEXT_DRSPP, response.get(OutputType.DRSPP).trim());
    }

}