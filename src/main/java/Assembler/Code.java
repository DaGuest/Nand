package Assembler;
import java.util.HashMap;

public class Code {
    HashMap<String, String> compTable;
    HashMap<String, String> jumpTable;

    public Code() {
        MakeCompTable();
    }

    /**
     * Constructs a mapping for computation and jump binary representations.
     */
    private void MakeCompTable() {
        this.compTable = new HashMap<>();
        // a = 0
        compTable.put("0", "0101010");
        compTable.put("1", "0111111");
        compTable.put("-1", "0111010");
        compTable.put("D", "0001100");
        compTable.put("A", "0110000");
        compTable.put("!D", "0001101");
        compTable.put("!A", "0110001");
        compTable.put("-D", "0001111");
        compTable.put("-A", "0110011");
        compTable.put("D+1", "0011111");
        compTable.put("A+1", "0110111");
        compTable.put("D-1", "0001110");
        compTable.put("A-1", "0110010");
        compTable.put("D+A", "0000010");
        compTable.put("D-A", "0010011");
        compTable.put("A-D", "0000111");
        compTable.put("D&A", "0000000");
        compTable.put("D|A", "0010101");
        // a = 1
        compTable.put("M", "1110000");
        compTable.put("!M", "1110001");
        compTable.put("-M", "1110011");
        compTable.put("M+1", "1110111");
        compTable.put("M-1", "1110010");
        compTable.put("D+M", "1000010");
        compTable.put("D-M", "1010011");
        compTable.put("M-D", "1000111");
        compTable.put("D&M", "1000000");
        compTable.put("D|M", "1010101");

        // Jump table
        this.jumpTable = new HashMap<>();
        jumpTable.put("", "000");
        jumpTable.put("JGT", "001");
        jumpTable.put("JEQ", "010");
        jumpTable.put("JGE", "011");
        jumpTable.put("JLT", "100");
        jumpTable.put("JNE", "101");
        jumpTable.put("JLE", "110");
        jumpTable.put("JMP", "111");
    }

    /** 
     * Converts the destination part of the C-command.
     * @param mnemonic Destination mnemonic. A combination of A, D and M.
     * @return String A 3-bit binary representation of the destination.
     */
    public String dest(String mnemonic) {
        String binaryCode = "";
        binaryCode += mnemonic.contains("A") ? "1" : "0";
        binaryCode += mnemonic.contains("D") ? "1" : "0";
        binaryCode += mnemonic.contains("M") ? "1" : "0";
        return binaryCode;
    }

    /**
     * Converts the computation part of the C-command.
     * @param mnemonic Computation mnemonic.
     * @return A 7-bit binary representation of the computation.
     */
    public String comp(String mnemonic) {
        return compTable.get(mnemonic);
    }

    /**
     * Converts the jump part of the C-command.
     * @param mnemonic Jump mnemonic
     * @return A 3-bit binary representation of the jump.
     */
    public String jump(String mnemonic) {
        return jumpTable.get(mnemonic);
    }

    /**
     * Takes the three different parts of a C-command and converts them into one binary representation.
     * @param comp computation part
     * @param dest destination part
     * @param jump jump part
     * @return 16-bit binary representation 
     */
    public String cDecode(String comp, String dest, String jump) {
        return "111" + comp(comp) + dest(dest) + jump(jump);
    }

    /**
     * Converts the given number into a binary representation.
     * @param symbol integer value 
     * @return 16-bit binary representation of the given number.
     */
    public String aDecode(int symbol) {
        return Integer.toBinaryString(0x10000 | symbol).substring(1);
    }
}
