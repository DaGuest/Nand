package Assembler;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;

public class Assembler {
    File file;
    FileWriter outFile;
    Parser parser;
    Code coder;
    SymbolTable symbolTable;
    int romAddress = 0;
    int ramAddress = 16;

    /**
     * The Assembler is responsible to convert .asm file to hack binary code.
     * The constructor creates the needed parser, binary code translator and symbol table.
     * It opens the given .asm file and the output file where the binary code is written to.
     * @param filename The filename of the .asm file to be converted.
     */
    public Assembler(String filename) {
        this.file = new File(filename);
        parser = new Parser(file);
        coder = new Code();
        symbolTable = new SymbolTable();
        InitializeSymbolTable();
        try {
            String stemFilename = Paths.get(filename).toString();
            stemFilename = stemFilename.substring(0, stemFilename.lastIndexOf("."));
            outFile = new FileWriter(stemFilename + ".hack");
        } catch (IOException e) {
            System.out.println("failed to open output file.");
        }
    }
    
    /**
     * Starts the assembly process. 
     * 
     * The run goes over the .asm file two times:
     * The first run adds any encountered new labels into the symbol table with their corresponding rom address.
     * The second run converts all encountered commands into binary strings and writes these to the outfile.
     */
    public void Run() {
        // First run to get all the labels in the symboltable
        while (parser.hasMoreCommands()) {
            parser.advance();
            if (parser.commandType() == Parser.CommandType.L_COMMAND) {
                if (!symbolTable.contains(parser.symbol())) {
                    symbolTable.addEntry(parser.symbol(), romAddress--);
                }
            }
            romAddress++;
        }
        parser.Reset();
        // Second run to add all A symbols to the symboltable
        while (parser.hasMoreCommands()) {
            String binaryString = "";
            parser.advance();
            switch (parser.commandType()) {
                case A_COMMAND -> {
                    String symbol = parser.symbol();
                    binaryString = ConvertAddressingCommand(symbol);
                }
                case C_COMMAND -> {
                    binaryString = ConvertComputeCommand(parser.comp(), parser.dest(), parser.jump());
                }
                case L_COMMAND -> {
                    // Do nothing
                }
            }
            try {
                outFile.write(binaryString);
            } catch (IOException e) {
                System.out.println("failed to write to output file.");
            }
        }
        try {
            outFile.close();
        } catch (IOException e) {
        }
    }
    
    /** 
     * Converts the current symbol into an A-Command bitcode string.
     * @param symbol 
     * @return String The binary representation of the assembly command.
     */
    private String ConvertAddressingCommand(String symbol) {
        int symbolValue;
        // Check if the value is an integer or a label
        try {
            symbolValue = Integer.parseInt(symbol);
        } catch (NumberFormatException e) {
            // Check if the label already encountered or new.
            if (symbolTable.contains(symbol)) {
                symbolValue = symbolTable.getAddress(symbol);
            } else {
                symbolTable.addEntry(symbol, ramAddress);
                symbolValue = ramAddress;
                ramAddress++;
            }
        }
        return coder.aDecode(symbolValue) + System.getProperty("line.separator");
    }
    
    
    /** 
     * Combines all the given arguments into an C-Command bitcode string.
     * @param comp compute part of the C-command.
     * @param dest destination part of the C-command.
     * @param jump jump part of the C-command.
     * @return String The binary representation of the assembly command.
     */
    private String ConvertComputeCommand(String comp, String dest, String jump) {
        return coder.cDecode(comp, dest, jump) + System.getProperty("line.separator");
    }
    
    /**
     * Loads all the preset symbols in the symbol table.
     */
    private void InitializeSymbolTable() {
        symbolTable.addEntry("SP", 0x0000);
        symbolTable.addEntry("LCL", 0x0001);
        symbolTable.addEntry("ARG", 0x0002);
        symbolTable.addEntry("THIS", 0x0003);
        symbolTable.addEntry("THAT", 0x0004);
        for (int i = 0; i < 16; i++) {
            symbolTable.addEntry("R" + i, 0x0000 + i);
        }
        symbolTable.addEntry("SCREEN", 0x4000);
        symbolTable.addEntry("KBD", 0x6000);
    }


    public static void main(String[] args) {
        if (args.length > 0) {
            Assembler assembler = new Assembler(args[0].strip());
            assembler.Run();
        }
    }
}
